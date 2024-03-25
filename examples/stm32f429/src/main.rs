#![no_std]
#![no_main]

mod node;

use defmt::*;
use defmt_rtt as _;
use panic_probe as _;

use stm32f4xx_hal::{
    self as hal,
    prelude::*,
    gpio::{ErasedPin, GpioExt, Output},
    rcc::RccExt,
};
use systick_monotonic::Systick;

use smoltcp::time::Instant;
use smoltcp::wire::{EthernetAddress, IpCidr, IpAddress, IpEndpoint};
use smoltcp::iface::{SocketSet, SocketHandle, Config, Interface, SocketStorage};
use smoltcp::socket::tcp::{Socket as TcpSocket, SocketBuffer as TcpSocketBuffer, State as TcpState};
use smoltcp::socket::udp::{Socket as UdpSocket, PacketBuffer as UdpPacketBuffer};
use smoltcp::socket::dhcpv4::{Socket as DhcpSocket, Event as DhcpEvent};
use smoltcp::storage::PacketMetadata;

use stm32_eth::{dma::{EthernetDMA, RxRingEntry, TxRingEntry}, Parts, PartsIn, EthPins};

const TIME_GRANULARITY: u32 = 1000;  // 1000 Hz granularity
const PORT: u16 = 10767;
const MAX_CLIENTS: usize = 5;

const NTP_OPTION: u8 = 42;
const NTP_PORT: u16 = 123;
// Simple packet requesting current timestamp
const NTP_REQUEST: &[u8] = b"\xE3\x00\x06\xEC\x00\x00\x00\x00\x00\x00\x00\x00\
                             \x31\x4E\x31\x34\x00\x00\x00\x00\x00\x00\x00\x00\
                             \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
                             \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00";

#[rtic::app(
    device = crate::hal::pac,
    dispatchers = [USART1, UART4],
)]
mod app {
    use super::*;

    #[monotonic(binds = SysTick, default = true)]
    type Time = Systick<TIME_GRANULARITY>;

    #[shared]
    struct Shared {
        node: node::SecNode<MAX_CLIENTS>,
        net: Net<TIME_GRANULARITY>,
        handles: [SocketHandle; MAX_CLIENTS],
        use_dhcp: bool,
    }

    #[local]
    struct Local {
        leds: Leds,
    }

    #[init(local = [
        rx_ring: [RxRingEntry; 2*MAX_CLIENTS] = [RxRingEntry::RX_INIT; 2*MAX_CLIENTS],
        tx_ring: [TxRingEntry; 2*MAX_CLIENTS] = [TxRingEntry::INIT; 2*MAX_CLIENTS],
        rx_buffers: [[u8; 1500*4]; MAX_CLIENTS] = [[0; 1500*4]; MAX_CLIENTS],
        tx_buffers: [[u8; 1500*4]; MAX_CLIENTS] = [[0; 1500*4]; MAX_CLIENTS],
        sockets_storage: [SocketStorage<'static>; MAX_CLIENTS] = [SocketStorage::EMPTY; MAX_CLIENTS],
    ])]
    fn init(cx: init::Context) -> (Shared, Local, init::Monotonics) {
        let p = cx.device;

        let rcc = p.RCC.constrain();
        let clocks = rcc.cfgr.sysclk(180.MHz()).hclk(180.MHz()).freeze();

        let gpioa = p.GPIOA.split();
        let gpiob = p.GPIOB.split();
        let gpioc = p.GPIOC.split();
        let gpiog = p.GPIOG.split();
        let pins = EthPins {
            ref_clk: gpioa.pa1,
            crs: gpioa.pa7,
            tx_en: gpiog.pg11,
            tx_d0: gpiog.pg13,
            tx_d1: gpiob.pb13,
            rx_d0: gpioc.pc4,
            rx_d1: gpioc.pc5,
        };
        let mdio = gpioa.pa2.into_alternate();
        let mdc = gpioc.pc1.into_alternate();

        // DHCP if user button not pressed
        let dhcp_btn = gpioc.pc13.into_pull_down_input();
        let use_dhcp = dhcp_btn.is_low();

        let led_green = gpiob.pb0.into_push_pull_output();
        let led_blue = gpiob.pb7.into_push_pull_output();
        let led_red = gpiob.pb14.into_push_pull_output();
        let mut leds = Leds::new(led_red.into(), led_green.into(), led_blue.into());
        // red LED: indicate "booting"
        leds.set(true, false, false);

        // set up ring buffers for network handling tokens
        let Parts { mut dma, .. } = stm32_eth::new_with_mii(
            PartsIn { dma: p.ETHERNET_DMA, mac: p.ETHERNET_MAC, mmc: p.ETHERNET_MMC, ptp: p.ETHERNET_PTP },
            cx.local.rx_ring,
            cx.local.tx_ring,
            clocks,
            pins,
            mdio,
            mdc,
        ).expect("eth setup");

        // determine MAC address from board's serial number
        let serial = read_serno();
        let ethernet_addr = EthernetAddress([
            0x46, 0x52, 0x4d,  // F R M
            (serial >> 16) as u8, (serial >> 8) as u8, serial as u8
        ]);
        let config = Config::new(ethernet_addr.into());
        let mut iface = Interface::new(config, &mut &mut dma, Instant::ZERO);
        // select the default Mesytec IP if static configuration
        if !use_dhcp {
            iface.update_ip_addrs(|addrs| {
                addrs.push(IpCidr::new(IpAddress::v4(192, 168, 0, 2), 24)).unwrap();
            });
        }

        // set up buffers for packet content and metadata

        // create the UDP socket
        let mut sockets = SocketSet::new(&mut cx.local.sockets_storage[..]);
        let mut handles = [SocketHandle::default(); MAX_CLIENTS];
        for (i, (txb, rxb)) in cx.local.tx_buffers.iter_mut().zip(cx.local.rx_buffers.iter_mut()).enumerate() {
            let tcp_socket = TcpSocket::new(TcpSocketBuffer::new(&mut rxb[..]),
                                            TcpSocketBuffer::new(&mut txb[..]));
            handles[i] = sockets.add(tcp_socket);
        }

        let node = node::create();

        // use systick monotonic clock for now
        let mono = Systick::new(cx.core.SYST, clocks.hclk().raw());

        info!("------------------------------------------------------------------------");
        if use_dhcp {
            dhcp::spawn().unwrap();
        } else {
            start::spawn().unwrap();
        }
        let net = Net { sockets, iface, dma, ntp_time: None };
        (Shared { node, net, handles, use_dhcp },
         Local { leds },
         init::Monotonics(mono))
    }

    #[task(shared = [net])]
    fn dhcp(mut cx: dhcp::Context) {
        // give the remote partner time to realize the link is up,
        // so we don't run into the 10sec DHCP discover interval
        while monotonics::now().ticks() < 2000 { }

        info!("Starting DHCP");
        let mut buf = [0; 1500];

        cx.shared.net.lock(|net| {
            // use dedicated socket storages here, we don't need them later
            let mut storage = [SocketStorage::EMPTY; 1];
            let mut sockets = SocketSet::new(&mut storage[..]);
            let mut ntp_addr = None;
            let ip_addr;

            let mut dhcp_socket = DhcpSocket::new();
            // request NTP address and provide a buffer for us to receive it
            dhcp_socket.set_parameter_request_list(&[1, 3, 6, NTP_OPTION]);
            dhcp_socket.set_receive_packet_buffer(&mut buf[..]);
            let dhcp_handle = sockets.add(dhcp_socket);

            loop {
                let time = Instant::from_millis(monotonics::now().ticks() as i64);
                net.iface.poll(time, &mut &mut net.dma, &mut sockets);

                let event = sockets.get_mut::<DhcpSocket>(dhcp_handle).poll();
                if let Some(DhcpEvent::Configured(config)) = event {
                    ip_addr = config.address;
                    net.iface.update_ip_addrs(|addrs| addrs.push(ip_addr.into()).unwrap());

                    if let Some(router) = config.router {
                        net.iface.routes_mut().add_default_ipv4_route(router).unwrap();
                    } else {
                        net.iface.routes_mut().remove_default_ipv4_route();
                    }

                    for opt in config.packet.expect("has a buffer").options() {
                        if opt.kind == NTP_OPTION && opt.data.len() == 4 {
                            ntp_addr = Some(IpAddress::v4(opt.data[0],
                                                          opt.data[1],
                                                          opt.data[2],
                                                          opt.data[3]));
                            break;
                        }
                    }
                    break;
                }
            }

            if let Some(addr) = ntp_addr {
                let start_time = monotonics::now().ticks();
                let mut storage = [SocketStorage::EMPTY; 1];
                let mut sockets = SocketSet::new(&mut storage[..]);

                let mut rx_buf = [0; 1500];
                let mut tx_buf = [0; 1500];
                let mut rx_meta_buf = [PacketMetadata::EMPTY; 1];
                let mut tx_meta_buf = [PacketMetadata::EMPTY; 1];
                let ntp_handle = sockets.add(UdpSocket::new(
                    UdpPacketBuffer::new(&mut rx_meta_buf[..], &mut rx_buf[..]),
                    UdpPacketBuffer::new(&mut tx_meta_buf[..], &mut tx_buf[..])
                ));

                sockets.get_mut::<UdpSocket>(ntp_handle).bind((ip_addr.address(), NTP_PORT))
                                                        .expect("bind udp");

                let endpoint = IpEndpoint::from((addr, NTP_PORT));
                info!("NTP: sending request to {}", endpoint);
                match sockets.get_mut::<UdpSocket>(ntp_handle).send_slice(NTP_REQUEST, endpoint) {
                    Ok(_) => loop {
                        let time = monotonics::now().ticks();
                        if time - start_time > 5000 {
                            // timeout NTP request
                            break;
                        }

                        net.iface.poll(Instant::from_millis(time as i64),
                                   &mut &mut net.dma, &mut sockets);

                        if let Ok((_, _)) = sockets.get_mut::<UdpSocket>(ntp_handle).recv_slice(&mut buf) {
                            let now = time as f64 / TIME_GRANULARITY as f64;
                            let stamp = u32::from_be_bytes([buf[40], buf[41], buf[42], buf[43]]);
                            // conversion to Unix time, differs by seventy years
                            let stamp = stamp - 2208988800;
                            info!("NTP: got timestamp {}", stamp);
                            net.ntp_time = Some(stamp as f64 - now);
                        }
                    }
                    Err(e) => warn!("could not send NTP request: {}", e)
                }
            }
        });
        start::spawn().unwrap();
    }

    #[task(shared = [net, &handles, &use_dhcp])]
    fn start(cx: start::Context) {
        let start::SharedResources { mut net, handles, use_dhcp } = cx.shared;

        net.lock(|net| {
            let ip_addr = net.iface.ipv4_addr().expect("iface has an ip");
            info!("IP setup done ({}), binding to {}:{}",
                  if *use_dhcp { "dhcp" } else { "static" }, ip_addr, PORT);

            for &handle in handles {
                let socket = net.sockets.get_mut::<TcpSocket>(handle);
                socket.set_nagle_enabled(false);
                socket.listen(PORT).expect("can listen");
            }

            net.dma.enable_interrupt();
        });
        poll::spawn_after(500.millis().into()).unwrap();
    }

    #[task(binds = ETH,
           local = [leds, connected: [bool; MAX_CLIENTS] = [false; MAX_CLIENTS]],
           shared = [node, net, &handles])]
    fn eth(cx: eth::Context) {
        let eth::LocalResources { leds, connected } = cx.local;
        let eth::SharedResources { node, net, handles } = cx.shared;
        let mut buf = [0; 1024];

        let _reason = stm32_eth::eth_interrupt_handler();
        // debug!("Got an ethernet interrupt! Reason: {}", _reason);

        (node, net).lock(|node, net| {
            let now = monotonics::now();
            net.poll(now);
            let time = net.get_time(now);

            for (i, &handle) in handles.iter().enumerate() {
                let socket = net.sockets.get_mut::<TcpSocket>(handle);

                if socket.is_active() {
                    if !connected[i] {
                        connected[i] = true;
                        node.client_connected(i);
                        leds.set(false, true, true);
                    }
                } else if connected[i] {
                    connected[i] = false;
                    node.client_finished(i);
                    leds.set(false, true, false);

                    if !socket.is_listening() && !socket.is_open() || socket.state() == TcpState::CloseWait {
                        socket.abort();
                        socket.listen(PORT).ok();
                        warn!("Disconnected... Reopening listening socket.");
                    }
                }

                if let Ok(recv_bytes) = socket.recv_slice(&mut buf) {
                    if recv_bytes > 0 {
                        info!("Got {} bytes on socket {}", recv_bytes, i);
                        let result = node.process(
                            time, &mut buf[..recv_bytes],
                            i as usecop::ClientId,
                            |sn, callback: &dyn Fn(&mut dyn usecop::io::Write)| {
                                let socket = net.sockets.get_mut::<TcpSocket>(handles[sn]);
                                callback(&mut Writer(socket));
                            }
                        );
                        if result.is_err() {
                            warn!("Error processing data");
                        }
                    }
                }
            }

            net.poll(monotonics::now());
        });
    }

    #[task(shared = [node, net, &handles])]
    fn poll(cx: poll::Context) {
        let poll::SharedResources { node, net, handles } = cx.shared;

        (node, net).lock(|node, net| {
            let now = monotonics::now();
            let time = net.get_time(now);
            node.poll(time, |sn, callback: &dyn Fn(&mut dyn usecop::io::Write)| {
                let socket = net.sockets.get_mut::<TcpSocket>(handles[sn]);
                callback(&mut Writer(socket));
            });
            net.poll(now);
        });
        poll::spawn_after(500.millis().into()).unwrap();
    }
}

fn read_serno() -> u32 {
    unsafe {
        *(0x1FFF_7A10 as *const u32) ^
        *(0x1FFF_7A14 as *const u32) ^
        *(0x1FFF_7A18 as *const u32)
    }
}

pub struct Net<const TIME_GRANULARITY: u32> {
    sockets: SocketSet<'static>,
    iface: Interface,
    dma: EthernetDMA<'static, 'static>,
    ntp_time: Option<f64>,
}

impl<const TIME_GRANULARITY: u32> Net<TIME_GRANULARITY> {
    fn poll<const NOM: u32, const DENOM: u32>(&mut self, now: fugit::Instant<u64, NOM, DENOM>) {
        let time = Instant::from_millis(now.ticks() as i64);
        self.iface.poll(time, &mut &mut self.dma, &mut self.sockets);
    }

    fn get_time<const NOM: u32, const DENOM: u32>(
        &self, now: fugit::Instant<u64, NOM, DENOM>) -> usecop::Timestamp
    {
        let ticks = now.ticks() as f64 / TIME_GRANULARITY as f64;
        if let Some(epoch) = self.ntp_time {
            usecop::Timestamp::Abs(epoch + ticks)
        } else {
            usecop::Timestamp::Rel(ticks)
        }
    }
}

struct Leds {
    r: ErasedPin<Output>,
    g: ErasedPin<Output>,
    b: ErasedPin<Output>,
}

impl Leds {
    fn new(r: ErasedPin<Output>, g: ErasedPin<Output>, b: ErasedPin<Output>) -> Self {
        Self { r, g, b }
    }

    fn set(&mut self, r: bool, g: bool, b: bool) {
        if r { self.r.set_high(); } else { self.r.set_low(); }
        if g { self.g.set_high(); } else { self.g.set_low(); }
        if b { self.b.set_high(); } else { self.b.set_low(); }
    }
}

struct Writer<'w, 's>(&'w mut TcpSocket<'s>);

impl<'w, 's> usecop::io::Write for Writer<'w, 's> {
    fn write(&mut self, buf: &[u8]) -> Result<usize, usecop::io::Error> {
        self.0.send_slice(buf)
              .map_err(|_| usecop::io::Error::new(usecop::io::ErrorKind::Other, ""))
    }

    fn flush(&mut self) -> Result<(), usecop::io::Error> {
        Ok(())
    }
}
