mod node;

use std::sync::{Arc, Mutex};
use std::io::Read;
use std::net::{TcpListener, TcpStream};
use std::thread;
use std::time::{Duration, SystemTime};

use anyhow::Result;
use log::*;

use esp_idf_svc::eth;
use esp_idf_svc::hal::{delay, gpio, prelude::*};
use esp_idf_svc::sntp::EspSntp;
use esp_idf_svc::eventloop::EspSystemEventLoop;

const MAX_CLIENTS: usize = 5;

fn main() -> Result<()> {
    esp_idf_svc::sys::link_patches();

    // Bind the log crate to the ESP Logging facilities
    esp_idf_svc::log::EspLogger::initialize_default();

    let peripherals = Peripherals::take().unwrap();
    let pins = peripherals.pins;

    let sysloop = EspSystemEventLoop::take()?;

    let mut clock_en_pin = gpio::PinDriver::output(pins.gpio16)?;
    clock_en_pin.set_high()?;
    delay::Ets::delay_ms(100 as u32);

    let eth = eth::EspEth::wrap(
        eth::EthDriver::new_rmii(
            peripherals.mac,
            pins.gpio25,
            pins.gpio26,
            pins.gpio27,
            pins.gpio23,
            pins.gpio22,
            pins.gpio21,
            pins.gpio19,
            pins.gpio18,
            eth::RmiiClockConfig::<gpio::Gpio0, gpio::Gpio16, gpio::Gpio17>::Input(
                pins.gpio0,
            ),
            None::<gpio::Gpio5>,
            eth::RmiiEthChipset::LAN87XX,
            None,
            sysloop.clone(),
        )?,
    )?;
    info!("Eth created");
    let mut eth = esp_idf_svc::eth::BlockingEth::wrap(eth, sysloop.clone())?;
    info!("Starting eth...");
    eth.start()?;
    info!("Waiting for DHCP lease...");
    eth.wait_netif_up()?;
    let ip_info = eth.eth().netif().get_ip_info()?;
    info!("DHCP info: {:?}", ip_info);

    // this will asynchronously query the time from the NTP server
    let _ntp = EspSntp::new(&Default::default());

    const REPEAT: Option<TcpStream> = None;
    let globals = Arc::new(Mutex::new((node::create(), [REPEAT; MAX_CLIENTS])));
    type Globals = Arc<Mutex<(node::SecNode<MAX_CLIENTS>,
                              [Option<TcpStream>; MAX_CLIENTS])>>;

    fn get_time() -> usecop::Timestamp {
        usecop::Timestamp::Abs(
            SystemTime::now()
                .duration_since(SystemTime::UNIX_EPOCH)
                .unwrap()
                .as_secs_f64())
    }

    fn handle_client(id: usecop::ClientId, globals: Globals) {
        let mut buf = [0; 256];
        loop {
            thread::sleep(Duration::from_millis(10));
            let (node, clients) = &mut *globals.lock().unwrap();
            match clients[id].as_mut().unwrap().read(&mut buf) {
                Ok(0) => {
                    // connection was closed
                    clients[id] = None;
                    node.client_finished(id);
                    break;
                }
                Ok(n) => {
                    info!("Got {} bytes on client {}", n, id);
                    if node.process(
                        get_time(), &mut buf[0..n], id, |write_id, callback| {
                            if let Some(other) = clients[write_id].as_mut() {
                                callback(&mut Writer(other))
                            }
                        }
                    ).is_err() {
                        warn!("Could not process client's data for {}", id);
                    }
                }
                Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                    continue;
                }
                Err(err) => {
                    clients[id] = None;
                    node.client_finished(id);
                    error!("Error receiving, closing client {}: {}", id, err);
                    break;
                }
            }
        }
    }

    fn poller(globals: Globals) {
        loop {
            thread::sleep(Duration::from_millis(500));
            let (node, clients) = &mut *globals.lock().unwrap();
            node.poll(get_time(), |write_id, callback| {
                if let Some(other) = clients[write_id].as_mut() {
                    callback(&mut Writer(other))
                }
            });
        }
    }

    info!("Starting SECoP poller and listener");
    let globals_clone = globals.clone();
    thread::spawn(move || poller(globals_clone));

    let listener = TcpListener::bind((ip_info.ip, 10767)).unwrap();
    'outer:
    for stream in listener.incoming() {
        match stream {
            Ok(stream) => {
                info!("Accepted client from {:?}", stream.peer_addr());
                let (node, clients) = &mut *globals.lock().unwrap();
                for id in 0..MAX_CLIENTS {
                    if clients[id].is_none() {
                        stream.set_nonblocking(true).unwrap();
                        clients[id] = Some(stream);
                        let globals_clone = globals.clone();
                        node.client_connected(id);
                        thread::spawn(move || handle_client(id, globals_clone));
                        info!("Assigned client to slot {}", id);
                        continue 'outer;
                    }
                }
                // no free slot for this connection
                stream.shutdown(std::net::Shutdown::Both).unwrap();
            }
            Err(e) => error!("Listener error: {}", e),
        }
    }

    unreachable!()
}

struct Writer<'w>(&'w mut TcpStream);

impl usecop::io::Write for Writer<'_> {
    fn write(&mut self, buf: &[u8]) -> Result<usize, usecop::io::Error> {
        self.0.write(buf).map_err(Into::into)
    }

    fn flush(&mut self) -> Result<(), usecop::io::Error> {
        Ok(())
    }
}
