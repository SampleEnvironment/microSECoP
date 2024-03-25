# µSECoP demo with ESP32 (WT32-ETH01 board)

## How to set up the hardware

The code is written for the
[WT32-ETH01](https://www.seeedstudio.com/Ethernet-module-based-on-ESP32-series-WT32-ETH01-p-4736.html)
board, which has an Ethernet connector onboard, using the LAN8720 chip.  If you
want to use it unchanged with a standalone ESP32, you need to connect the
Ethernet pins yourself.

To flash, connect a USB-TTL adapter to the board:

- TXD - pin RX0
- RXD - pin TX0
- 5V  - pin VCC
- GND - pin GND

To flash the board, you need to power it on while pulling the `IO0` pin low.  To
reboot with the newly flashed firmware, power cycle again with the pin
disconnected.

## How to build and run

* Install the [Rust Espressif compiler toolchain and the Espressif LLVM Clang
  toolchain](https://github.com/esp-rs/rust-build)

* Make sure to call `source ~/export-esp.sh` every time in a new shell before using it

* Install additional tools:

```
$ cargo install ldproxy
$ cargo install espflash
```

* Build, flash and run:

```
$ cargo run --release
```

* Connect:

The firmware acquires an IPv4 address via DHCP.  It then also queries an NTP
server to get an absolute timestamp for SECoP qualifiers.
