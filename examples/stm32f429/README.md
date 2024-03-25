# µSECoP demo with STM32F429 (NUCLEO-F429ZI board)

## How to set up the hardware

The setup code is written for the
[NUCLEO-F429ZI](https://www.st.com/en/evaluation-tools/nucleo-f429zi.html)
board, which has an Ethernet connector onboard.  If you want to use it unchanged
with a standalone STM32, you need to connect the Ethernet pins, buttons and LEDs
like described in ST user manual UM1974.

## How to build and run

* Install Rustup: see https://rustup.rs/ (stable toolchain)

* Install the ARM toolchain and flashing tool:

```
$ rustup target add thumbv7em-none-eabihf
$ apt install libudev-dev  # example for Debian based systems
$ cargo install probe-rs --features cli
```

* Build and run:

```
$ cargo run --release
```

* Connect:

By default, the firmware acquires an IPv4 address via DHCP.  This is
recommended, since it can then also query the NTP server given via
DHCP option to get an absolute timestamp for SECoP qualifiers.

If you press the user button (blue) while booting, it will give itself the
static address 192.168.0.2/24.
