# µSECoP demo with STM32F429 (NUCLEO-F429ZI board)

## How to set up the hardware


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
