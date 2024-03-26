use stm32f4xx_hal::{
    adc::{Adc, Temperature, config::SampleTime},
    pac::ADC1,
};
use usecop::{ModuleInternals, Result};

pub type SecNode<const N: usize> = usecop::node::SecNode<MyModules, N>;

pub fn create<const N: usize>(adc: Adc<ADC1>) -> SecNode<N> {
    SecNode::new("stm32", "microSECoP demo on STM32F4", MyModules {
        temp: Temp { adc, conversion: 2.5,
                     internals: ModuleInternals::new("chip temperature", 5.0) },
    })
}

#[derive(usecop_derive::Modules)]
pub struct MyModules {
    temp: Temp,
}

#[derive(Clone, Copy, usecop_derive::DataInfo)]
enum TempStatus {
    Idle = 100,
}

#[derive(usecop_derive::Module)]
#[secop(interface = "Readable")]
#[secop(param(name = "value", doc = "main value of the temperature",
              readonly = true,
              datainfo(double(unit="degC"))))]
#[secop(param(name = "status", doc = "status of readout",
              readonly = true,
              datainfo(tuple(member(rust="TempStatus"),
                             member(str(maxchars=18))))))]
#[secop(param(name = "conversion", doc = "conversion factor",
              readonly = false, generate_accessors = true,
              datainfo(double())))]
#[secop(command(name = "buzz", doc = "buzz it!",
                argument(str(maxchars=10)),
                result(str(maxchars=10))))]
struct Temp {
    internals: ModuleInternals,
    adc: Adc<ADC1>,
    conversion: f64,
}

impl Temp {
    fn read_value(&mut self) -> Result<f64> {
        let sample = self.adc.convert(&Temperature, SampleTime::Cycles_480);
        defmt::info!("sample: {}", sample);
        let v_sense = self.adc.sample_to_millivolts(sample);
        defmt::info!("voltage: {}", v_sense);
        Ok(25.0 + (v_sense - 760) as f64 / self.conversion)
    }

    fn read_status(&mut self) -> Result<(TempStatus, &str)> {
        Ok((TempStatus::Idle, "all good, trust me"))
    }

    fn do_buzz<'a>(&mut self, arg: &'a mut str) -> Result<&'a str> {
        defmt::info!("got buzzed: <{}>", arg);
        Ok(arg)
    }
}
