use esp_idf_svc::wifi::EspWifi;
use usecop::{ModuleInternals, Result};

pub type SecNode<const N: usize> = usecop::node::SecNode<MyModules, N>;

pub fn create<const N: usize>(mut wifi: EspWifi<'static>) -> SecNode<N> {
    wifi.start_scan(&Default::default(), false).unwrap();
    SecNode::new("esp32", "microSECoP demo over ESP32 and LAN82xx", MyModules {
        temp: WifiNets { wifi, max_wifis: 5,
                         internals: ModuleInternals::new("accessible wifi networks", 5.0) },
    })
}

#[derive(usecop_derive::Modules)]
pub struct MyModules {
    temp: WifiNets,
}

#[derive(Clone, Copy, usecop_derive::DataInfo)]
enum WifiStatus {
    Idle = 100,
}

#[derive(usecop_derive::Module)]
#[secop(interface = "Readable")]
#[secop(param(name = "value", doc = "list of wifi networks",
              readonly = true,
              datainfo(array(members(str(maxchars=30)), minlen=1, maxlen=10))))]
#[secop(param(name = "status", doc = "status of scan",
              readonly = true,
              datainfo(tuple(member(rust="WifiStatus"),
                             member(str(maxchars=18))))))]
#[secop(param(name = "max_wifis", doc = "maximum number of wifi networks to return",
              readonly = false, generate_accessors = true,
              datainfo(int(min=1, max=10))))]
#[secop(command(name = "buzz", doc = "buzz it!",
                argument(str(maxchars=10)),
                result(str(maxchars=10))))]
struct WifiNets {
    internals: ModuleInternals,
    wifi: EspWifi<'static>,
    max_wifis: i64,
}

impl WifiNets {
    fn read_value(&mut self) -> Result<Vec<heapless::String<32>>> {
        if let Ok(ap_infos) = self.wifi.get_scan_result() {
            self.wifi.start_scan(&Default::default(), false).unwrap();
            let mut result: Vec<_> = ap_infos.iter().map(|ap| ap.ssid.clone()).collect();
            result.sort();
            result.truncate(self.max_wifis as usize);
            Ok(result)
        } else {
            Ok(vec![])
        }
    }

    fn read_status(&mut self) -> Result<(WifiStatus, &str)> {
        Ok((WifiStatus::Idle, "all good, trust me"))
    }

    fn do_buzz<'a>(&mut self, arg: &'a mut str) -> Result<&'a str> {
        log::info!("got buzzed: <{}>", arg);
        Ok(arg)
    }
}
