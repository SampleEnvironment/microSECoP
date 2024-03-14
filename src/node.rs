// -----------------------------------------------------------------------------
// This file is part of µSECoP.
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free Software
// Foundation; either version 2 of the License, or (at your option) any later
// version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc.,
// 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
// Module authors:
//   Georg Brandl <g.brandl@fz-juelich.de>
//   Enrico Faulhaber <enrico.faulhaber@frm2.tum.de>
//
// -----------------------------------------------------------------------------
//
//! SECoP node abstraction.

use core2::io::Write;
use crate::proto::{InMsg, OutMsg, Error, Specifier, Timestamp};
use crate::{wire, http, ClientId, ModuleDescription, ToJson};


#[derive(Clone, Copy, Default, PartialEq, Eq)]
enum ClientState {
    #[default]
    Disconnected,
    Initial,
    WebSocket(bool),
    Plain(bool),
}

impl ClientState {
    fn activate(&mut self, yesno: bool) {
        match self {
            Self::WebSocket(active) | Self::Plain(active) => *active = yesno,
            _ => ()
        }
    }
}


pub struct SecNode<T: Modules, const MAX_CLIENTS: ClientId = 8> {
    state: [ClientState; MAX_CLIENTS],
    equipment_id: &'static str,
    description: &'static str,
    modules: T,
}

impl<T: Modules, const MAX_CLIENTS: ClientId> SecNode<T, MAX_CLIENTS> {
    pub fn new(equipment_id: &'static str, description: &'static str, modules: T) -> Self {
        Self {
            state: [ClientState::Disconnected; MAX_CLIENTS],
            equipment_id,
            description,
            modules,
        }
    }

    pub fn new_client(&mut self) -> Option<ClientId> {
        self.state.iter().position(|&x| x == ClientState::Disconnected)
                         .map(|i| { self.state[i] = ClientState::Initial; i })
    }

    pub fn client_connected(&mut self, client: ClientId) {
        self.state[client] = ClientState::Initial;
    }

    pub fn client_finished(&mut self, client: ClientId) {
        self.state[client] = ClientState::Disconnected;
    }

    pub fn process(&mut self, time: Timestamp, input: &mut [u8], id: ClientId,
                   callback: impl FnMut(ClientId, &dyn Fn(&mut dyn Write)))
        -> Result<usize, ()>
    {

        match self.state[id] {
            ClientState::Initial if input.starts_with(b"GET") => match http::handle(input, id, callback) {
                Ok(Some(used)) => {
                    self.state[id] = ClientState::WebSocket(false);
                    Ok(used)
                }
                Ok(None) => Ok(0),  // need more input
                Err(_) => Err(()),
            }
            ClientState::Initial => {
                self.state[id] = ClientState::Plain(false);
                // recursive call for Plain case
                self.process(time, input, id, callback)
            }
            ClientState::WebSocket(_) => match http::decode_ws(input) {
                Err(_) => {
                    self.state[id] = ClientState::Disconnected;
                    Err(())
                }
                Ok(Some((msg, used))) => {
                    self.process_msg(time, msg, id, callback);
                    Ok(used)
                }
                Ok(None) => Ok(0)
            }
            ClientState::Plain(_) => match find_line(input) {
                Some((msg, used)) => {
                    self.process_msg(time, msg, id, callback);
                    Ok(used)
                }
                None => Ok(0)
            }
            ClientState::Disconnected => Err(()),  // should not happen
        }
    }

    fn process_msg(&mut self, time: Timestamp, msg: &mut [u8], id: ClientId,
                   mut callback: impl FnMut(ClientId, &dyn Fn(&mut dyn Write))) {
        let states = self.state;
        let mut sender = Sender::new(id, &mut callback, &states);
        let result = match InMsg::parse(msg) {
            Ok(input_msg) => match input_msg {
                InMsg::Change { spec, value } => match self.modules.by_name(spec.module) {
                    Some(m) => return m.change(time, spec, value, sender),
                    None => Error::no_module().spec_msg(wire::CHANGE, spec),
                },
                InMsg::Do { spec, arg } => match self.modules.by_name(spec.module) {
                    Some(m) => return m.do_(time, spec, arg, sender),
                    None => Error::no_module().spec_msg(wire::DO, spec),
                },
                InMsg::Read { spec } => match self.modules.by_name(spec.module) {
                    Some(m) => return m.read(time, spec, sender),
                    None => Error::no_module().spec_msg(wire::READ, spec),
                },
                InMsg::Activate { module } => {
                    self.state[id].activate(true);
                    sender.distribute_single = true;
                    self.modules.for_each(|name, module| {
                        module.poll(time, name, true, &mut sender);
                    });
                    OutMsg::Active { module }
                },
                InMsg::Deactivate { module } => {
                    self.state[id].activate(false);
                    OutMsg::Inactive { module }
                },
                InMsg::Ping { token } => OutMsg::Pong { token, value: (), time },
                InMsg::Idn => OutMsg::IdnReply,
                InMsg::Describe =>
                    return self.modules.describe(self.equipment_id, self.description, sender),
                InMsg::Help => OutMsg::Helping { message: "See https://sampleenvironment.org/secop" },
            },
            Err(errmsg) => errmsg,
        };
        sender.send(result)
    }

    pub fn poll(&mut self, time: Timestamp, mut callback: impl FnMut(ClientId, &dyn Fn(&mut dyn Write))) {
        let states = self.state;
        let mut sender = Sender::new(MAX_CLIENTS + 1, &mut callback, &states);
        self.modules.for_each(|name, module| {
            module.poll(time, name, false, &mut sender);
        });
    }
}

pub trait Modules {
    fn count(&self) -> usize;
    fn by_name(&mut self, name: &str) -> Option<&mut dyn Module>;
    fn for_each(&mut self, f: impl FnMut(&str, &mut dyn Module));
    fn describe(&self, eq_id: &str, desc: &str, sender: Sender);
}

pub trait Module {
    /// Return the descriptive data for this module (a JSON object).
    fn describe(&self) -> ModuleDescription;
    /// Read a parameter and possibly emit an update message.
    fn read(&mut self, time: Timestamp, spec: Specifier, reply: Sender);
    /// Change a parameter and possibly emit an update message.
    fn change(&mut self, time: Timestamp, spec: Specifier, value: &mut str, reply: Sender);
    /// Execute a command.
    fn do_(&mut self, time: Timestamp, spec: Specifier, arg: &mut str, reply: Sender);
    /// Do a polling cycle.
    fn poll(&mut self, time: Timestamp, name: &str, all: bool, reply: &mut Sender);
}

/// The internal state of a module needed by the framework.
pub struct ModuleInternals {
    pub description: &'static str,
    pub pollinterval: f64,
    pub lastpoll: f64,
}

impl ModuleInternals {
    pub fn new(description: &'static str, pollinterval: f64) -> Self {
        Self {
            description,
            pollinterval,
            lastpoll: 0.0,
        }
    }
}

fn find_line(input: &mut [u8]) -> Option<(&mut [u8], usize)> {
    input.iter().position(|&x| x == b'\n').map(|i| (&mut input[..i], i + 1))
}

/// A callback that we can call with the desired client ID, and it then calls
/// the inner callback with a writer to let us write data.
type SenderCallback<'a> = &'a mut dyn FnMut(ClientId, &dyn Fn(&mut dyn Write));

pub struct Sender<'a> {
    client: ClientId,
    writer: SenderCallback<'a>,
    states: &'a [ClientState],
    pub distribute_single: bool,
}

impl Sender<'_> {
    fn new<'a>(client: ClientId, writer: SenderCallback<'a>,
               states: &'a [ClientState]) -> Sender<'a> {
        Sender { client, writer, states, distribute_single: false }
    }

    pub fn send<V: ToJson>(&mut self, msg: OutMsg<'_, V>) {
        if matches!(self.states[self.client], ClientState::WebSocket(_)) {
            self.send_ws(self.client, &msg);
        } else {
            self.send_plain(self.client, &msg);
        }
    }

    pub fn distribute<V: ToJson>(&mut self, msg: OutMsg<'_, V>) {
        if self.distribute_single {
            return self.send(msg);
        }
        for (sn, state) in self.states.iter().enumerate() {
            match state {
                ClientState::Plain(true) => self.send_plain(sn, &msg),
                ClientState::WebSocket(true) => self.send_ws(sn, &msg),
                _ => continue,
            }
        }
    }

    fn send_plain<V: ToJson>(&mut self, sn: ClientId, msg: &OutMsg<'_, V>) {
        (self.writer)(sn, &|writer: &mut dyn Write| {
            let _ = write!(writer, "{}", msg);
        });
    }

    fn send_ws<V: ToJson>(&mut self, sn: ClientId, msg: &OutMsg<'_, V>) {
        (self.writer)(sn, &|writer: &mut dyn Write| {
            let mut writer = crate::http::WsWriterWrapper::new(writer);
            let _ = write!(writer, "{}", msg);
            let _ = writer.send_frame(true);
        });
    }
}
