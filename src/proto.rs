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
//! SECoP protocol definitions.

use core::fmt::{Display, Formatter};
use crate::{wire, Json, ToJson, NodeDescription};

/// Identifies a single client.
pub type ClientId = usize;

#[derive(Debug, Clone, Copy)]
pub enum Timestamp {
    Abs(f64),
    Rel(f64),
}

impl Timestamp {
    pub fn any(&self) -> f64 {
        match *self {
            Timestamp::Abs(t) => t,
            Timestamp::Rel(t) => t,
        }
    }
}

impl Display for Timestamp {
    fn fmt(&self, f: &mut Formatter) -> core::fmt::Result {
        match self {
            Timestamp::Abs(t) => write!(f, "{{\"t\":{}}}", Json(t)),
            Timestamp::Rel(_) => f.write_str("{}"),
        }
    }
}


pub type Result<'a, T> = core::result::Result<T, Error<'a>>;

#[derive(Debug)]
pub enum ErrorKind {
    // Internal
    Config,
    Programming,
    Parsing,
    // API defined
    Protocol,
    NoSuchModule,
    NoSuchParameter,
    NoSuchCommand,
    CommandFailed,
    CommandRunning,
    ReadOnly,
    BadValue,
    CommunicationFailed,
    Timeout,       // ATM also C.F.
    HardwareError, // ATM also C.F.
    IsBusy,
    IsError,
    Disabled,
}

impl ErrorKind {
    fn wire(&self) -> &'static str {
        use ErrorKind::*;
        match self {
            Config | Programming | Parsing => "InternalError",
            Protocol => "ProtocolError",
            NoSuchModule => "NoSuchModule",
            NoSuchParameter => "NoSuchParameter",
            NoSuchCommand => "NoSuchCommand",
            CommandFailed => "CommandFailed",
            CommandRunning => "CommandRunning",
            ReadOnly => "ReadOnly",
            BadValue => "BadValue",
            CommunicationFailed => "CommunicationFailed",
            Timeout => "CommunicationFailed",
            HardwareError => "CommunicationFailed",
            IsBusy => "IsBusy",
            IsError => "IsError",
            Disabled => "Disabled",
        }
    }
}

#[derive(Debug)]
pub struct Error<'a> {
    kind: ErrorKind,
    message: &'a str,
}

impl<'a> Error<'a> {
    pub fn new(kind: ErrorKind, message: &'static str) -> Self {
        Self { kind, message }
    }

    pub fn spec_msg(self, request: &'a str, specifier: Specifier<'a>) -> OutMsg<'a, ()> {
        OutMsg::Error {
            request,
            specifier,
            class: self.kind.wire(),
            message: self.message,
        }
    }

    pub fn msg(self) -> OutMsg<'a, ()> {
        OutMsg::Error {
            request: "",
            specifier: Specifier::empty(),
            class: self.kind.wire(),
            message: self.message,
        }
    }

    // Quick construction.

    pub fn bad_value(message: &'static str) -> Self {
        Self { kind: ErrorKind::BadValue, message }
    }

    pub fn protocol(message: &'static str) -> Self {
        Self { kind: ErrorKind::Protocol, message }
    }

    pub fn no_module() -> Self {
        Self { kind: ErrorKind::NoSuchModule, message: "No such module" }
    }

    pub fn no_param() -> Self {
        Self { kind: ErrorKind::NoSuchParameter, message: "No such parameter" }
    }

    pub fn no_command() -> Self {
        Self { kind: ErrorKind::NoSuchCommand, message: "No such command" }
    }

    pub fn read_only() -> Self {
        Self { kind: ErrorKind::ReadOnly, message: "Parameter is read only" }
    }

    pub fn comm_failed(message: &'static str) -> Self {
        Self { kind: ErrorKind::CommunicationFailed, message }
    }
}

#[derive(Debug, Clone)]
pub struct Specifier<'a> {
    pub module: &'a str,
    pub param: Option<&'a str>,
}

impl<'a> Specifier<'a> {
    pub fn new(module: &'a str, param: &'a str) -> Self {
        Self { module, param: Some(param) }
    }

    pub fn empty() -> Self {
        Self { module: "", param: None }
    }
}

impl Display for Specifier<'_> {
    fn fmt(&self, f: &mut Formatter) -> core::fmt::Result {
        if let Some(param) = self.param {
            write!(f, "{}:{}", self.module, param)
        } else {
            write!(f, "{}", self.module)
        }
    }
}


/// Enum that represents any message that can be sent to us.
#[derive(Debug)]
pub enum InMsg<'a> {
    /// identify request
    Idn,
    /// help request
    Help,
    /// description request
    Describe,
    /// event enable request
    Activate { module: &'a str },
    /// event disable request
    Deactivate { module: &'a str },
    /// command execution request
    Do { spec: Specifier<'a>, arg: &'a mut str },
    /// change request
    Change { spec: Specifier<'a>, value: &'a mut str },
    /// read request
    Read { spec: Specifier<'a> },
    /// heartbeat request
    Ping { token: &'a str },
}

/// Enum that represents any message that we can send.
pub enum OutMsg<'a, T> {
    /// identify reply
    IdnReply,
    /// help reply
    Helping { message: &'a str },
    /// description reply
    Describing { id: &'a str, structure: NodeDescription<'a> },
    /// event enable reply
    Active { module: &'a str },
    /// event disable reply
    Inactive { module: &'a str },
    /// command result
    Done { spec: Specifier<'a>, value: T, time: Timestamp },
    /// change result
    Changed { spec: Specifier<'a>, value: T, time: Timestamp },
    /// update event
    Update { spec: Specifier<'a>, value: T, time: Timestamp },
    /// heartbeat reply
    Pong { token: &'a str, value: T, time: Timestamp },
    /// error reply
    Error { request: &'a str, specifier: Specifier<'a>, class: &'a str, message: &'a str },
}

impl InMsg<'_> {
    /// Parse a string slice containing a message.
    ///
    /// This matches a regular expression, and then creates an `InMsg` if successful.
    pub fn parse(msg: &mut [u8]) -> core::result::Result<InMsg<'_>, OutMsg<'_, ()>> {
        match core::str::from_utf8_mut(msg) {
            Err(_) => Err(Error::protocol("invalid UTF8").msg()),
            Ok(msg) => match Self::parse_str(msg) {
                Ok(v) => Ok(v),
                Err(e) => Err(e.msg()),
            }
        }
    }

    fn parse_str(msg: &mut str) -> Result<InMsg<'_>> {
        // Manual split to keep the data as a mutable str.
        for (i, ch) in msg.char_indices() {
            if ch == ' ' {
                let (action, rest) = msg.split_at_mut(i);
                let rest = &mut rest[1..];
                for (i, ch) in rest.char_indices() {
                    if ch == ' ' {
                        let (specifier, data) = rest.split_at_mut(i);
                        let mut data = &mut data[1..];
                        while data.ends_with(char::is_whitespace) { // Manual trim
                            let len = data.len() - 1;
                            data = &mut data[..len];
                        }
                        return Self::parse_inner(action, specifier, data);
                    }
                }
                return Self::parse_inner(action, rest.trim_end(), Default::default());
            }
        }
        Self::parse_inner(msg.trim_end(), "", Default::default())
    }

    fn parse_inner<'a>(action: &'a str, specifier: &'a str, data: &'a mut str) -> Result<'a, InMsg<'a>> {
        let mut spec_split = specifier.splitn(2, ':').map(Into::into);
        let module = spec_split.next().expect("cannot be absent");
        let mut spec = || {
            let param = spec_split.next().ok_or(Error::protocol("missing parameter"))?;
            Ok(Specifier { module, param: Some(param) })
        };

        let parsed = match action {
            wire::READ =>       InMsg::Read { spec: spec()? },
            wire::CHANGE =>     InMsg::Change { spec: spec()?, value: data },
            wire::DO =>         InMsg::Do { spec: spec()?, arg: data },
            wire::DESCRIBE =>   InMsg::Describe,
            wire::ACTIVATE =>   InMsg::Activate { module },
            wire::DEACTIVATE => InMsg::Deactivate { module },
            wire::PING =>       InMsg::Ping { token: specifier },
            wire::IDN =>        InMsg::Idn,
            wire::HELP =>       InMsg::Help,
            _ => return Err(Error::protocol("message type not supported"))
        };

        Ok(parsed)
    }
}

impl<T: ToJson> Display for OutMsg<'_, T> {
    fn fmt(&self, f: &mut Formatter) -> core::fmt::Result {
        match self {
            OutMsg::Update { spec, value, time } =>
                writeln!(f, "{} {} [{},{}]", wire::UPDATE, spec, Json(value), time),
            OutMsg::Changed { spec, value, time } =>
                writeln!(f, "{} {} [{},{}]", wire::CHANGED, spec, Json(value), time),
            OutMsg::Done { spec, value, time } =>
                writeln!(f, "{} {} [{},{}]", wire::DONE, spec, Json(value), time),
            OutMsg::Describing { id, structure } =>
                writeln!(f, "{} {} {}", wire::DESCRIBING, id, structure),
            OutMsg::Active { module } =>
                if module.is_empty() { writeln!(f, "{}", wire::ACTIVE) }
                else { writeln!(f, "{} {}", wire::ACTIVE, module) },
            OutMsg::Inactive { module } =>
                if module.is_empty() { writeln!(f, "{}", wire::INACTIVE) }
                else { writeln!(f, "{} {}", wire::INACTIVE, module) },
            OutMsg::Pong { token, value, time } =>
                writeln!(f, "{} {} [{},{}]", wire::PONG, token, Json(value), time),
            OutMsg::IdnReply => writeln!(f, "{}", wire::IDN_REPLY),
            OutMsg::Helping { message } =>
                writeln!(f, "{}  {}", wire::HELPING, Json(message)),
            OutMsg::Error { request, specifier, class, message } =>
                if request.is_empty() {
                    writeln!(f, "{}  [{},{},{{}}]", wire::ERROR, Json(class), Json(message))
                } else {
                    writeln!(f, "{}_{} {} [{},{},{{}}]",
                             wire::ERROR, request, specifier, Json(class), Json(message))
                }
        }
    }
}

impl Display for Error<'_> {
    fn fmt(&self, f: &mut Formatter) -> core::fmt::Result {
        write!(f, "{}: {}", self.kind.wire(), self.message)
    }
}
