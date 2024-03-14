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
//! SECoP on-wire constants.

pub const IDN: &str = "*IDN?";
pub const IDN_REPLY: &str = "ISSE&SINE2020,SECoP,V2019-09-16,v1.0";
pub const DESCRIBE: &str = "describe";
pub const DESCRIBING: &str = "describing";
pub const ACTIVATE: &str = "activate";
pub const ACTIVE: &str = "active";
pub const DEACTIVATE: &str = "deactivate";
pub const INACTIVE: &str = "inactive";
pub const PING: &str = "ping";
pub const PONG: &str = "pong";
pub const ERROR: &str = "error";
pub const DO: &str = "do";
pub const DONE: &str = "done";
pub const CHANGE: &str = "change";
pub const CHANGED: &str = "changed";
pub const READ: &str = "read";
pub const UPDATE: &str = "update";
pub const HELP: &str = "help";
pub const HELPING: &str = "helping";
