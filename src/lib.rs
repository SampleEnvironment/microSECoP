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
//
// -----------------------------------------------------------------------------
//
//! TODO document this.

#![no_std]

pub mod wire;
pub mod proto;
pub mod json;
pub mod datamodel;
pub mod node;
mod http;

pub use core2::io;

pub use proto::{ClientId, Result, Timestamp, Error, ErrorKind};
pub use json::{FromJson, ToJson, Json};
pub use datamodel::{DataInfo, NodeDescription, ModuleDescription, AccessibleDescription};
pub use node::{SecNode, Module, Modules, ModuleInternals};
