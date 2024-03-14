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
//! SECoP data types and datainfo.

use core::fmt::{Display, Formatter};
use crate::Json;


pub struct NodeDescription<'a> {
    pub equipment_id: &'a str,
    pub firmware: &'a str,
    pub version: &'a str,
    pub description: &'a str,
    pub interface: &'a str,
    pub modules: &'a [(&'a str, ModuleDescription<'a>)],
}

impl Display for NodeDescription<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "{{\"equipment_id\":{},\"firmware\":{},\"version\":{},\
                   \"description\":{},\"interface\":{},\"modules\":{{",
               Json(&self.equipment_id), Json(&self.firmware),
               Json(&self.version), Json(&self.description),
               Json(&self.interface))?;
        for (i, (name, desc)) in self.modules.iter().enumerate() {
            if i > 0 { write!(f, ",")?; }
            write!(f, "{}:{}", Json(name), desc)?;
        }
        write!(f, "}}}}")
    }
}


pub struct ModuleDescription<'a> {
    pub description: &'a str,
    pub implementation: &'a str,
    pub interface_classes: &'a [&'a str],
    pub features: &'a [&'a str],
    pub accessibles: &'a [(&'a str, AccessibleDescription<'a>)],
}

impl Display for ModuleDescription<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "{{\"description\":{},\"implementation\":{},\"interface_classes\":[",
               Json(&self.description), Json(&self.implementation))?;
        for (i, iface) in self.interface_classes.iter().enumerate() {
            if i > 0 { write!(f, ",")?; }
            write!(f, "{}", Json(iface))?;
        }
        write!(f, "],\"features\":[")?;
        for (i, feature) in self.features.iter().enumerate() {
            if i > 0 { write!(f, ",")?; }
            write!(f, "{}", Json(feature))?;
        }
        write!(f, "],\"accessibles\":{{")?;
        for (i, (name, acc)) in self.accessibles.iter().enumerate() {
            if i > 0 { write!(f, ",")?; }
            write!(f, "{}:{}", Json(name), acc)?;
        }
        write!(f, "}}}}")
    }
}

pub struct AccessibleDescription<'a> {
    pub description: &'a str,
    pub readonly: bool,
    pub datainfo: &'a DataInfo<'a>,
    pub group: Option<&'a str>,
}

impl Display for AccessibleDescription<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "{{\"description\":{},\"readonly\":{},\"datainfo\":{}",
               Json(&self.description), self.readonly, self.datainfo)?;
        if let Some(grp) = &self.group {
            write!(f, ",\"group\":{}", Json(grp))?;
        }
        write!(f, "}}")
    }
}

pub enum DataInfo<'a> {
    Null,
    Bool,
    Double { min: Option<f64>, max: Option<f64>,
             unit: Option<&'a str>, fmtstr: Option<&'a str>,
             abs_res: Option<f64>, rel_res: Option<f64> },
    Scaled { scale: f64, min: i64, max: i64,
             unit: Option<&'a str>, fmtstr: Option<&'a str>,
             abs_res: Option<f64>, rel_res: Option<f64> },
    Int { min: i64, max: i64 },
    Str { minchars: Option<usize>, maxchars: usize, is_utf8: bool },
    Blob { minbytes: Option<usize>, maxbytes: usize },
    Enum { members: &'a [(&'a str, i32)] },
    Array { minlen: usize, maxlen: usize, members: &'a DataInfo<'a> },
    Tuple { members: &'a [DataInfo<'a>] },
    Struct { members: &'a [(&'a str, DataInfo<'a>)] },
    Command { argument: &'a DataInfo<'a>, result: &'a DataInfo<'a> },
}

impl Display for DataInfo<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        match self {
            DataInfo::Null => write!(f, "null"),
            DataInfo::Bool => write!(f, "{{\"type\":\"bool\"}}"),
            DataInfo::Double { min, max, unit, fmtstr, abs_res, rel_res } => {
                write!(f, "{{\"type\":\"double\"")?;
                if let Some(v) = min { write!(f, ",\"min\":{}", Json(v))?; }
                if let Some(v) = max { write!(f, ",\"max\":{}", Json(v))?; }
                if let Some(v) = unit { write!(f, ",\"unit\":{}", Json(v))?; }
                if let Some(v) = fmtstr { write!(f, ",\"format\":{}", Json(v))?; }
                if let Some(v) = abs_res { write!(f, ",\"absolute_resolution\":{}", Json(v))?; }
                if let Some(v) = rel_res { write!(f, ",\"relative_resolution\":{}", Json(v))?; }
                write!(f, "}}")
            }
            DataInfo::Scaled { scale, min, max, unit, fmtstr, abs_res, rel_res } => {
                write!(f, "{{\"type\":\"scaled\",\"scale\":{},\"min\":{},\"max\":{}",
                       Json(scale), min, max)?;
                if let Some(v) = unit { write!(f, ",\"unit\":{}", Json(v))?; }
                if let Some(v) = fmtstr { write!(f, ",\"format\":{}", Json(v))?; }
                if let Some(v) = abs_res { write!(f, ",\"absolute_resolution\":{}", Json(v))?; }
                if let Some(v) = rel_res { write!(f, ",\"relative_resolution\":{}", Json(v))?; }
                write!(f, "}}")
            }
            DataInfo::Int { min, max } =>
                write!(f, "{{\"type\":\"int\",\"min\":{},\"max\":{}}}", min, max),
            DataInfo::Str { minchars, maxchars, is_utf8 } => {
                write!(f, "{{\"type\":\"string\"")?;
                if let Some(v) = minchars { write!(f, ",\"minchars\":{}", v)?; }
                write!(f, ",\"maxchars\":{}", maxchars)?;
                if *is_utf8 { write!(f, ",\"isUTF8\":true")?; }
                write!(f, "}}")
            }
            DataInfo::Blob { minbytes, maxbytes } => {
                write!(f, "{{\"type\":\"blob\"")?;
                if let Some(v) = minbytes { write!(f, ",\"minbytes\":{}", v)?; }
                write!(f, ",\"maxbytes\":{}", maxbytes)?;
                write!(f, "}}")
            }
            DataInfo::Enum { members } => {
                write!(f, "{{\"type\":\"enum\",\"members\":{{")?;
                for (i, (name, value)) in members.iter().enumerate() {
                    if i > 0 { write!(f, ",")?; }
                    write!(f, "{}:{}", Json(name), value)?;
                }
                write!(f, "}}}}")
            }
            DataInfo::Array { minlen, maxlen, members } =>
                write!(f, "{{\"type\":\"array\",\"minlen\":{},\"maxlen\":{},\"members\":{}}}",
                       minlen, maxlen, members),
            DataInfo::Tuple { members } => {
                write!(f, "{{\"type\":\"tuple\",\"members\":[")?;
                for (i, member) in members.iter().enumerate() {
                    if i > 0 { write!(f, ",")?; }
                    write!(f, "{}", member)?;
                }
                write!(f, "]}}")
            }
            DataInfo::Struct { members } => {
                write!(f, "{{\"type\":\"struct\",\"members\":{{")?;
                for (i, (name, member)) in members.iter().enumerate() {
                    if i > 0 { write!(f, ",")?; }
                    write!(f, "{}:{}", Json(name), member)?;
                }
                write!(f, "}}}}")
            }
            DataInfo::Command { argument, result } =>
                write!(f, "{{\"type\":\"command\",\"argument\":{},\"result\":{}}}",
                       argument, result),
        }
    }
}

pub trait CustomDataInfo {
    const DATAINFO: DataInfo<'static>;
}
