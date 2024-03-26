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
//! JSON conversion traits.

use core::fmt::{Display, Formatter, Write};
use crate::{Result, Error, DataInfo};

/// Parse a value from JSON and validate it against the given `DataInfo`.
pub trait FromJson<'s> {
    fn from_json<'a>(json: &'s mut str, datainfo: &DataInfo<'a>) -> Result<'a, Self>
    where Self: Sized;
}

/// Format a value to JSON, and optionally validate it against the given
/// `DataInfo`.
pub trait ToJson {
    fn to_json(&self, f: &mut Formatter<'_>) -> core::fmt::Result;
    fn check<'a>(&self, datainfo: &DataInfo<'a>) -> Result<'a, ()>;
}

/// A wrapper to `Display` any supported value as JSON using `ToJson`.
pub struct Json<V>(pub V);

impl<'a, V: ToJson> Display for Json<&'a V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        self.0.to_json(f)
    }
}


impl FromJson<'_> for () {
    fn from_json<'a>(json: &mut str, datainfo: &DataInfo<'a>) -> Result<'a, Self> {
        if !matches!(datainfo, DataInfo::Null) {
            return Err(Error::bad_value("expected Null datainfo"));
        }
        if json == "null" {
            Ok(())
        } else {
            Err(Error::bad_value("expected null"))
        }
    }
}

impl FromJson<'_> for bool {
    fn from_json<'a>(json: &mut str, datainfo: &DataInfo<'a>) -> Result<'a, Self> {
        if !matches!(datainfo, DataInfo::Bool) {
            return Err(Error::bad_value("expected Bool datainfo"));
        }
        if json == "true" {
            Ok(true)
        } else if json == "false" {
            Ok(false)
        } else {
            Err(Error::bad_value("expected bool"))
        }
    }
}

impl FromJson<'_> for f64 {
    fn from_json<'a>(json: &mut str, datainfo: &DataInfo<'a>) -> Result<'a, Self> {
        if let DataInfo::Double { min, max, .. } = datainfo {
            let val = json.parse::<f64>().map_err(|_| Error::bad_value("expected double"))?;
            if let Some(min) = min {
                if val < *min { return Err(Error::bad_value("value too small")); }
            }
            if let Some(max) = max {
                if val > *max { return Err(Error::bad_value("value too large")); }
            }
            Ok(val)
        } else {
            Err(Error::bad_value("expected Double datainfo"))
        }
    }
}

impl FromJson<'_> for i32 {
    fn from_json<'a>(json: &mut str, datainfo: &DataInfo<'a>) -> Result<'a, Self> {
        if let DataInfo::Enum { members } = datainfo {
            match deserialize_json_string(json) {
                Some(strname) => for (name, value) in members.iter() {
                    if strname == *name {
                        return Ok(*value);
                    }
                }
                None => match json.parse::<i32>() {
                    Ok(extvalue) => for (_, value) in members.iter() {
                        if extvalue == *value {
                            return Ok(*value);
                        }
                    }
                    Err(_) => return Err(Error::bad_value("expected string")),
                }
            }
            Err(Error::bad_value("value not in enum"))
        } else {
            Err(Error::bad_value("expected Enum datainfo"))
        }
    }
}

impl FromJson<'_> for i64 {
    fn from_json<'a>(json: &mut str, datainfo: &DataInfo<'a>) -> Result<'a, Self> {
        if let DataInfo::Int { min, max } = datainfo {
            let val = json.parse::<i64>().map_err(|_| Error::bad_value("expected int"))?;
            if val < *min { return Err(Error::bad_value("value too small")); }
            if val > *max { return Err(Error::bad_value("value too large")); }
            Ok(val)
        } else {
            Err(Error::bad_value("expected Int datainfo"))
        }
    }
}

fn check_str<'s, 'a>(s: &'s str, datainfo: &DataInfo<'a>) -> Result<'a, ()> {
    if let DataInfo::Str { minchars, maxchars, is_utf8 } = datainfo {
        if let Some(min) = minchars {
            if s.len() < *min { return Err(Error::bad_value("value too short")); }
        }
        if s.len() > *maxchars { return Err(Error::bad_value("value too long")); }
        if !is_utf8 && s.chars().any(|c| !c.is_ascii()) {
            return Err(Error::bad_value("value contains non-ASCII characters"));
        }
    }
    Ok(())
}

impl<'s> FromJson<'s> for &'s mut str {
    fn from_json<'a>(json: &'s mut str, datainfo: &DataInfo<'a>) -> Result<'a, Self> {
        match deserialize_json_string(json) {
            Some(s) => {
                check_str(s, datainfo)?;
                Ok(s)
            }
            None => Err(Error::bad_value("expected string")),
        }
    }
}

#[cfg(feature = "std")]
impl<'s> FromJson<'s> for String {
    fn from_json<'a>(json: &'s mut str, datainfo: &DataInfo<'a>) -> Result<'a, Self> {
        match deserialize_json_string(json) {
            Some(s) => {
                check_str(s, datainfo)?;
                Ok(s.to_string())
            }
            None => Err(Error::bad_value("expected string")),
        }
    }
}

#[cfg(feature = "heapless")]
impl<'s, const N: usize> FromJson<'s> for heapless::String<N> {
    fn from_json<'a>(json: &'s mut str, datainfo: &DataInfo<'a>) -> Result<'a, Self> {
        match deserialize_json_string(json) {
            Some(s) => {
                check_str(s, datainfo)?;
                core::str::FromStr::from_str(s).map_err(|_| Error::bad_value("value too long"))
            }
            None => Err(Error::bad_value("expected string")),
        }
    }
}

impl ToJson for () {
    fn to_json(&self, f: &mut Formatter) -> core::fmt::Result {
        f.write_str("null")
    }
    fn check<'a>(&self, datainfo: &DataInfo<'a>) -> Result<'a, ()> {
        if !matches!(datainfo, DataInfo::Null) {
            return Err(Error::bad_value("expected Null datainfo"));
        }
        Ok(())
    }
}

impl ToJson for bool {
    fn to_json(&self, f: &mut Formatter) -> core::fmt::Result {
        write!(f, "{}", self)
    }
    fn check<'a>(&self, datainfo: &DataInfo<'a>) -> Result<'a, ()> {
        if !matches!(datainfo, DataInfo::Bool) {
            return Err(Error::bad_value("expected Bool datainfo"));
        }
        Ok(())
    }
}

impl ToJson for i32 {
    fn to_json(&self, f: &mut Formatter) -> core::fmt::Result {
        write!(f, "{}", self)
    }
    fn check<'a>(&self, datainfo: &DataInfo<'a>) -> Result<'a, ()> {
        if let DataInfo::Enum { members } = datainfo {
            if members.iter().any(|(_, v)| *v == *self) {
                Ok(())
            } else {
                Err(Error::bad_value("value not in enum"))
            }
        } else {
            Err(Error::bad_value("expected Enum datainfo"))
        }
    }
}

impl ToJson for i64 {
    fn to_json(&self, f: &mut Formatter) -> core::fmt::Result {
        write!(f, "{}", self)
    }
    fn check<'a>(&self, datainfo: &DataInfo<'a>) -> Result<'a, ()> {
        if let DataInfo::Int { min, max } = datainfo {
            if self < min { return Err(Error::bad_value("value too small")); }
            if self > max { return Err(Error::bad_value("value too large")); }
            Ok(())
        } else {
            Err(Error::bad_value("expected Int datainfo"))
        }
    }
}

impl ToJson for f64 {
    fn to_json(&self, f: &mut Formatter) -> core::fmt::Result {
        let mut buf = dtoa::Buffer::new();
        let res = buf.format(*self);
        f.write_str(res)
    }
    fn check<'a>(&self, datainfo: &DataInfo<'a>) -> Result<'a, ()> {
        if let DataInfo::Double { min, max, .. } = datainfo {
            if let Some(min) = min {
                if self < min { return Err(Error::bad_value("value too small")); }
            }
            if let Some(max) = max {
                if self > max { return Err(Error::bad_value("value too large")); }
            }
            Ok(())
        } else {
            Err(Error::bad_value("expected Double datainfo"))
        }
    }
}

impl<'a> ToJson for &'a str {
    fn to_json(&self, f: &mut Formatter) -> core::fmt::Result {
        f.write_char('"')?;
        for c in self.chars() {
            match c {
                '"' => f.write_str("\\\"")?,
                '\\' => f.write_str("\\\\")?,
                '\n' => f.write_str("\\n")?,
                '\r' => f.write_str("\\r")?,
                '\t' => f.write_str("\\t")?,
                c if (c as u32) < 0x20 => write!(f, "\\u{:04x}", c as u32)?,
                c => f.write_char(c)?,
            }
        }
        f.write_char('"')
    }
    fn check<'b>(&self, datainfo: &DataInfo<'b>) -> Result<'b, ()> {
        if let DataInfo::Str { minchars, maxchars, .. } = datainfo {
            if let Some(min) = minchars {
                if self.len() < *min { return Err(Error::bad_value("value too short")); }
            }
            if self.len() > *maxchars { return Err(Error::bad_value("value too long")); }
            // TODO is_utf8
            Ok(())
        } else {
            Err(Error::bad_value("expected String datainfo"))
        }
    }
}

impl<'a> ToJson for &'a mut str {
    fn to_json(&self, f: &mut Formatter) -> core::fmt::Result {
        <&str as ToJson>::to_json(&&**self, f)
    }
    fn check<'b>(&self, datainfo: &DataInfo<'b>) -> Result<'b, ()> {
        <&str as ToJson>::check(&&**self, datainfo)
    }
}

#[cfg(feature = "std")]
impl ToJson for String {
    fn to_json(&self, f: &mut Formatter) -> core::fmt::Result {
        self.as_str().to_json(f)
    }
    fn check<'b>(&self, datainfo: &DataInfo<'b>) -> Result<'b, ()> {
        self.as_str().check(datainfo)
    }
}

#[cfg(feature = "heapless")]
impl<const N: usize> ToJson for heapless::String<N> {
    fn to_json(&self, f: &mut Formatter) -> core::fmt::Result {
        self.as_str().to_json(f)
    }
    fn check<'b>(&self, datainfo: &DataInfo<'b>) -> Result<'b, ()> {
        self.as_str().check(datainfo)
    }
}

impl<'a, T: ToJson> ToJson for &'a [T] {
    fn to_json(&self, f: &mut Formatter) -> core::fmt::Result {
        f.write_char('[')?;
        for (i, item) in self.iter().enumerate() {
            if i > 0 { f.write_str(",")?; }
            item.to_json(f)?;
        }
        f.write_char(']')
    }
    fn check<'b>(&self, datainfo: &DataInfo<'b>) -> Result<'b, ()> {
        if let DataInfo::Array { minlen, maxlen, members } = datainfo {
            if self.len() < *minlen { return Err(Error::bad_value("array too short")); }
            if self.len() > *maxlen { return Err(Error::bad_value("array too long")); }
            for item in self.iter() {
                item.check(members)?;
            }
            Ok(())
        } else {
            Err(Error::bad_value("expected Array datainfo"))
        }
    }
}

#[cfg(feature = "std")]
impl<T: ToJson> ToJson for Vec<T> {
    fn to_json(&self, f: &mut Formatter) -> core::fmt::Result {
        self.as_slice().to_json(f)
    }
    fn check<'b>(&self, datainfo: &DataInfo<'b>) -> Result<'b, ()> {
        self.as_slice().check(datainfo)
    }
}

#[cfg(feature = "heapless")]
impl<T: ToJson, const N: usize> ToJson for heapless::Vec<T, N> {
    fn to_json(&self, f: &mut Formatter) -> core::fmt::Result {
        self.as_slice().to_json(f)
    }
    fn check<'b>(&self, datainfo: &DataInfo<'b>) -> Result<'b, ()> {
        self.as_slice().check(datainfo)
    }
}

macro_rules! braces {
    ($first:tt, $($rest:tt)+) => { concat!("[{},", braces!(@ $($rest)+)) };
    (@ $first:tt, $($rest:tt)+) => { concat!("{},", braces!(@ $($rest)+)) };
    (@ $last:tt) => { "{}]" };
}

macro_rules! tojson_for_tuple {
    ($($tv:tt),+ : $len:tt : $($idx:tt),+) => {

        impl<$($tv: ToJson),+> ToJson for ($($tv),+) {
            fn to_json(&self, f: &mut Formatter) -> core::fmt::Result {
                write!(f, braces!($($tv),+), $( Json(&self.$idx) ),+ )
            }
            fn check<'a>(&self, datainfo: &DataInfo<'a>) -> Result<'a, ()> {
                if let DataInfo::Tuple { members } = datainfo {
                    if members.len() != $len {
                        return Err(Error::bad_value("expected Tuple datainfo with 2 members"));
                    }
                    $( self.$idx.check(&members[$idx])?; )+
                    Ok(())
                } else {
                    Err(Error::bad_value("expected Tuple datainfo"))
                }
            }
        }

    }
}

tojson_for_tuple!(T1, T2 : 2 : 0, 1);
tojson_for_tuple!(T1, T2, T3 : 3 : 0, 1, 2);
tojson_for_tuple!(T1, T2, T3, T4 : 4 : 0, 1, 2, 3);
tojson_for_tuple!(T1, T2, T3, T4, T5 : 5 : 0, 1, 2, 3, 4);
tojson_for_tuple!(T1, T2, T3, T4, T5, T6 : 6 : 0, 1, 2, 3, 4, 5);


/// In-place deserialize a JSON string.
fn deserialize_json_string(input: &mut str) -> Option<&mut str> {
    let bytes = unsafe { input.as_bytes_mut() };

    // check for quotes
    let len = bytes.len();
    if len < 2 || bytes[0] != b'"' || bytes[len - 1] != b'"' {
        return None;
    }

    // unescape
    let mut in_ = 1;
    let mut out = 0;
    let mut esc = false;
    while in_ < len - 1 {
        let c = bytes[in_];
        if esc {
            match c {
                b'"' => bytes[out] = b'"',
                b'\\' => bytes[out] = b'\\',
                b'/' => bytes[out] = b'/',
                b'b' => bytes[out] = b'\x08',
                b'f' => bytes[out] = b'\x0c',
                b'n' => bytes[out] = b'\n',
                b'r' => bytes[out] = b'\r',
                b't' => bytes[out] = b'\t',
                b'u' => {
                    let mut val = 0;
                    for _ in 0..4 {
                        in_ += 1;
                        // We can index safely because we know the last
                        // character is a quote.
                        let c = bytes[in_];
                        val = val * 16 + match c {
                            b'0'..=b'9' => c - b'0',
                            b'a'..=b'f' => c - b'a' + 10,
                            b'A'..=b'F' => c - b'A' + 10,
                            _ => return None,
                        } as u16;
                    }
                    if val < 0x80 {
                        bytes[out] = val as u8;
                    } else if val < 0x800 {
                        bytes[out] = 0xc0 | (val >> 6) as u8;
                        bytes[out + 1] = 0x80 | (val & 0x3f) as u8;
                        out += 1;
                    } else {
                        bytes[out] = 0xe0 | (val >> 12) as u8;
                        bytes[out + 1] = 0x80 | ((val >> 6) & 0x3f) as u8;
                        bytes[out + 2] = 0x80 | (val & 0x3f) as u8;
                        out += 2;
                    }
                }
                _ => return None,
            }
            out += 1;
            esc = false;
        } else if c == b'\\' {
            esc = true;
        } else {
            bytes[out] = c;
            out += 1;
        }
        in_ += 1;
    }
    let final_len = out;

    // zero out the rest to avoid UB
    while out < bytes.len() {
        bytes[out] = 0;
        out += 1;
    }

    // now we can use `input` again, everything is valid UTF-8
    Some(&mut input[..final_len])
}
