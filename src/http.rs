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
//! WebSocket handling.

use base64::Engine;
use core2::io::Write;
use sha1::Digest;
use crate::ClientId;

const CLIENT: &str = include_str!("index.html");


/// Mask/unmask a frame.
pub fn apply_mask(buf: &mut [u8], mask: [u8; 4]) {
    apply_mask_fast32(buf, mask)
}

/// A safe unoptimized mask application.
fn apply_mask_fallback(buf: &mut [u8], mask: [u8; 4]) {
    for (i, byte) in buf.iter_mut().enumerate() {
        *byte ^= mask[i & 3];
    }
}

/// Faster version of `apply_mask()` which operates on 4-byte blocks.
fn apply_mask_fast32(buf: &mut [u8], mask: [u8; 4]) {
    let mask_u32 = u32::from_ne_bytes(mask);

    let (prefix, words, suffix) = unsafe { buf.align_to_mut::<u32>() };
    apply_mask_fallback(prefix, mask);
    let head = prefix.len() & 3;
    let mask_u32 = if head > 0 {
        if cfg!(target_endian = "big") {
            mask_u32.rotate_left(8 * head as u32)
        } else {
            mask_u32.rotate_right(8 * head as u32)
        }
    } else {
        mask_u32
    };
    for word in words.iter_mut() {
        *word ^= mask_u32;
    }
    apply_mask_fallback(suffix, mask_u32.to_ne_bytes());
}

pub fn decode_ws(input: &mut [u8]) -> Result<Option<(&mut [u8], usize)>, ()> {
    let mut mask = [0; 4];

    if input.len() < 6 {  // must have 2 bytes header and 4 bytes mask
        return Ok(None);
    }

    if input[1] & 0x80 == 0 {  // no unmasked frames allowed
        return Err(());
    }

    // let is_fin = input[0] & 0x80 != 0; // TODO
    let opcode = input[0] & 0x0f;

    if opcode != 1 && opcode != 2 {  // close or other strange frame received
        return Err(());
    }

    let mut len = (input[1] & 0x7f) as usize;

    let start_msg = if len == 126 {
        len = u16::from_be_bytes([input[2], input[3]]) as usize;
        8
    } else if len == 127 {
        len = u64::from_be_bytes([input[2], input[3], input[4], input[5],
                                  input[6], input[7], input[8], input[9]]) as usize;
        14
    } else {
        6
    };

    if input.len() < start_msg + len {
        return Ok(None);  // need more data
    }

    mask.copy_from_slice(&input[start_msg - 4..][..4]);
    let msg = &mut input[start_msg..][..len];
    apply_mask(msg, mask);

    Ok(Some((msg, start_msg + len)))
}

pub fn handle(input: &mut [u8], id: ClientId, mut callback: impl FnMut(ClientId, &dyn Fn(&mut dyn Write)))
              -> Result<Option<usize>, ()>
{
    // Got a complete HTTP request?
    if !input.ends_with(b"\r\n\r\n") {
        return Ok(None);
    }

    // Only GET requests to root are supported.
    if !input.starts_with(b"GET / HTTP/1.1\r\n") {
        return Err(());
    }

    let mut accept_key_buf = [0; 28];
    let mut accept_key = "";
    let mut has_upgrade = false;

    for line in input.split(|&b| b == b'\r') {
        let is_header = |hdr: &[u8]| line.len() >= hdr.len() && line[..hdr.len()].eq_ignore_ascii_case(hdr);

        if is_header(b"\nSec-WebSocket-Key: ") {
            let n = derive_accept_key(&line[20..], &mut accept_key_buf);
            accept_key = core::str::from_utf8(&accept_key_buf[..n]).expect("base64");
        }
        else if is_header(b"\nUpgrade: websocket") {
            has_upgrade = true;
        }
    }

    if has_upgrade && !accept_key.is_empty() {
        // it's a websocket handshake
        callback(id, &|w| { let _ = write!(w, "HTTP/1.1 101 Switching Protocols\r\n\
                                               Upgrade: websocket\r\n\
                                               Connection: Upgrade\r\n\
                                               Sec-WebSocket-Accept: {}\r\n\r\n",
                                           accept_key); });
        Ok(Some(input.len()))
    } else {
        // send our HTML and close the connection
        callback(id, &|w| {
            let _ = write!(w, "HTTP/1.1 200 OK\r\n\
                               Content-type: text/html; charset=utf-8\r\n\
                               Content-length: {}\r\n\r\n{}",
                           CLIENT.len(), CLIENT);
        });
        Err(())
    }
}

fn derive_accept_key(request_key: &[u8], buf: &mut [u8]) -> usize {
    const WS_GUID: &[u8] = b"258EAFA5-E914-47DA-95CA-C5AB0DC85B11";
    let mut sha1 = sha1::Sha1::default();
    sha1.update(request_key);
    sha1.update(WS_GUID);
    base64::prelude::BASE64_STANDARD.encode_slice(&sha1.finalize(), buf)
                                    .expect("buffer size")
}

const BUF_SIZE: usize = 1024;

pub struct WsWriterWrapper<W> {
    writer: W,
    buf: [u8; BUF_SIZE],
    used: usize,
}

impl<W: Write> WsWriterWrapper<W> {
    pub fn new(writer: W) -> Self {
        Self {
            writer,
            buf: [0; BUF_SIZE],
            used: 0,
        }
    }

    pub fn send_frame(&mut self, fin: bool) -> core2::io::Result<()> {
        let mut header = [0u8; 2];
        header[0] = if fin { 0x80 } else { 0x00 } | 0x01;
        if self.used as u8 <= 125 {
            header[1] = self.used as u8;
            self.writer.write_all(&header)?;
        } else {
            header[1] = 126;
            self.writer.write_all(&header)?;
            header[0] = (self.used >> 8) as u8;
            header[1] = self.used as u8;
            self.writer.write_all(&header)?;
        }
        self.writer.write_all(&self.buf[..self.used])?;
        self.used = 0;
        Ok(())
    }
}

impl<W: Write> Write for WsWriterWrapper<W> {
    fn write(&mut self, buf: &[u8]) -> core2::io::Result<usize> {
        let to_send = core::cmp::min(buf.len(), BUF_SIZE - self.used);
        self.buf[self.used..][..to_send].copy_from_slice(&buf[..to_send]);
        self.used += to_send;
        if self.used == BUF_SIZE {
            self.send_frame(false)?;
        }
        Ok(to_send)
    }

    fn flush(&mut self) -> core2::io::Result<()> {
        self.writer.flush()
    }
}
