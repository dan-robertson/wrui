#![allow(unused)]
use serde;
use serde::{Serialize, Serializer};
use glutin::{Event, ElementState, ScanCode, VirtualKeyCode};
use glutin::{MouseScrollDelta, TouchPhase, MouseButton, Touch};
use std::path::PathBuf;
use std::io;
use std::io::Write;
use std::result;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::error;

pub struct SerializableEvent(pub Event);

impl Serialize for SerializableEvent {
    fn serialize<S>(&self, serializer: S)
                    -> result::Result<S::Ok, S::Error> where S: Serializer {
        EventD::serialize(&self.0, serializer)
    }
}

// we have s sexpr serializer
pub struct SExprSerializer<W> {
    writer: W,
    should_space_before_next: bool,
}

#[derive(Debug)]
pub enum Error{
    IOError(io::Error),
    SerdeError(String),
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match self {
            &Error::IOError(ref e) => e.description(),
            &Error::SerdeError(ref s) => s.as_str()
        }
    }
    fn cause(&self) -> Option<&error::Error> {
        match self {
            &Error::IOError(ref e) => Some(e),
            &Error::SerdeError(ref _s) => None,
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> result::Result<(), fmt::Error> {
        match self {
            &Error::IOError(ref e) => e.fmt(f),
            &Error::SerdeError(ref s) => s.fmt(f),
        }
    }
}

type Result<T> = result::Result<T, Error>;

impl serde::ser::Error for Error {
    fn custom<T: Display>(msg: T) -> Error {
        Error::SerdeError(msg.to_string())
    }
}

trait ToError<T> {
    fn t(self) -> Result<T>;
}

impl<T> ToError<T> for result::Result<T, io::Error> {
    fn t(self) -> Result<T> {
        self.map_err(|e| Error::IOError(e))
    }
}

impl<W> SExprSerializer<W> where W: Write{
    pub fn new(writer: W) -> Self {
        SExprSerializer {
            writer: writer,
            should_space_before_next: false
        }
    }

    fn space(&mut self) -> Result<()> {
        if self.should_space_before_next {
            self.should_space_before_next = false;
            self.writer.write_all(b" ").t()
        } else {
            Ok(())
        }
    }
    fn spacenext(&mut self) {
        self.should_space_before_next = true;
    }

    fn open_paren(&mut self) -> Result<()> {
        self.space().and_then(|_| {
            self.writer.write_all(b"(").t()
        })
    }
    fn close_paren(&mut self) -> Result<()> {
        self.spacenext();
        self.writer.write_all(b")").t()
    }
}

impl<'a, W> Serializer for &'a mut SExprSerializer<W> where W: Write {
    type Ok = ();
    type Error = Error;
    
    type SerializeSeq = Self;
    type SerializeTuple = Self;
    type SerializeTupleStruct = Self;
    type SerializeTupleVariant = Self;
    type SerializeMap = Self;
    type SerializeStruct = Self;
    type SerializeStructVariant = Self;

    fn serialize_bool(self, v: bool) -> Result<()> {
        self.space().and_then(|_| {
            self.spacenext();
            if v { self.writer.write_all(b"T").t() }
            else { self.writer.write_all(b"NIL").t() }
        })
    }

    fn serialize_i8(self, v: i8) -> Result<()> { self.serialize_i64(v as i64) }
    fn serialize_i16(self, v: i16) -> Result<()> { self.serialize_i64(v as i64) }
    fn serialize_i32(self, v: i32) -> Result<()> { self.serialize_i64(v as i64) }
    fn serialize_u8(self, v: u8) -> Result<()> { self.serialize_u64(v as u64) }
    fn serialize_u16(self, v: u16) -> Result<()> { self.serialize_u64(v as u64) }
    fn serialize_u32(self, v: u32) -> Result<()> { self.serialize_u64(v as u64) }

    fn serialize_i64(self, v: i64) -> Result<()> { 
        self.space().and_then(|_| {
            self.spacenext();
            write!(self.writer, "{}", v).t()
        })
    }
    fn serialize_u64(self, v: u64) -> Result<()> { 
        self.space().and_then(|_| {
            self.spacenext();
            write!(self.writer, "{}", v).t()
        })
    }

    fn serialize_f32(self, v: f32) -> Result<()> {
        self.space().and_then(|_| {
            self.spacenext();
            write!(self.writer, "{}", v).t()
        })
    }

    fn serialize_f64(self, v: f64) -> Result<()> {
        self.space().and_then(|_| {
            self.spacenext();
            write!(self.writer, "{}", v).t()
        })
    }

    fn serialize_char(self, v: char) -> Result<()> {
        self.space().and_then(|_| {
            self.spacenext();
            match v {
                '\n' => self.writer.write_all(b"#\\Newline"),
                ' '  => self.writer.write_all(b"#\\Space"),
                '\t' => self.writer.write_all(b"#\\Tab"),
                '\r' => self.writer.write_all(b"#\\Return"),
                _ => write!(self.writer, "#\\{}", v),
            }.t()
        })
    }
    
    fn serialize_str(self, v: &str) -> Result<()> {
        self.space().and_then(|_| {
            self.spacenext();
            self.writer.write_all(b"\"").t().and_then(|_| {
                let mut s = v;
                'outer: while s.len() > 0 {
                    for (i, chr) in v.char_indices() {
                        match chr {
                            '"' | '\\' => {
                                let (pre, post) = s.split_at(i);
                                try!(self.writer.write_all(pre.as_bytes()).t());
                                try!(write!(self.writer, "\\{}", chr).t());
                                let (_, post2) = post.split_at(1);
                                s = post2;
                                continue 'outer;
                            }
                            _   => (), // continue
                        }
                    }
                    return self.writer.write_all(s.as_bytes()).t().and_then(|_| {
                        self.writer.write_all(b"\"").t()
                    });
                }
                self.writer.write_all(b"\"").t()
            })
        })
    }
    
    fn serialize_bytes(self, v: &[u8]) -> Result<()> {
        use serde::ser::SerializeSeq;
        let mut seq = self.serialize_seq(Some(v.len()))?;
        for byte in v {
            seq.serialize_element(byte)?;
        }
        seq.end()
    }

    fn serialize_none(self) -> Result<()> { self.serialize_bool(false) }
    fn serialize_unit(self) -> Result<()> { self.serialize_bool(false) }
    fn serialize_unit_struct(self, _name: &'static str) -> Result<()> { self.serialize_bool(false) }
    fn serialize_some<T>(self, v: &T) -> Result<()> where T: ?Sized + Serialize {
        v.serialize(self)
    }

    fn serialize_unit_variant(self, _name: &'static str, _variant_index: u32,
                              variant: &'static str) -> Result<()> {
        self.space().and_then(|_| {
            self.spacenext();
            // can probably get away without checking for space, backslash etc
            self.writer.write_all(variant.to_uppercase().as_bytes()).t()
        })
    }

    fn serialize_newtype_struct<T>(self, _name: &'static str, value: &T)
                                   -> Result<()> where T: ?Sized + Serialize {
        value.serialize(self)
    }

    fn serialize_newtype_variant<T>(self, name: &'static str, variant_index: u32,
                                    variant: &'static str, value: &T)
                                    -> Result<()> where T: ?Sized + Serialize {
        try!(self.open_paren());
        try!(self.serialize_unit_variant(name, variant_index, variant));
        try!(value.serialize(&mut *self));
        self.close_paren()
    }
    
    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq> {
        self.open_paren().and_then(|_| { Ok(self) })
    }
    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple> {
        self.serialize_seq(Some(len))
    }
    fn serialize_tuple_struct(self, _name: &'static str, len: usize)
                              -> Result<Self::SerializeTupleStruct> {
        self.serialize_seq(Some(len))
    }
    fn serialize_tuple_variant(self, name: &'static str, variant_index: u32,
                               variant: &'static str, len: usize)
                               -> Result<Self::SerializeTupleVariant>{
        self.serialize_seq(Some(len)).and_then(|s| {
            s.serialize_unit_variant(name, variant_index, variant).and_then(|_| Ok(s))
        })
    }
    fn serialize_map(self, len: Option<usize>) -> Result<Self::SerializeMap> {
        self.serialize_seq(len)
    }
    fn serialize_struct(self, _name: &'static str,
                        len: usize) -> Result<Self::SerializeStruct> {
        self.serialize_seq(Some(len))
    }
    fn serialize_struct_variant(self, name: &'static str, variant_index: u32,
                               variant: &'static str, len: usize)
                               -> Result<Self::SerializeStructVariant>{
        self.serialize_seq(Some(len)).and_then(|s| {
            s.serialize_unit_variant(name, variant_index, variant).and_then(|_| {
                Ok(s)
            })
        })
    }
}

impl<'a, W> serde::ser::SerializeSeq for &'a mut SExprSerializer<W> where W: Write {
    type Ok = (); type Error = Error;
    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
        where T: ?Sized + Serialize 
    { value.serialize(&mut **self) }
    fn end(self) -> Result<()> { self.close_paren() }
}
impl<'a, W> serde::ser::SerializeTuple for &'a mut SExprSerializer<W> where W: Write {
    type Ok = (); type Error = Error;
    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
        where T: ?Sized + Serialize 
    { value.serialize(&mut **self) }
    fn end(self) -> Result<()> { self.close_paren() }
}
impl<'a, W> serde::ser::SerializeTupleStruct for &'a mut SExprSerializer<W> where W: Write {
    type Ok = (); type Error = Error;
    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
        where T: ?Sized + Serialize 
    { value.serialize(&mut **self) }
    fn end(self) -> Result<()> { self.close_paren() }
}
impl<'a, W> serde::ser::SerializeTupleVariant for &'a mut SExprSerializer<W> where W: Write {
    type Ok = (); type Error = Error;
    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
        where T: ?Sized + Serialize 
    { value.serialize(&mut **self) }
    fn end(self) -> Result<()> { self.close_paren() }
}
// implemented as an alist
impl<'a, W> serde::ser::SerializeMap for &'a mut SExprSerializer<W> where W: Write {
    type Ok = (); type Error = Error;
    fn serialize_key<T>(&mut self, key: &T) -> Result<()>
        where T: ?Sized + Serialize { 
        self.open_paren().and_then(|_| {
            key.serialize(&mut **self)
        })
    }
    fn serialize_value<T>(&mut self, value: &T) -> Result<()>
        where T: ?Sized + Serialize { 
        value.serialize(&mut **self).and_then(|_| {
            self.close_paren()
        })
    }
    fn end(self) -> Result<()> { self.close_paren() }
}
// implemented as a plist
impl<'a, W> serde::ser::SerializeStruct for &'a mut SExprSerializer<W> where W: Write {
    type Ok = (); type Error = Error;
    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
        where T: ?Sized + Serialize { 
        self.space().and_then(|_| {
            self.writer.write_all(b":").t().and_then(|_| {
                self.serialize_unit_variant("", 0, key).and_then(|_| {
                    self.spacenext();
                    value.serialize(&mut **self)
                })
            })
        })
    }
    fn end(self) -> Result<()> { self.close_paren() }
}
//same
impl<'a, W> serde::ser::SerializeStructVariant for &'a mut SExprSerializer<W> where W: Write {
    type Ok = (); type Error = Error;
    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
        where T: ?Sized + Serialize { 
        self.space().and_then(|_| {
            self.writer.write_all(b":").t().and_then(|_| {
                self.serialize_unit_variant("", 0, key).and_then(|_| {
                    self.spacenext();
                    value.serialize(&mut **self)
                })
            })
        })
    }
    fn end(self) -> Result<()> { self.close_paren() }
}


#[derive(Serialize)]
#[serde(remote = "Event")]
enum EventD {
    Resized(u32, u32),
    Moved(i32, i32),
    Closed,
    DroppedFile(PathBuf),
    ReceivedCharacter(char),
    Focused(bool),
    KeyboardInput(
        #[serde(with = "ElementStateD")] ElementState,
        ScanCode,
        #[serde(serialize_with = "serialize_virtual_key_code")] 
        Option<VirtualKeyCode>),
    MouseMoved(i32, i32),
    MouseWheel(
        #[serde(with = "MouseScrollDeltaD")] MouseScrollDelta,
        #[serde(with = "TouchPhaseD")] TouchPhase,
        Option<(i32, i32)>),
    MouseInput(
        #[serde(with = "ElementStateD")] ElementState,
        #[serde(with = "MouseButtonD")] MouseButton,
        Option<(i32, i32)>),
    TouchpadPressure(f32, i64),
    Awakened,
    Refresh,
    Suspended(bool),
    #[serde(with = "TouchD")] Touch(Touch),
}

#[derive(Serialize)]
#[serde(remote = "ElementState")]
enum ElementStateD {
    Pressed,
    Released,
}

#[derive(Serialize)]
#[serde(remote = "MouseScrollDelta")]
enum MouseScrollDeltaD {
    LineDelta(f32, f32),
    PixelDelta(f32, f32),
}

#[derive(Serialize)]
#[serde(remote = "TouchPhase")]
enum TouchPhaseD {
    Started,
    Moved,
    Ended,
    Cancelled,
}

#[derive(Serialize)]
#[serde(remote = "Touch")]
struct TouchD {
    #[serde(with = "TouchPhaseD")]
    pub phase: TouchPhase,
    pub location: (f64, f64),
    pub id: u64,
}

#[derive(Serialize)]
#[serde(remote = "MouseButton")]
enum MouseButtonD {
    Left,
    Right,
    Middle,
    Other(u8),
}

fn serialize_virtual_key_code<S>(x: &Option<VirtualKeyCode>, serializer: S)
                                 -> result::Result<S::Ok, S::Error> where S: Serializer {
    match x {
        &None => serializer.serialize_none(),
        &Some(vkk) => serializer.serialize_some(&VirtualKeyCodeWrapper(vkk)),
    }
}

struct VirtualKeyCodeWrapper(VirtualKeyCode);

impl Serialize for VirtualKeyCodeWrapper {
    fn serialize<S>(&self, serializer: S)
                    -> result::Result<S::Ok, S::Error> where S: Serializer {
        VirtualKeyCodeD::serialize(&self.0, serializer)
    }
}

#[derive(Serialize)]
#[serde(remote = "VirtualKeyCode")]
enum VirtualKeyCodeD {
    Key1,
    Key2,
    Key3,
    Key4,
    Key5,
    Key6,
    Key7,
    Key8,
    Key9,
    Key0,
    A,
    B,
    C,
    D,
    E,
    F,
    G,
    H,
    I,
    J,
    K,
    L,
    M,
    N,
    O,
    P,
    Q,
    R,
    S,
    T,
    U,
    V,
    W,
    X,
    Y,
    Z,
    Escape,
    F1,
    F2,
    F3,
    F4,
    F5,
    F6,
    F7,
    F8,
    F9,
    F10,
    F11,
    F12,
    F13,
    F14,
    F15,
    Snapshot,
    Scroll,
    Pause,
    Insert,
    Home,
    Delete,
    End,
    PageDown,
    PageUp,
    Left,
    Up,
    Right,
    Down,
    Back,
    Return,
    Space,
    Compose,
    Numlock,
    Numpad0,
    Numpad1,
    Numpad2,
    Numpad3,
    Numpad4,
    Numpad5,
    Numpad6,
    Numpad7,
    Numpad8,
    Numpad9,
    AbntC1,
    AbntC2,
    Add,
    Apostrophe,
    Apps,
    At,
    Ax,
    Backslash,
    Calculator,
    Capital,
    Colon,
    Comma,
    Convert,
    Decimal,
    Divide,
    Equals,
    Grave,
    Kana,
    Kanji,
    LAlt,
    LBracket,
    LControl,
    LMenu,
    LShift,
    LWin,
    Mail,
    MediaSelect,
    MediaStop,
    Minus,
    Multiply,
    Mute,
    MyComputer,
    NavigateForward,
    NavigateBackward,
    NextTrack,
    NoConvert,
    NumpadComma,
    NumpadEnter,
    NumpadEquals,
    OEM102,
    Period,
    PlayPause,
    Power,
    PrevTrack,
    RAlt,
    RBracket,
    RControl,
    RMenu,
    RShift,
    RWin,
    Semicolon,
    Slash,
    Sleep,
    Stop,
    Subtract,
    Sysrq,
    Tab,
    Underline,
    Unlabeled,
    VolumeDown,
    VolumeUp,
    Wake,
    WebBack,
    WebFavorites,
    WebForward,
    WebHome,
    WebRefresh,
    WebSearch,
    WebStop,
    Yen,
}
