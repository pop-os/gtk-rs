// This file was generated by gir (https://github.com/gtk-rs/gir)
// from gir-files (https://github.com/gtk-rs/gir-files)
// DO NOT EDIT

use crate::OutputStream;
use crate::PollableOutputStream;
use crate::Seekable;
use glib::object::Cast;
use glib::object::IsA;
use glib::signal::connect_raw;
use glib::signal::SignalHandlerId;
use glib::translate::*;
use std::boxed::Box as Box_;
use std::fmt;
use std::mem::transmute;

glib::wrapper! {
    pub struct MemoryOutputStream(Object<ffi::GMemoryOutputStream, ffi::GMemoryOutputStreamClass>) @extends OutputStream, @implements PollableOutputStream, Seekable;

    match fn {
        get_type => || ffi::g_memory_output_stream_get_type(),
    }
}

impl MemoryOutputStream {
    pub fn new_resizable() -> MemoryOutputStream {
        unsafe {
            OutputStream::from_glib_full(ffi::g_memory_output_stream_new_resizable()).unsafe_cast()
        }
    }
}

pub const NONE_MEMORY_OUTPUT_STREAM: Option<&MemoryOutputStream> = None;

pub trait MemoryOutputStreamExt: 'static {
    fn get_data_size(&self) -> usize;

    fn steal_as_bytes(&self) -> glib::Bytes;

    fn connect_property_data_size_notify<F: Fn(&Self) + 'static>(&self, f: F) -> SignalHandlerId;
}

impl<O: IsA<MemoryOutputStream>> MemoryOutputStreamExt for O {
    fn get_data_size(&self) -> usize {
        unsafe { ffi::g_memory_output_stream_get_data_size(self.as_ref().to_glib_none().0) }
    }

    fn steal_as_bytes(&self) -> glib::Bytes {
        unsafe {
            from_glib_full(ffi::g_memory_output_stream_steal_as_bytes(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn connect_property_data_size_notify<F: Fn(&Self) + 'static>(&self, f: F) -> SignalHandlerId {
        unsafe extern "C" fn notify_data_size_trampoline<P, F: Fn(&P) + 'static>(
            this: *mut ffi::GMemoryOutputStream,
            _param_spec: glib::ffi::gpointer,
            f: glib::ffi::gpointer,
        ) where
            P: IsA<MemoryOutputStream>,
        {
            let f: &F = &*(f as *const F);
            f(&MemoryOutputStream::from_glib_borrow(this).unsafe_cast_ref())
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"notify::data-size\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    notify_data_size_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }
}

impl fmt::Display for MemoryOutputStream {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("MemoryOutputStream")
    }
}
