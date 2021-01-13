// This file was generated by gir (https://github.com/gtk-rs/gir)
// from gir-files (https://github.com/gtk-rs/gir-files)
// DO NOT EDIT

use crate::OutputStream;
use glib::object::Cast;
use glib::object::IsA;
use glib::signal::connect_raw;
use glib::signal::SignalHandlerId;
use glib::translate::*;
use std::boxed::Box as Box_;
use std::fmt;
use std::mem::transmute;

glib::wrapper! {
    pub struct FilterOutputStream(Object<ffi::GFilterOutputStream, ffi::GFilterOutputStreamClass>) @extends OutputStream;

    match fn {
        get_type => || ffi::g_filter_output_stream_get_type(),
    }
}

pub const NONE_FILTER_OUTPUT_STREAM: Option<&FilterOutputStream> = None;

pub trait FilterOutputStreamExt: 'static {
    fn get_base_stream(&self) -> Option<OutputStream>;

    fn get_close_base_stream(&self) -> bool;

    fn set_close_base_stream(&self, close_base: bool);

    fn connect_property_close_base_stream_notify<F: Fn(&Self) + 'static>(
        &self,
        f: F,
    ) -> SignalHandlerId;
}

impl<O: IsA<FilterOutputStream>> FilterOutputStreamExt for O {
    fn get_base_stream(&self) -> Option<OutputStream> {
        unsafe {
            from_glib_none(ffi::g_filter_output_stream_get_base_stream(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn get_close_base_stream(&self) -> bool {
        unsafe {
            from_glib(ffi::g_filter_output_stream_get_close_base_stream(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn set_close_base_stream(&self, close_base: bool) {
        unsafe {
            ffi::g_filter_output_stream_set_close_base_stream(
                self.as_ref().to_glib_none().0,
                close_base.to_glib(),
            );
        }
    }

    fn connect_property_close_base_stream_notify<F: Fn(&Self) + 'static>(
        &self,
        f: F,
    ) -> SignalHandlerId {
        unsafe extern "C" fn notify_close_base_stream_trampoline<P, F: Fn(&P) + 'static>(
            this: *mut ffi::GFilterOutputStream,
            _param_spec: glib::ffi::gpointer,
            f: glib::ffi::gpointer,
        ) where
            P: IsA<FilterOutputStream>,
        {
            let f: &F = &*(f as *const F);
            f(&FilterOutputStream::from_glib_borrow(this).unsafe_cast_ref())
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"notify::close-base-stream\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    notify_close_base_stream_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }
}

impl fmt::Display for FilterOutputStream {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("FilterOutputStream")
    }
}
