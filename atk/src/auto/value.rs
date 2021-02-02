// This file was generated by gir (https://github.com/gtk-rs/gir)
// from gir-files (https://github.com/gtk-rs/gir-files)
// DO NOT EDIT

use crate::Range;
use glib::object::Cast;
use glib::object::IsA;
use glib::signal::connect_raw;
use glib::signal::SignalHandlerId;
use glib::translate::*;
use std::boxed::Box as Box_;
use std::fmt;
use std::mem;
use std::mem::transmute;
use std::ptr;

glib::wrapper! {
    pub struct Value(Interface<ffi::AtkValue>);

    match fn {
        get_type => || ffi::atk_value_get_type(),
    }
}

pub const NONE_VALUE: Option<&Value> = None;

pub trait ValueExt: 'static {
    fn get_current_value(&self) -> glib::Value;

    fn get_increment(&self) -> f64;

    fn get_maximum_value(&self) -> glib::Value;

    fn get_minimum_increment(&self) -> glib::Value;

    fn get_minimum_value(&self) -> glib::Value;

    fn get_range(&self) -> Option<Range>;

    fn get_sub_ranges(&self) -> Vec<Range>;

    fn get_value_and_text(&self) -> (f64, glib::GString);

    fn set_current_value(&self, value: &glib::Value) -> bool;

    fn set_value(&self, new_value: f64);

    fn connect_value_changed<F: Fn(&Self, f64, &str) + 'static>(&self, f: F) -> SignalHandlerId;
}

impl<O: IsA<Value>> ValueExt for O {
    fn get_current_value(&self) -> glib::Value {
        unsafe {
            let mut value = glib::Value::uninitialized();
            ffi::atk_value_get_current_value(
                self.as_ref().to_glib_none().0,
                value.to_glib_none_mut().0,
            );
            value
        }
    }

    fn get_increment(&self) -> f64 {
        unsafe { ffi::atk_value_get_increment(self.as_ref().to_glib_none().0) }
    }

    fn get_maximum_value(&self) -> glib::Value {
        unsafe {
            let mut value = glib::Value::uninitialized();
            ffi::atk_value_get_maximum_value(
                self.as_ref().to_glib_none().0,
                value.to_glib_none_mut().0,
            );
            value
        }
    }

    fn get_minimum_increment(&self) -> glib::Value {
        unsafe {
            let mut value = glib::Value::uninitialized();
            ffi::atk_value_get_minimum_increment(
                self.as_ref().to_glib_none().0,
                value.to_glib_none_mut().0,
            );
            value
        }
    }

    fn get_minimum_value(&self) -> glib::Value {
        unsafe {
            let mut value = glib::Value::uninitialized();
            ffi::atk_value_get_minimum_value(
                self.as_ref().to_glib_none().0,
                value.to_glib_none_mut().0,
            );
            value
        }
    }

    fn get_range(&self) -> Option<Range> {
        unsafe { from_glib_full(ffi::atk_value_get_range(self.as_ref().to_glib_none().0)) }
    }

    fn get_sub_ranges(&self) -> Vec<Range> {
        unsafe {
            FromGlibPtrContainer::from_glib_full(ffi::atk_value_get_sub_ranges(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn get_value_and_text(&self) -> (f64, glib::GString) {
        unsafe {
            let mut value = mem::MaybeUninit::uninit();
            let mut text = ptr::null_mut();
            ffi::atk_value_get_value_and_text(
                self.as_ref().to_glib_none().0,
                value.as_mut_ptr(),
                &mut text,
            );
            let value = value.assume_init();
            (value, from_glib_full(text))
        }
    }

    fn set_current_value(&self, value: &glib::Value) -> bool {
        unsafe {
            from_glib(ffi::atk_value_set_current_value(
                self.as_ref().to_glib_none().0,
                value.to_glib_none().0,
            ))
        }
    }

    fn set_value(&self, new_value: f64) {
        unsafe {
            ffi::atk_value_set_value(self.as_ref().to_glib_none().0, new_value);
        }
    }

    fn connect_value_changed<F: Fn(&Self, f64, &str) + 'static>(&self, f: F) -> SignalHandlerId {
        unsafe extern "C" fn value_changed_trampoline<P, F: Fn(&P, f64, &str) + 'static>(
            this: *mut ffi::AtkValue,
            value: libc::c_double,
            text: *mut libc::c_char,
            f: glib::ffi::gpointer,
        ) where
            P: IsA<Value>,
        {
            let f: &F = &*(f as *const F);
            f(
                &Value::from_glib_borrow(this).unsafe_cast_ref(),
                value,
                &glib::GString::from_glib_borrow(text),
            )
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"value-changed\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    value_changed_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("Value")
    }
}
