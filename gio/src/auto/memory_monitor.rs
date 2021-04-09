// This file was generated by gir (https://github.com/gtk-rs/gir)
// from gir-files (https://github.com/gtk-rs/gir-files)
// DO NOT EDIT

use crate::MemoryMonitorWarningLevel;
use glib::object::Cast;
use glib::object::IsA;
use glib::signal::connect_raw;
use glib::signal::SignalHandlerId;
use glib::translate::*;
use std::boxed::Box as Box_;
use std::fmt;
use std::mem::transmute;

glib::wrapper! {
    pub struct MemoryMonitor(Interface<ffi::GMemoryMonitor, ffi::GMemoryMonitorInterface>);

    match fn {
        get_type => || ffi::g_memory_monitor_get_type(),
    }
}

impl MemoryMonitor {
    pub fn dup_default() -> MemoryMonitor {
        unsafe { from_glib_full(ffi::g_memory_monitor_dup_default()) }
    }
}

pub const NONE_MEMORY_MONITOR: Option<&MemoryMonitor> = None;

pub trait MemoryMonitorExt: 'static {
    #[cfg(any(feature = "v2_64", feature = "dox"))]
    #[cfg_attr(feature = "dox", doc(cfg(feature = "v2_64")))]
    fn connect_low_memory_warning<F: Fn(&Self, MemoryMonitorWarningLevel) + 'static>(
        &self,
        f: F,
    ) -> SignalHandlerId;
}

impl<O: IsA<MemoryMonitor>> MemoryMonitorExt for O {
    #[cfg(any(feature = "v2_64", feature = "dox"))]
    #[cfg_attr(feature = "dox", doc(cfg(feature = "v2_64")))]
    fn connect_low_memory_warning<F: Fn(&Self, MemoryMonitorWarningLevel) + 'static>(
        &self,
        f: F,
    ) -> SignalHandlerId {
        unsafe extern "C" fn low_memory_warning_trampoline<
            P,
            F: Fn(&P, MemoryMonitorWarningLevel) + 'static,
        >(
            this: *mut ffi::GMemoryMonitor,
            level: ffi::GMemoryMonitorWarningLevel,
            f: glib::ffi::gpointer,
        ) where
            P: IsA<MemoryMonitor>,
        {
            let f: &F = &*(f as *const F);
            f(
                &MemoryMonitor::from_glib_borrow(this).unsafe_cast_ref(),
                from_glib(level),
            )
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"low-memory-warning\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    low_memory_warning_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }
}

impl fmt::Display for MemoryMonitor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("MemoryMonitor")
    }
}
