// This file was generated by gir (https://github.com/gtk-rs/gir)
// from gir-files (https://github.com/gtk-rs/gir-files)
// DO NOT EDIT

use crate::SocketConnection;
use crate::SocketListener;
use glib::object::Cast;
use glib::object::IsA;
use glib::signal::connect_raw;
use glib::signal::SignalHandlerId;
use glib::translate::*;
#[cfg(any(feature = "v2_46", feature = "dox"))]
#[cfg_attr(feature = "dox", doc(cfg(feature = "v2_46")))]
use glib::StaticType;
use std::boxed::Box as Box_;
use std::fmt;
use std::mem::transmute;

glib::wrapper! {
    pub struct SocketService(Object<ffi::GSocketService, ffi::GSocketServiceClass>) @extends SocketListener;

    match fn {
        get_type => || ffi::g_socket_service_get_type(),
    }
}

impl SocketService {
    pub fn new() -> SocketService {
        unsafe { from_glib_full(ffi::g_socket_service_new()) }
    }
}

impl Default for SocketService {
    fn default() -> Self {
        Self::new()
    }
}

pub const NONE_SOCKET_SERVICE: Option<&SocketService> = None;

pub trait SocketServiceExt: 'static {
    fn is_active(&self) -> bool;

    fn start(&self);

    fn stop(&self);

    #[cfg(any(feature = "v2_46", feature = "dox"))]
    #[cfg_attr(feature = "dox", doc(cfg(feature = "v2_46")))]
    fn get_property_active(&self) -> bool;

    #[cfg(any(feature = "v2_46", feature = "dox"))]
    #[cfg_attr(feature = "dox", doc(cfg(feature = "v2_46")))]
    fn set_property_active(&self, active: bool);

    fn connect_incoming<F: Fn(&Self, &SocketConnection, Option<&glib::Object>) -> bool + 'static>(
        &self,
        f: F,
    ) -> SignalHandlerId;

    #[cfg(any(feature = "v2_46", feature = "dox"))]
    #[cfg_attr(feature = "dox", doc(cfg(feature = "v2_46")))]
    fn connect_property_active_notify<F: Fn(&Self) + 'static>(&self, f: F) -> SignalHandlerId;
}

impl<O: IsA<SocketService>> SocketServiceExt for O {
    fn is_active(&self) -> bool {
        unsafe {
            from_glib(ffi::g_socket_service_is_active(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn start(&self) {
        unsafe {
            ffi::g_socket_service_start(self.as_ref().to_glib_none().0);
        }
    }

    fn stop(&self) {
        unsafe {
            ffi::g_socket_service_stop(self.as_ref().to_glib_none().0);
        }
    }

    #[cfg(any(feature = "v2_46", feature = "dox"))]
    #[cfg_attr(feature = "dox", doc(cfg(feature = "v2_46")))]
    fn get_property_active(&self) -> bool {
        unsafe {
            let mut value = glib::Value::from_type(<bool as StaticType>::static_type());
            glib::gobject_ffi::g_object_get_property(
                self.to_glib_none().0 as *mut glib::gobject_ffi::GObject,
                b"active\0".as_ptr() as *const _,
                value.to_glib_none_mut().0,
            );
            value
                .get()
                .expect("Return Value for property `active` getter")
                .unwrap()
        }
    }

    #[cfg(any(feature = "v2_46", feature = "dox"))]
    #[cfg_attr(feature = "dox", doc(cfg(feature = "v2_46")))]
    fn set_property_active(&self, active: bool) {
        unsafe {
            glib::gobject_ffi::g_object_set_property(
                self.to_glib_none().0 as *mut glib::gobject_ffi::GObject,
                b"active\0".as_ptr() as *const _,
                glib::Value::from(&active).to_glib_none().0,
            );
        }
    }

    fn connect_incoming<
        F: Fn(&Self, &SocketConnection, Option<&glib::Object>) -> bool + 'static,
    >(
        &self,
        f: F,
    ) -> SignalHandlerId {
        unsafe extern "C" fn incoming_trampoline<
            P,
            F: Fn(&P, &SocketConnection, Option<&glib::Object>) -> bool + 'static,
        >(
            this: *mut ffi::GSocketService,
            connection: *mut ffi::GSocketConnection,
            source_object: *mut glib::gobject_ffi::GObject,
            f: glib::ffi::gpointer,
        ) -> glib::ffi::gboolean
        where
            P: IsA<SocketService>,
        {
            let f: &F = &*(f as *const F);
            f(
                &SocketService::from_glib_borrow(this).unsafe_cast_ref(),
                &from_glib_borrow(connection),
                Option::<glib::Object>::from_glib_borrow(source_object)
                    .as_ref()
                    .as_ref(),
            )
            .to_glib()
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"incoming\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    incoming_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }

    #[cfg(any(feature = "v2_46", feature = "dox"))]
    #[cfg_attr(feature = "dox", doc(cfg(feature = "v2_46")))]
    fn connect_property_active_notify<F: Fn(&Self) + 'static>(&self, f: F) -> SignalHandlerId {
        unsafe extern "C" fn notify_active_trampoline<P, F: Fn(&P) + 'static>(
            this: *mut ffi::GSocketService,
            _param_spec: glib::ffi::gpointer,
            f: glib::ffi::gpointer,
        ) where
            P: IsA<SocketService>,
        {
            let f: &F = &*(f as *const F);
            f(&SocketService::from_glib_borrow(this).unsafe_cast_ref())
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"notify::active\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    notify_active_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }
}

impl fmt::Display for SocketService {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("SocketService")
    }
}
