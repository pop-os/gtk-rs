// This file was generated by gir (https://github.com/gtk-rs/gir)
// from gir-files (https://github.com/gtk-rs/gir-files)
// DO NOT EDIT

use crate::IOStream;
use crate::SocketConnectable;
use crate::TlsCertificateFlags;
use crate::TlsConnection;
use glib::object::Cast;
use glib::object::IsA;
use glib::signal::connect_raw;
use glib::signal::SignalHandlerId;
use glib::translate::*;
use std::boxed::Box as Box_;
use std::fmt;
use std::mem::transmute;
use std::ptr;

glib::wrapper! {
    pub struct TlsClientConnection(Interface<ffi::GTlsClientConnection, ffi::GTlsClientConnectionInterface>) @requires TlsConnection, IOStream;

    match fn {
        get_type => || ffi::g_tls_client_connection_get_type(),
    }
}

impl TlsClientConnection {
    pub fn new<P: IsA<IOStream>, Q: IsA<SocketConnectable>>(
        base_io_stream: &P,
        server_identity: Option<&Q>,
    ) -> Result<TlsClientConnection, glib::Error> {
        unsafe {
            let mut error = ptr::null_mut();
            let ret = ffi::g_tls_client_connection_new(
                base_io_stream.as_ref().to_glib_none().0,
                server_identity.map(|p| p.as_ref()).to_glib_none().0,
                &mut error,
            );
            if error.is_null() {
                Ok(from_glib_full(ret))
            } else {
                Err(from_glib_full(error))
            }
        }
    }
}

pub const NONE_TLS_CLIENT_CONNECTION: Option<&TlsClientConnection> = None;

pub trait TlsClientConnectionExt: 'static {
    #[cfg(any(feature = "v2_46", feature = "dox"))]
    #[cfg_attr(feature = "dox", doc(cfg(feature = "v2_46")))]
    fn copy_session_state<P: IsA<TlsClientConnection>>(&self, source: &P);

    fn get_accepted_cas(&self) -> Vec<glib::ByteArray>;

    fn get_server_identity(&self) -> Option<SocketConnectable>;

    #[cfg_attr(feature = "v2_56", deprecated)]
    fn get_use_ssl3(&self) -> bool;

    fn get_validation_flags(&self) -> TlsCertificateFlags;

    fn set_server_identity<P: IsA<SocketConnectable>>(&self, identity: &P);

    #[cfg_attr(feature = "v2_56", deprecated)]
    fn set_use_ssl3(&self, use_ssl3: bool);

    fn set_validation_flags(&self, flags: TlsCertificateFlags);

    fn connect_property_accepted_cas_notify<F: Fn(&Self) + 'static>(&self, f: F)
        -> SignalHandlerId;

    fn connect_property_server_identity_notify<F: Fn(&Self) + 'static>(
        &self,
        f: F,
    ) -> SignalHandlerId;

    #[cfg_attr(feature = "v2_56", deprecated)]
    fn connect_property_use_ssl3_notify<F: Fn(&Self) + 'static>(&self, f: F) -> SignalHandlerId;

    fn connect_property_validation_flags_notify<F: Fn(&Self) + 'static>(
        &self,
        f: F,
    ) -> SignalHandlerId;
}

impl<O: IsA<TlsClientConnection>> TlsClientConnectionExt for O {
    #[cfg(any(feature = "v2_46", feature = "dox"))]
    #[cfg_attr(feature = "dox", doc(cfg(feature = "v2_46")))]
    fn copy_session_state<P: IsA<TlsClientConnection>>(&self, source: &P) {
        unsafe {
            ffi::g_tls_client_connection_copy_session_state(
                self.as_ref().to_glib_none().0,
                source.as_ref().to_glib_none().0,
            );
        }
    }

    fn get_accepted_cas(&self) -> Vec<glib::ByteArray> {
        unsafe {
            FromGlibPtrContainer::from_glib_full(ffi::g_tls_client_connection_get_accepted_cas(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn get_server_identity(&self) -> Option<SocketConnectable> {
        unsafe {
            from_glib_none(ffi::g_tls_client_connection_get_server_identity(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn get_use_ssl3(&self) -> bool {
        unsafe {
            from_glib(ffi::g_tls_client_connection_get_use_ssl3(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn get_validation_flags(&self) -> TlsCertificateFlags {
        unsafe {
            from_glib(ffi::g_tls_client_connection_get_validation_flags(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn set_server_identity<P: IsA<SocketConnectable>>(&self, identity: &P) {
        unsafe {
            ffi::g_tls_client_connection_set_server_identity(
                self.as_ref().to_glib_none().0,
                identity.as_ref().to_glib_none().0,
            );
        }
    }

    fn set_use_ssl3(&self, use_ssl3: bool) {
        unsafe {
            ffi::g_tls_client_connection_set_use_ssl3(
                self.as_ref().to_glib_none().0,
                use_ssl3.to_glib(),
            );
        }
    }

    fn set_validation_flags(&self, flags: TlsCertificateFlags) {
        unsafe {
            ffi::g_tls_client_connection_set_validation_flags(
                self.as_ref().to_glib_none().0,
                flags.to_glib(),
            );
        }
    }

    fn connect_property_accepted_cas_notify<F: Fn(&Self) + 'static>(
        &self,
        f: F,
    ) -> SignalHandlerId {
        unsafe extern "C" fn notify_accepted_cas_trampoline<P, F: Fn(&P) + 'static>(
            this: *mut ffi::GTlsClientConnection,
            _param_spec: glib::ffi::gpointer,
            f: glib::ffi::gpointer,
        ) where
            P: IsA<TlsClientConnection>,
        {
            let f: &F = &*(f as *const F);
            f(&TlsClientConnection::from_glib_borrow(this).unsafe_cast_ref())
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"notify::accepted-cas\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    notify_accepted_cas_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }

    fn connect_property_server_identity_notify<F: Fn(&Self) + 'static>(
        &self,
        f: F,
    ) -> SignalHandlerId {
        unsafe extern "C" fn notify_server_identity_trampoline<P, F: Fn(&P) + 'static>(
            this: *mut ffi::GTlsClientConnection,
            _param_spec: glib::ffi::gpointer,
            f: glib::ffi::gpointer,
        ) where
            P: IsA<TlsClientConnection>,
        {
            let f: &F = &*(f as *const F);
            f(&TlsClientConnection::from_glib_borrow(this).unsafe_cast_ref())
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"notify::server-identity\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    notify_server_identity_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }

    fn connect_property_use_ssl3_notify<F: Fn(&Self) + 'static>(&self, f: F) -> SignalHandlerId {
        unsafe extern "C" fn notify_use_ssl3_trampoline<P, F: Fn(&P) + 'static>(
            this: *mut ffi::GTlsClientConnection,
            _param_spec: glib::ffi::gpointer,
            f: glib::ffi::gpointer,
        ) where
            P: IsA<TlsClientConnection>,
        {
            let f: &F = &*(f as *const F);
            f(&TlsClientConnection::from_glib_borrow(this).unsafe_cast_ref())
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"notify::use-ssl3\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    notify_use_ssl3_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }

    fn connect_property_validation_flags_notify<F: Fn(&Self) + 'static>(
        &self,
        f: F,
    ) -> SignalHandlerId {
        unsafe extern "C" fn notify_validation_flags_trampoline<P, F: Fn(&P) + 'static>(
            this: *mut ffi::GTlsClientConnection,
            _param_spec: glib::ffi::gpointer,
            f: glib::ffi::gpointer,
        ) where
            P: IsA<TlsClientConnection>,
        {
            let f: &F = &*(f as *const F);
            f(&TlsClientConnection::from_glib_borrow(this).unsafe_cast_ref())
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"notify::validation-flags\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    notify_validation_flags_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }
}

impl fmt::Display for TlsClientConnection {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("TlsClientConnection")
    }
}
