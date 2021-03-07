// This file was generated by gir (https://github.com/gtk-rs/gir)
// from gir-files (https://github.com/gtk-rs/gir-files)
// DO NOT EDIT

use crate::SocketAddressEnumerator;
use glib::object::IsA;
use glib::translate::*;
use std::fmt;

glib::wrapper! {
    pub struct SocketConnectable(Interface<ffi::GSocketConnectable>);

    match fn {
        get_type => || ffi::g_socket_connectable_get_type(),
    }
}

pub const NONE_SOCKET_CONNECTABLE: Option<&SocketConnectable> = None;

pub trait SocketConnectableExt: 'static {
    fn enumerate(&self) -> Option<SocketAddressEnumerator>;

    fn proxy_enumerate(&self) -> Option<SocketAddressEnumerator>;

    #[cfg(any(feature = "v2_48", feature = "dox"))]
    #[cfg_attr(feature = "dox", doc(cfg(feature = "v2_48")))]
    fn to_string(&self) -> Option<glib::GString>;
}

impl<O: IsA<SocketConnectable>> SocketConnectableExt for O {
    fn enumerate(&self) -> Option<SocketAddressEnumerator> {
        unsafe {
            from_glib_full(ffi::g_socket_connectable_enumerate(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn proxy_enumerate(&self) -> Option<SocketAddressEnumerator> {
        unsafe {
            from_glib_full(ffi::g_socket_connectable_proxy_enumerate(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    #[cfg(any(feature = "v2_48", feature = "dox"))]
    #[cfg_attr(feature = "dox", doc(cfg(feature = "v2_48")))]
    fn to_string(&self) -> Option<glib::GString> {
        unsafe {
            from_glib_full(ffi::g_socket_connectable_to_string(
                self.as_ref().to_glib_none().0,
            ))
        }
    }
}

impl fmt::Display for SocketConnectable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("SocketConnectable")
    }
}
