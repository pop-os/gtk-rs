// This file was generated by gir (https://github.com/gtk-rs/gir)
// from gir-files (https://github.com/gtk-rs/gir-files)
// DO NOT EDIT

use crate::DBusInterfaceInfo;
use crate::DBusObject;
use glib::object::IsA;
use glib::translate::*;
use std::fmt;

glib::wrapper! {
    pub struct DBusInterface(Interface<ffi::GDBusInterface>);

    match fn {
        get_type => || ffi::g_dbus_interface_get_type(),
    }
}

pub const NONE_DBUS_INTERFACE: Option<&DBusInterface> = None;

pub trait DBusInterfaceExt: 'static {
    fn get(&self) -> Option<DBusObject>;

    fn get_info(&self) -> Option<DBusInterfaceInfo>;

    fn set_object<P: IsA<DBusObject>>(&self, object: Option<&P>);
}

impl<O: IsA<DBusInterface>> DBusInterfaceExt for O {
    fn get(&self) -> Option<DBusObject> {
        unsafe {
            from_glib_full(ffi::g_dbus_interface_dup_object(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn get_info(&self) -> Option<DBusInterfaceInfo> {
        unsafe {
            from_glib_none(ffi::g_dbus_interface_get_info(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn set_object<P: IsA<DBusObject>>(&self, object: Option<&P>) {
        unsafe {
            ffi::g_dbus_interface_set_object(
                self.as_ref().to_glib_none().0,
                object.map(|p| p.as_ref()).to_glib_none().0,
            );
        }
    }
}

impl fmt::Display for DBusInterface {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("DBusInterface")
    }
}
