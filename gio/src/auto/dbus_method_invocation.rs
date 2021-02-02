// This file was generated by gir (https://github.com/gtk-rs/gir)
// from gir-files (https://github.com/gtk-rs/gir-files)
// DO NOT EDIT

use crate::DBusConnection;
use crate::DBusMessage;
use crate::DBusMethodInfo;
use crate::DBusPropertyInfo;
#[cfg(any(unix, feature = "dox"))]
#[cfg_attr(feature = "dox", doc(cfg(unix)))]
use crate::UnixFDList;
#[cfg(any(unix, feature = "dox"))]
#[cfg_attr(feature = "dox", doc(cfg(unix)))]
use glib::object::IsA;
use glib::translate::*;
use std::fmt;

glib::wrapper! {
    pub struct DBusMethodInvocation(Object<ffi::GDBusMethodInvocation>);

    match fn {
        get_type => || ffi::g_dbus_method_invocation_get_type(),
    }
}

impl DBusMethodInvocation {
    #[doc(alias = "g_dbus_method_invocation_get_connection")]
    pub fn get_connection(&self) -> Option<DBusConnection> {
        unsafe {
            from_glib_none(ffi::g_dbus_method_invocation_get_connection(
                self.to_glib_none().0,
            ))
        }
    }

    #[doc(alias = "g_dbus_method_invocation_get_interface_name")]
    pub fn get_interface_name(&self) -> Option<glib::GString> {
        unsafe {
            from_glib_none(ffi::g_dbus_method_invocation_get_interface_name(
                self.to_glib_none().0,
            ))
        }
    }

    #[doc(alias = "g_dbus_method_invocation_get_message")]
    pub fn get_message(&self) -> Option<DBusMessage> {
        unsafe {
            from_glib_none(ffi::g_dbus_method_invocation_get_message(
                self.to_glib_none().0,
            ))
        }
    }

    #[doc(alias = "g_dbus_method_invocation_get_method_info")]
    pub fn get_method_info(&self) -> Option<DBusMethodInfo> {
        unsafe {
            from_glib_none(ffi::g_dbus_method_invocation_get_method_info(
                self.to_glib_none().0,
            ))
        }
    }

    #[doc(alias = "g_dbus_method_invocation_get_method_name")]
    pub fn get_method_name(&self) -> Option<glib::GString> {
        unsafe {
            from_glib_none(ffi::g_dbus_method_invocation_get_method_name(
                self.to_glib_none().0,
            ))
        }
    }

    #[doc(alias = "g_dbus_method_invocation_get_object_path")]
    pub fn get_object_path(&self) -> Option<glib::GString> {
        unsafe {
            from_glib_none(ffi::g_dbus_method_invocation_get_object_path(
                self.to_glib_none().0,
            ))
        }
    }

    #[doc(alias = "g_dbus_method_invocation_get_parameters")]
    pub fn get_parameters(&self) -> Option<glib::Variant> {
        unsafe {
            from_glib_none(ffi::g_dbus_method_invocation_get_parameters(
                self.to_glib_none().0,
            ))
        }
    }

    #[doc(alias = "g_dbus_method_invocation_get_property_info")]
    pub fn get_property_info(&self) -> Option<DBusPropertyInfo> {
        unsafe {
            from_glib_none(ffi::g_dbus_method_invocation_get_property_info(
                self.to_glib_none().0,
            ))
        }
    }

    #[doc(alias = "g_dbus_method_invocation_get_sender")]
    pub fn get_sender(&self) -> Option<glib::GString> {
        unsafe {
            from_glib_none(ffi::g_dbus_method_invocation_get_sender(
                self.to_glib_none().0,
            ))
        }
    }

    //#[doc(alias = "g_dbus_method_invocation_get_user_data")]
    //pub fn get_user_data(&self) -> /*Unimplemented*/Option<Fundamental: Pointer> {
    //    unsafe { TODO: call ffi:g_dbus_method_invocation_get_user_data() }
    //}

    #[doc(alias = "g_dbus_method_invocation_return_dbus_error")]
    pub fn return_dbus_error(&self, error_name: &str, error_message: &str) {
        unsafe {
            ffi::g_dbus_method_invocation_return_dbus_error(
                self.to_glib_full(),
                error_name.to_glib_none().0,
                error_message.to_glib_none().0,
            );
        }
    }

    //#[doc(alias = "g_dbus_method_invocation_return_error")]
    //pub fn return_error(&self, domain: glib::Quark, code: i32, format: &str, : /*Unknown conversion*//*Unimplemented*/Fundamental: VarArgs) {
    //    unsafe { TODO: call ffi:g_dbus_method_invocation_return_error() }
    //}

    //#[doc(alias = "g_dbus_method_invocation_return_error_valist")]
    //pub fn return_error_valist(&self, domain: glib::Quark, code: i32, format: &str, var_args: /*Unknown conversion*//*Unimplemented*/Unsupported) {
    //    unsafe { TODO: call ffi:g_dbus_method_invocation_return_error_valist() }
    //}

    #[doc(alias = "g_dbus_method_invocation_return_value")]
    pub fn return_value(&self, parameters: Option<&glib::Variant>) {
        unsafe {
            ffi::g_dbus_method_invocation_return_value(
                self.to_glib_full(),
                parameters.to_glib_none().0,
            );
        }
    }

    #[cfg(any(unix, feature = "dox"))]
    #[cfg_attr(feature = "dox", doc(cfg(unix)))]
    #[doc(alias = "g_dbus_method_invocation_return_value_with_unix_fd_list")]
    pub fn return_value_with_unix_fd_list<P: IsA<UnixFDList>>(
        &self,
        parameters: Option<&glib::Variant>,
        fd_list: Option<&P>,
    ) {
        unsafe {
            ffi::g_dbus_method_invocation_return_value_with_unix_fd_list(
                self.to_glib_full(),
                parameters.to_glib_none().0,
                fd_list.map(|p| p.as_ref()).to_glib_none().0,
            );
        }
    }
}

impl fmt::Display for DBusMethodInvocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("DBusMethodInvocation")
    }
}
