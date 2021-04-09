// This file was generated by gir (https://github.com/gtk-rs/gir)
// from gir-files (https://github.com/gtk-rs/gir-files)
// DO NOT EDIT

use crate::DBusConnection;
use crate::MenuModel;
use glib::translate::*;
use std::fmt;

glib::wrapper! {
    pub struct DBusMenuModel(Object<ffi::GDBusMenuModel>) @extends MenuModel;

    match fn {
        get_type => || ffi::g_dbus_menu_model_get_type(),
    }
}

impl DBusMenuModel {
    #[doc(alias = "g_dbus_menu_model_get")]
    pub fn get(
        connection: &DBusConnection,
        bus_name: Option<&str>,
        object_path: &str,
    ) -> DBusMenuModel {
        unsafe {
            from_glib_full(ffi::g_dbus_menu_model_get(
                connection.to_glib_none().0,
                bus_name.to_glib_none().0,
                object_path.to_glib_none().0,
            ))
        }
    }
}

impl fmt::Display for DBusMenuModel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("DBusMenuModel")
    }
}
