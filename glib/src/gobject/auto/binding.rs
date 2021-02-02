// This file was generated by gir (https://github.com/gtk-rs/gir)
// from gir-files (https://github.com/gtk-rs/gir-files)
// DO NOT EDIT

use crate::translate::*;
use crate::BindingFlags;
use std::fmt;

crate::wrapper! {
    pub struct Binding(Object<gobject_ffi::GBinding>);

    match fn {
        get_type => || gobject_ffi::g_binding_get_type(),
    }
}

impl Binding {
    pub fn get_flags(&self) -> BindingFlags {
        unsafe { from_glib(gobject_ffi::g_binding_get_flags(self.to_glib_none().0)) }
    }

    pub fn get_source_property(&self) -> crate::GString {
        unsafe {
            from_glib_none(gobject_ffi::g_binding_get_source_property(
                self.to_glib_none().0,
            ))
        }
    }

    pub fn get_target_property(&self) -> crate::GString {
        unsafe {
            from_glib_none(gobject_ffi::g_binding_get_target_property(
                self.to_glib_none().0,
            ))
        }
    }

    pub fn unbind(&self) {
        unsafe {
            gobject_ffi::g_binding_unbind(self.to_glib_none().0);
        }
    }
}

unsafe impl Send for Binding {}
unsafe impl Sync for Binding {}

impl fmt::Display for Binding {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("Binding")
    }
}
