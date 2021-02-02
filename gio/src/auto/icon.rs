// This file was generated by gir (https://github.com/gtk-rs/gir)
// from gir-files (https://github.com/gtk-rs/gir-files)
// DO NOT EDIT

use glib::object::IsA;
use glib::translate::*;
use std::fmt;
use std::ptr;

glib::wrapper! {
    pub struct Icon(Interface<ffi::GIcon>);

    match fn {
        get_type => || ffi::g_icon_get_type(),
    }
}

impl Icon {
    pub fn deserialize(value: &glib::Variant) -> Option<Icon> {
        unsafe { from_glib_full(ffi::g_icon_deserialize(value.to_glib_none().0)) }
    }

    pub fn hash(&self) -> u32 {
        unsafe {
            ffi::g_icon_hash(
                ToGlibPtr::<*mut ffi::GIcon>::to_glib_none(self).0 as glib::ffi::gconstpointer,
            )
        }
    }

    pub fn new_for_string(str: &str) -> Result<Icon, glib::Error> {
        unsafe {
            let mut error = ptr::null_mut();
            let ret = ffi::g_icon_new_for_string(str.to_glib_none().0, &mut error);
            if error.is_null() {
                Ok(from_glib_full(ret))
            } else {
                Err(from_glib_full(error))
            }
        }
    }
}

pub const NONE_ICON: Option<&Icon> = None;

pub trait IconExt: 'static {
    fn equal<P: IsA<Icon>>(&self, icon2: Option<&P>) -> bool;

    fn serialize(&self) -> Option<glib::Variant>;

    fn to_string(&self) -> Option<glib::GString>;
}

impl<O: IsA<Icon>> IconExt for O {
    fn equal<P: IsA<Icon>>(&self, icon2: Option<&P>) -> bool {
        unsafe {
            from_glib(ffi::g_icon_equal(
                self.as_ref().to_glib_none().0,
                icon2.map(|p| p.as_ref()).to_glib_none().0,
            ))
        }
    }

    fn serialize(&self) -> Option<glib::Variant> {
        unsafe { from_glib_full(ffi::g_icon_serialize(self.as_ref().to_glib_none().0)) }
    }

    fn to_string(&self) -> Option<glib::GString> {
        unsafe { from_glib_full(ffi::g_icon_to_string(self.as_ref().to_glib_none().0)) }
    }
}

impl fmt::Display for Icon {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("Icon")
    }
}
