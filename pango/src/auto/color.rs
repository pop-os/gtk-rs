// This file was generated by gir (https://github.com/gtk-rs/gir)
// from gir-files (https://github.com/gtk-rs/gir-files)
// DO NOT EDIT

use glib::translate::*;
use std::fmt;
#[cfg(any(feature = "v1_46", feature = "dox"))]
#[cfg_attr(feature = "dox", doc(cfg(feature = "v1_46")))]
use std::mem;

glib::wrapper! {
    #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Color(Boxed<ffi::PangoColor>);

    match fn {
        copy => |ptr| ffi::pango_color_copy(mut_override(ptr)),
        free => |ptr| ffi::pango_color_free(ptr),
        get_type => || ffi::pango_color_get_type(),
    }
}

impl Color {
    pub fn parse(&mut self, spec: &str) -> bool {
        unsafe {
            from_glib(ffi::pango_color_parse(
                self.to_glib_none_mut().0,
                spec.to_glib_none().0,
            ))
        }
    }

    #[cfg(any(feature = "v1_46", feature = "dox"))]
    #[cfg_attr(feature = "dox", doc(cfg(feature = "v1_46")))]
    pub fn parse_with_alpha(&mut self, spec: &str) -> Option<u16> {
        unsafe {
            let mut alpha = mem::MaybeUninit::uninit();
            let ret = from_glib(ffi::pango_color_parse_with_alpha(
                self.to_glib_none_mut().0,
                alpha.as_mut_ptr(),
                spec.to_glib_none().0,
            ));
            let alpha = alpha.assume_init();
            if ret {
                Some(alpha)
            } else {
                None
            }
        }
    }

    pub fn to_str(&self) -> glib::GString {
        unsafe { from_glib_full(ffi::pango_color_to_string(self.to_glib_none().0)) }
    }
}

impl fmt::Display for Color {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&self.to_str())
    }
}
