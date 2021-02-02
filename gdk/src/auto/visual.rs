// This file was generated by gir (https://github.com/gtk-rs/gir)
// from gir-files (https://github.com/gtk-rs/gir-files)
// DO NOT EDIT

use crate::ByteOrder;
use crate::Screen;
use crate::VisualType;
use glib::translate::*;
use std::fmt;
use std::mem;

glib::wrapper! {
    pub struct Visual(Object<ffi::GdkVisual>);

    match fn {
        get_type => || ffi::gdk_visual_get_type(),
    }
}

impl Visual {
    #[cfg_attr(feature = "v3_22", deprecated)]
    #[doc(alias = "gdk_visual_get_bits_per_rgb")]
    pub fn get_bits_per_rgb(&self) -> i32 {
        unsafe { ffi::gdk_visual_get_bits_per_rgb(self.to_glib_none().0) }
    }

    #[doc(alias = "gdk_visual_get_blue_pixel_details")]
    pub fn get_blue_pixel_details(&self) -> (u32, i32, i32) {
        unsafe {
            let mut mask = mem::MaybeUninit::uninit();
            let mut shift = mem::MaybeUninit::uninit();
            let mut precision = mem::MaybeUninit::uninit();
            ffi::gdk_visual_get_blue_pixel_details(
                self.to_glib_none().0,
                mask.as_mut_ptr(),
                shift.as_mut_ptr(),
                precision.as_mut_ptr(),
            );
            let mask = mask.assume_init();
            let shift = shift.assume_init();
            let precision = precision.assume_init();
            (mask, shift, precision)
        }
    }

    #[cfg_attr(feature = "v3_22", deprecated)]
    #[doc(alias = "gdk_visual_get_byte_order")]
    pub fn get_byte_order(&self) -> ByteOrder {
        unsafe { from_glib(ffi::gdk_visual_get_byte_order(self.to_glib_none().0)) }
    }

    #[cfg_attr(feature = "v3_22", deprecated)]
    #[doc(alias = "gdk_visual_get_colormap_size")]
    pub fn get_colormap_size(&self) -> i32 {
        unsafe { ffi::gdk_visual_get_colormap_size(self.to_glib_none().0) }
    }

    #[doc(alias = "gdk_visual_get_depth")]
    pub fn get_depth(&self) -> i32 {
        unsafe { ffi::gdk_visual_get_depth(self.to_glib_none().0) }
    }

    #[doc(alias = "gdk_visual_get_green_pixel_details")]
    pub fn get_green_pixel_details(&self) -> (u32, i32, i32) {
        unsafe {
            let mut mask = mem::MaybeUninit::uninit();
            let mut shift = mem::MaybeUninit::uninit();
            let mut precision = mem::MaybeUninit::uninit();
            ffi::gdk_visual_get_green_pixel_details(
                self.to_glib_none().0,
                mask.as_mut_ptr(),
                shift.as_mut_ptr(),
                precision.as_mut_ptr(),
            );
            let mask = mask.assume_init();
            let shift = shift.assume_init();
            let precision = precision.assume_init();
            (mask, shift, precision)
        }
    }

    #[doc(alias = "gdk_visual_get_red_pixel_details")]
    pub fn get_red_pixel_details(&self) -> (u32, i32, i32) {
        unsafe {
            let mut mask = mem::MaybeUninit::uninit();
            let mut shift = mem::MaybeUninit::uninit();
            let mut precision = mem::MaybeUninit::uninit();
            ffi::gdk_visual_get_red_pixel_details(
                self.to_glib_none().0,
                mask.as_mut_ptr(),
                shift.as_mut_ptr(),
                precision.as_mut_ptr(),
            );
            let mask = mask.assume_init();
            let shift = shift.assume_init();
            let precision = precision.assume_init();
            (mask, shift, precision)
        }
    }

    #[doc(alias = "gdk_visual_get_screen")]
    pub fn get_screen(&self) -> Screen {
        unsafe { from_glib_none(ffi::gdk_visual_get_screen(self.to_glib_none().0)) }
    }

    #[doc(alias = "gdk_visual_get_visual_type")]
    pub fn get_visual_type(&self) -> VisualType {
        unsafe { from_glib(ffi::gdk_visual_get_visual_type(self.to_glib_none().0)) }
    }

    #[cfg_attr(feature = "v3_22", deprecated)]
    #[doc(alias = "gdk_visual_get_best")]
    pub fn get_best() -> Visual {
        assert_initialized_main_thread!();
        unsafe { from_glib_none(ffi::gdk_visual_get_best()) }
    }

    #[cfg_attr(feature = "v3_22", deprecated)]
    #[doc(alias = "gdk_visual_get_best_depth")]
    pub fn get_best_depth() -> i32 {
        assert_initialized_main_thread!();
        unsafe { ffi::gdk_visual_get_best_depth() }
    }

    #[cfg_attr(feature = "v3_22", deprecated)]
    #[doc(alias = "gdk_visual_get_best_type")]
    pub fn get_best_type() -> VisualType {
        assert_initialized_main_thread!();
        unsafe { from_glib(ffi::gdk_visual_get_best_type()) }
    }

    #[cfg_attr(feature = "v3_22", deprecated)]
    #[doc(alias = "gdk_visual_get_best_with_both")]
    pub fn get_best_with_both(depth: i32, visual_type: VisualType) -> Option<Visual> {
        assert_initialized_main_thread!();
        unsafe {
            from_glib_none(ffi::gdk_visual_get_best_with_both(
                depth,
                visual_type.to_glib(),
            ))
        }
    }

    #[cfg_attr(feature = "v3_22", deprecated)]
    #[doc(alias = "gdk_visual_get_best_with_depth")]
    pub fn get_best_with_depth(depth: i32) -> Option<Visual> {
        assert_initialized_main_thread!();
        unsafe { from_glib_none(ffi::gdk_visual_get_best_with_depth(depth)) }
    }

    #[cfg_attr(feature = "v3_22", deprecated)]
    #[doc(alias = "gdk_visual_get_best_with_type")]
    pub fn get_best_with_type(visual_type: VisualType) -> Option<Visual> {
        assert_initialized_main_thread!();
        unsafe { from_glib_none(ffi::gdk_visual_get_best_with_type(visual_type.to_glib())) }
    }

    #[cfg_attr(feature = "v3_22", deprecated)]
    #[doc(alias = "gdk_visual_get_system")]
    pub fn get_system() -> Visual {
        assert_initialized_main_thread!();
        unsafe { from_glib_none(ffi::gdk_visual_get_system()) }
    }
}

impl fmt::Display for Visual {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("Visual")
    }
}
