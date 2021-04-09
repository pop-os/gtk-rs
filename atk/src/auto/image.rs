// This file was generated by gir (https://github.com/gtk-rs/gir)
// from gir-files (https://github.com/gtk-rs/gir-files)
// DO NOT EDIT

use crate::CoordType;
use glib::object::IsA;
use glib::translate::*;
use std::fmt;
use std::mem;

glib::wrapper! {
    pub struct Image(Interface<ffi::AtkImage, ffi::AtkImageIface>);

    match fn {
        get_type => || ffi::atk_image_get_type(),
    }
}

pub const NONE_IMAGE: Option<&Image> = None;

pub trait AtkImageExt: 'static {
    fn get_image_description(&self) -> Option<glib::GString>;

    fn get_image_locale(&self) -> Option<glib::GString>;

    fn get_image_position(&self, coord_type: CoordType) -> (i32, i32);

    fn get_image_size(&self) -> (i32, i32);

    fn set_image_description(&self, description: &str) -> bool;
}

impl<O: IsA<Image>> AtkImageExt for O {
    fn get_image_description(&self) -> Option<glib::GString> {
        unsafe {
            from_glib_none(ffi::atk_image_get_image_description(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn get_image_locale(&self) -> Option<glib::GString> {
        unsafe {
            from_glib_none(ffi::atk_image_get_image_locale(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn get_image_position(&self, coord_type: CoordType) -> (i32, i32) {
        unsafe {
            let mut x = mem::MaybeUninit::uninit();
            let mut y = mem::MaybeUninit::uninit();
            ffi::atk_image_get_image_position(
                self.as_ref().to_glib_none().0,
                x.as_mut_ptr(),
                y.as_mut_ptr(),
                coord_type.to_glib(),
            );
            let x = x.assume_init();
            let y = y.assume_init();
            (x, y)
        }
    }

    fn get_image_size(&self) -> (i32, i32) {
        unsafe {
            let mut width = mem::MaybeUninit::uninit();
            let mut height = mem::MaybeUninit::uninit();
            ffi::atk_image_get_image_size(
                self.as_ref().to_glib_none().0,
                width.as_mut_ptr(),
                height.as_mut_ptr(),
            );
            let width = width.assume_init();
            let height = height.assume_init();
            (width, height)
        }
    }

    fn set_image_description(&self, description: &str) -> bool {
        unsafe {
            from_glib(ffi::atk_image_set_image_description(
                self.as_ref().to_glib_none().0,
                description.to_glib_none().0,
            ))
        }
    }
}

impl fmt::Display for Image {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("Image")
    }
}
