// This file was generated by gir (d8a605d) from gir-files (469db10)
// DO NOT EDIT

use Pixbuf;
use PixbufAnimation;
use ffi;
use glib;
use glib::object::Downcast;
use glib::object::IsA;
use glib::signal::SignalHandlerId;
use glib::signal::connect;
use glib::translate::*;
use glib_ffi;
use gobject_ffi;
use std::boxed::Box as Box_;
use std::mem;
use std::mem::transmute;
use std::ptr;

glib_wrapper! {
    pub struct PixbufSimpleAnim(Object<ffi::GdkPixbufSimpleAnim, ffi::GdkPixbufSimpleAnimClass>): PixbufAnimation;

    match fn {
        get_type => || ffi::gdk_pixbuf_simple_anim_get_type(),
    }
}

impl PixbufSimpleAnim {
    pub fn new(width: i32, height: i32, rate: f32) -> PixbufSimpleAnim {
        unsafe {
            from_glib_full(ffi::gdk_pixbuf_simple_anim_new(width, height, rate))
        }
    }
}

pub trait PixbufSimpleAnimExt {
    fn add_frame(&self, pixbuf: &Pixbuf);

    fn get_loop(&self) -> bool;

    fn set_loop(&self, loop_: bool);

    fn connect_property_loop_notify<F: Fn(&Self) + 'static>(&self, f: F) -> SignalHandlerId;
}

impl<O: IsA<PixbufSimpleAnim> + IsA<glib::object::Object>> PixbufSimpleAnimExt for O {
    fn add_frame(&self, pixbuf: &Pixbuf) {
        unsafe {
            ffi::gdk_pixbuf_simple_anim_add_frame(self.to_glib_none().0, pixbuf.to_glib_none().0);
        }
    }

    fn get_loop(&self) -> bool {
        unsafe {
            from_glib(ffi::gdk_pixbuf_simple_anim_get_loop(self.to_glib_none().0))
        }
    }

    fn set_loop(&self, loop_: bool) {
        unsafe {
            ffi::gdk_pixbuf_simple_anim_set_loop(self.to_glib_none().0, loop_.to_glib());
        }
    }

    fn connect_property_loop_notify<F: Fn(&Self) + 'static>(&self, f: F) -> SignalHandlerId {
        unsafe {
            let f: Box_<Box_<Fn(&Self) + 'static>> = Box_::new(Box_::new(f));
            connect(self.to_glib_none().0, "notify::loop",
                transmute(notify_loop_trampoline::<Self> as usize), Box_::into_raw(f) as *mut _)
        }
    }
}

unsafe extern "C" fn notify_loop_trampoline<P>(this: *mut ffi::GdkPixbufSimpleAnim, _param_spec: glib_ffi::gpointer, f: glib_ffi::gpointer)
where P: IsA<PixbufSimpleAnim> {
    callback_guard!();
    let f: &&(Fn(&P) + 'static) = transmute(f);
    f(&PixbufSimpleAnim::from_glib_borrow(this).downcast_unchecked())
}
