// This file was generated by gir (https://github.com/gtk-rs/gir)
// from gir-files (https://github.com/gtk-rs/gir-files)
// DO NOT EDIT

use crate::Point;
use crate::Rect;
use glib::translate::*;

glib::wrapper! {
    #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Quad(Boxed<ffi::graphene_quad_t>);

    match fn {
        copy => |ptr| glib::gobject_ffi::g_boxed_copy(ffi::graphene_quad_get_type(), ptr as *mut _) as *mut ffi::graphene_quad_t,
        free => |ptr| glib::gobject_ffi::g_boxed_free(ffi::graphene_quad_get_type(), ptr as *mut _),
        init => |_ptr| (),
        clear => |_ptr| (),
        get_type => || ffi::graphene_quad_get_type(),
    }
}

impl Quad {
    pub fn bounds(&self) -> Rect {
        unsafe {
            let mut r = Rect::uninitialized();
            ffi::graphene_quad_bounds(self.to_glib_none().0, r.to_glib_none_mut().0);
            r
        }
    }

    pub fn contains(&self, p: &Point) -> bool {
        unsafe {
            from_glib(ffi::graphene_quad_contains(
                self.to_glib_none().0,
                p.to_glib_none().0,
            ))
        }
    }

    pub fn get_point(&self, index_: u32) -> Option<Point> {
        unsafe { from_glib_none(ffi::graphene_quad_get_point(self.to_glib_none().0, index_)) }
    }

    pub fn init(&mut self, p1: &Point, p2: &Point, p3: &Point, p4: &Point) {
        unsafe {
            ffi::graphene_quad_init(
                self.to_glib_none_mut().0,
                p1.to_glib_none().0,
                p2.to_glib_none().0,
                p3.to_glib_none().0,
                p4.to_glib_none().0,
            );
        }
    }

    //pub fn init_from_points(&mut self, points: /*Unimplemented*/FixedArray TypeId { ns_id: 1, id: 12 }; 4) {
    //    unsafe { TODO: call ffi:graphene_quad_init_from_points() }
    //}

    pub fn init_from_rect(&mut self, r: &Rect) {
        unsafe {
            ffi::graphene_quad_init_from_rect(self.to_glib_none_mut().0, r.to_glib_none().0);
        }
    }
}
