// This file was generated by gir (https://github.com/gtk-rs/gir)
// from gir-files (https://github.com/gtk-rs/gir-files)
// DO NOT EDIT

#[cfg(any(feature = "v2_44", feature = "dox"))]
#[cfg_attr(feature = "dox", doc(cfg(feature = "v2_44")))]
use glib::object::Cast;
use glib::object::IsA;
#[cfg(any(feature = "v2_44", feature = "dox"))]
#[cfg_attr(feature = "dox", doc(cfg(feature = "v2_44")))]
use glib::signal::connect_raw;
#[cfg(any(feature = "v2_44", feature = "dox"))]
#[cfg_attr(feature = "dox", doc(cfg(feature = "v2_44")))]
use glib::signal::SignalHandlerId;
#[cfg(any(feature = "v2_44", feature = "dox"))]
#[cfg_attr(feature = "dox", doc(cfg(feature = "v2_44")))]
use glib::translate::*;
#[cfg(any(feature = "v2_44", feature = "dox"))]
#[cfg_attr(feature = "dox", doc(cfg(feature = "v2_44")))]
use std::boxed::Box as Box_;
use std::fmt;
#[cfg(any(feature = "v2_44", feature = "dox"))]
#[cfg_attr(feature = "dox", doc(cfg(feature = "v2_44")))]
use std::mem::transmute;

glib::wrapper! {
    pub struct ListModel(Interface<ffi::GListModel>);

    match fn {
        get_type => || ffi::g_list_model_get_type(),
    }
}

pub const NONE_LIST_MODEL: Option<&ListModel> = None;

pub trait ListModelExt: 'static {
    //#[cfg(any(feature = "v2_44", feature = "dox"))]
    //#[cfg_attr(feature = "dox", doc(cfg(feature = "v2_44")))]
    //fn get_item(&self, position: u32) -> /*Unimplemented*/Option<Fundamental: Pointer>;

    #[cfg(any(feature = "v2_44", feature = "dox"))]
    #[cfg_attr(feature = "dox", doc(cfg(feature = "v2_44")))]
    fn get_item_type(&self) -> glib::types::Type;

    #[cfg(any(feature = "v2_44", feature = "dox"))]
    #[cfg_attr(feature = "dox", doc(cfg(feature = "v2_44")))]
    fn get_n_items(&self) -> u32;

    #[cfg(any(feature = "v2_44", feature = "dox"))]
    #[cfg_attr(feature = "dox", doc(cfg(feature = "v2_44")))]
    fn get_object(&self, position: u32) -> Option<glib::Object>;

    #[cfg(any(feature = "v2_44", feature = "dox"))]
    #[cfg_attr(feature = "dox", doc(cfg(feature = "v2_44")))]
    fn items_changed(&self, position: u32, removed: u32, added: u32);

    #[cfg(any(feature = "v2_44", feature = "dox"))]
    #[cfg_attr(feature = "dox", doc(cfg(feature = "v2_44")))]
    fn connect_items_changed<F: Fn(&Self, u32, u32, u32) + 'static>(&self, f: F)
        -> SignalHandlerId;
}

impl<O: IsA<ListModel>> ListModelExt for O {
    //#[cfg(any(feature = "v2_44", feature = "dox"))]
    //#[cfg_attr(feature = "dox", doc(cfg(feature = "v2_44")))]
    //fn get_item(&self, position: u32) -> /*Unimplemented*/Option<Fundamental: Pointer> {
    //    unsafe { TODO: call ffi:g_list_model_get_item() }
    //}

    #[cfg(any(feature = "v2_44", feature = "dox"))]
    #[cfg_attr(feature = "dox", doc(cfg(feature = "v2_44")))]
    fn get_item_type(&self) -> glib::types::Type {
        unsafe {
            from_glib(ffi::g_list_model_get_item_type(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    #[cfg(any(feature = "v2_44", feature = "dox"))]
    #[cfg_attr(feature = "dox", doc(cfg(feature = "v2_44")))]
    fn get_n_items(&self) -> u32 {
        unsafe { ffi::g_list_model_get_n_items(self.as_ref().to_glib_none().0) }
    }

    #[cfg(any(feature = "v2_44", feature = "dox"))]
    #[cfg_attr(feature = "dox", doc(cfg(feature = "v2_44")))]
    fn get_object(&self, position: u32) -> Option<glib::Object> {
        unsafe {
            from_glib_full(ffi::g_list_model_get_object(
                self.as_ref().to_glib_none().0,
                position,
            ))
        }
    }

    #[cfg(any(feature = "v2_44", feature = "dox"))]
    #[cfg_attr(feature = "dox", doc(cfg(feature = "v2_44")))]
    fn items_changed(&self, position: u32, removed: u32, added: u32) {
        unsafe {
            ffi::g_list_model_items_changed(
                self.as_ref().to_glib_none().0,
                position,
                removed,
                added,
            );
        }
    }

    #[cfg(any(feature = "v2_44", feature = "dox"))]
    #[cfg_attr(feature = "dox", doc(cfg(feature = "v2_44")))]
    fn connect_items_changed<F: Fn(&Self, u32, u32, u32) + 'static>(
        &self,
        f: F,
    ) -> SignalHandlerId {
        unsafe extern "C" fn items_changed_trampoline<P, F: Fn(&P, u32, u32, u32) + 'static>(
            this: *mut ffi::GListModel,
            position: libc::c_uint,
            removed: libc::c_uint,
            added: libc::c_uint,
            f: glib::ffi::gpointer,
        ) where
            P: IsA<ListModel>,
        {
            let f: &F = &*(f as *const F);
            f(
                &ListModel::from_glib_borrow(this).unsafe_cast_ref(),
                position,
                removed,
                added,
            )
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"items-changed\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    items_changed_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }
}

impl fmt::Display for ListModel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("ListModel")
    }
}
