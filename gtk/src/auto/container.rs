// This file was generated by gir (https://github.com/gtk-rs/gir)
// from gir-files (https://github.com/gtk-rs/gir-files)
// DO NOT EDIT

use crate::Adjustment;
use crate::Buildable;
use crate::ResizeMode;
use crate::Widget;
use crate::WidgetPath;
use glib::object::Cast;
use glib::object::IsA;
use glib::signal::connect_raw;
use glib::signal::SignalHandlerId;
use glib::translate::*;
use glib::StaticType;
use std::boxed::Box as Box_;
use std::fmt;
use std::mem::transmute;

glib::wrapper! {
    pub struct Container(Object<ffi::GtkContainer, ffi::GtkContainerClass>) @extends Widget, @implements Buildable;

    match fn {
        get_type => || ffi::gtk_container_get_type(),
    }
}

pub const NONE_CONTAINER: Option<&Container> = None;

pub trait ContainerExt: 'static {
    fn add<P: IsA<Widget>>(&self, widget: &P);

    //fn add_with_properties<P: IsA<Widget>>(&self, widget: &P, first_prop_name: &str, : /*Unknown conversion*//*Unimplemented*/Fundamental: VarArgs);

    fn check_resize(&self);

    //fn child_get<P: IsA<Widget>>(&self, child: &P, first_prop_name: &str, : /*Unknown conversion*//*Unimplemented*/Fundamental: VarArgs);

    //fn child_get_valist<P: IsA<Widget>>(&self, child: &P, first_property_name: &str, var_args: /*Unknown conversion*//*Unimplemented*/Unsupported);

    fn child_notify<P: IsA<Widget>>(&self, child: &P, child_property: &str);

    #[cfg(any(feature = "v3_18", feature = "dox"))]
    #[cfg_attr(feature = "dox", doc(cfg(feature = "v3_18")))]
    fn child_notify_by_pspec<P: IsA<Widget>>(&self, child: &P, pspec: &glib::ParamSpec);

    //fn child_set<P: IsA<Widget>>(&self, child: &P, first_prop_name: &str, : /*Unknown conversion*//*Unimplemented*/Fundamental: VarArgs);

    //fn child_set_valist<P: IsA<Widget>>(&self, child: &P, first_property_name: &str, var_args: /*Unknown conversion*//*Unimplemented*/Unsupported);

    fn child_type(&self) -> glib::types::Type;

    fn forall<P: FnMut(&Widget)>(&self, callback: P);

    fn foreach<P: FnMut(&Widget)>(&self, callback: P);

    fn get_border_width(&self) -> u32;

    fn get_children(&self) -> Vec<Widget>;

    //#[cfg_attr(feature = "v3_24", deprecated)]
    //fn get_focus_chain(&self, focusable_widgets: /*Unimplemented*/Vec<Widget>) -> bool;

    fn get_focus_child(&self) -> Option<Widget>;

    fn get_focus_hadjustment(&self) -> Option<Adjustment>;

    fn get_focus_vadjustment(&self) -> Option<Adjustment>;

    fn get_path_for_child<P: IsA<Widget>>(&self, child: &P) -> Option<WidgetPath>;

    fn propagate_draw<P: IsA<Widget>>(&self, child: &P, cr: &cairo::Context);

    fn remove<P: IsA<Widget>>(&self, widget: &P);

    fn set_border_width(&self, border_width: u32);

    #[cfg_attr(feature = "v3_24", deprecated)]
    fn set_focus_chain(&self, focusable_widgets: &[Widget]);

    fn set_focus_child<P: IsA<Widget>>(&self, child: Option<&P>);

    fn set_focus_hadjustment<P: IsA<Adjustment>>(&self, adjustment: &P);

    fn set_focus_vadjustment<P: IsA<Adjustment>>(&self, adjustment: &P);

    #[cfg_attr(feature = "v3_24", deprecated)]
    fn unset_focus_chain(&self);

    fn set_property_child<P: IsA<Widget>>(&self, child: Option<&P>);

    fn get_property_resize_mode(&self) -> ResizeMode;

    fn set_property_resize_mode(&self, resize_mode: ResizeMode);

    fn connect_add<F: Fn(&Self, &Widget) + 'static>(&self, f: F) -> SignalHandlerId;

    fn connect_check_resize<F: Fn(&Self) + 'static>(&self, f: F) -> SignalHandlerId;

    fn connect_remove<F: Fn(&Self, &Widget) + 'static>(&self, f: F) -> SignalHandlerId;

    fn connect_set_focus_child<F: Fn(&Self, &Widget) + 'static>(&self, f: F) -> SignalHandlerId;

    fn connect_property_border_width_notify<F: Fn(&Self) + 'static>(&self, f: F)
        -> SignalHandlerId;

    fn connect_property_child_notify<F: Fn(&Self) + 'static>(&self, f: F) -> SignalHandlerId;

    fn connect_property_resize_mode_notify<F: Fn(&Self) + 'static>(&self, f: F) -> SignalHandlerId;
}

impl<O: IsA<Container>> ContainerExt for O {
    fn add<P: IsA<Widget>>(&self, widget: &P) {
        unsafe {
            ffi::gtk_container_add(
                self.as_ref().to_glib_none().0,
                widget.as_ref().to_glib_none().0,
            );
        }
    }

    //fn add_with_properties<P: IsA<Widget>>(&self, widget: &P, first_prop_name: &str, : /*Unknown conversion*//*Unimplemented*/Fundamental: VarArgs) {
    //    unsafe { TODO: call ffi:gtk_container_add_with_properties() }
    //}

    fn check_resize(&self) {
        unsafe {
            ffi::gtk_container_check_resize(self.as_ref().to_glib_none().0);
        }
    }

    //fn child_get<P: IsA<Widget>>(&self, child: &P, first_prop_name: &str, : /*Unknown conversion*//*Unimplemented*/Fundamental: VarArgs) {
    //    unsafe { TODO: call ffi:gtk_container_child_get() }
    //}

    //fn child_get_valist<P: IsA<Widget>>(&self, child: &P, first_property_name: &str, var_args: /*Unknown conversion*//*Unimplemented*/Unsupported) {
    //    unsafe { TODO: call ffi:gtk_container_child_get_valist() }
    //}

    fn child_notify<P: IsA<Widget>>(&self, child: &P, child_property: &str) {
        unsafe {
            ffi::gtk_container_child_notify(
                self.as_ref().to_glib_none().0,
                child.as_ref().to_glib_none().0,
                child_property.to_glib_none().0,
            );
        }
    }

    #[cfg(any(feature = "v3_18", feature = "dox"))]
    #[cfg_attr(feature = "dox", doc(cfg(feature = "v3_18")))]
    fn child_notify_by_pspec<P: IsA<Widget>>(&self, child: &P, pspec: &glib::ParamSpec) {
        unsafe {
            ffi::gtk_container_child_notify_by_pspec(
                self.as_ref().to_glib_none().0,
                child.as_ref().to_glib_none().0,
                pspec.to_glib_none().0,
            );
        }
    }

    //fn child_set<P: IsA<Widget>>(&self, child: &P, first_prop_name: &str, : /*Unknown conversion*//*Unimplemented*/Fundamental: VarArgs) {
    //    unsafe { TODO: call ffi:gtk_container_child_set() }
    //}

    //fn child_set_valist<P: IsA<Widget>>(&self, child: &P, first_property_name: &str, var_args: /*Unknown conversion*//*Unimplemented*/Unsupported) {
    //    unsafe { TODO: call ffi:gtk_container_child_set_valist() }
    //}

    fn child_type(&self) -> glib::types::Type {
        unsafe {
            from_glib(ffi::gtk_container_child_type(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn forall<P: FnMut(&Widget)>(&self, callback: P) {
        let callback_data: P = callback;
        unsafe extern "C" fn callback_func<P: FnMut(&Widget)>(
            widget: *mut ffi::GtkWidget,
            data: glib::ffi::gpointer,
        ) {
            let widget = from_glib_borrow(widget);
            let callback: *mut P = data as *const _ as usize as *mut P;
            (*callback)(&widget);
        }
        let callback = Some(callback_func::<P> as _);
        let super_callback0: &P = &callback_data;
        unsafe {
            ffi::gtk_container_forall(
                self.as_ref().to_glib_none().0,
                callback,
                super_callback0 as *const _ as usize as *mut _,
            );
        }
    }

    fn foreach<P: FnMut(&Widget)>(&self, callback: P) {
        let callback_data: P = callback;
        unsafe extern "C" fn callback_func<P: FnMut(&Widget)>(
            widget: *mut ffi::GtkWidget,
            data: glib::ffi::gpointer,
        ) {
            let widget = from_glib_borrow(widget);
            let callback: *mut P = data as *const _ as usize as *mut P;
            (*callback)(&widget);
        }
        let callback = Some(callback_func::<P> as _);
        let super_callback0: &P = &callback_data;
        unsafe {
            ffi::gtk_container_foreach(
                self.as_ref().to_glib_none().0,
                callback,
                super_callback0 as *const _ as usize as *mut _,
            );
        }
    }

    fn get_border_width(&self) -> u32 {
        unsafe { ffi::gtk_container_get_border_width(self.as_ref().to_glib_none().0) }
    }

    fn get_children(&self) -> Vec<Widget> {
        unsafe {
            FromGlibPtrContainer::from_glib_container(ffi::gtk_container_get_children(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    //fn get_focus_chain(&self, focusable_widgets: /*Unimplemented*/Vec<Widget>) -> bool {
    //    unsafe { TODO: call ffi:gtk_container_get_focus_chain() }
    //}

    fn get_focus_child(&self) -> Option<Widget> {
        unsafe {
            from_glib_none(ffi::gtk_container_get_focus_child(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn get_focus_hadjustment(&self) -> Option<Adjustment> {
        unsafe {
            from_glib_none(ffi::gtk_container_get_focus_hadjustment(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn get_focus_vadjustment(&self) -> Option<Adjustment> {
        unsafe {
            from_glib_none(ffi::gtk_container_get_focus_vadjustment(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn get_path_for_child<P: IsA<Widget>>(&self, child: &P) -> Option<WidgetPath> {
        unsafe {
            from_glib_full(ffi::gtk_container_get_path_for_child(
                self.as_ref().to_glib_none().0,
                child.as_ref().to_glib_none().0,
            ))
        }
    }

    fn propagate_draw<P: IsA<Widget>>(&self, child: &P, cr: &cairo::Context) {
        unsafe {
            ffi::gtk_container_propagate_draw(
                self.as_ref().to_glib_none().0,
                child.as_ref().to_glib_none().0,
                mut_override(cr.to_glib_none().0),
            );
        }
    }

    fn remove<P: IsA<Widget>>(&self, widget: &P) {
        unsafe {
            ffi::gtk_container_remove(
                self.as_ref().to_glib_none().0,
                widget.as_ref().to_glib_none().0,
            );
        }
    }

    fn set_border_width(&self, border_width: u32) {
        unsafe {
            ffi::gtk_container_set_border_width(self.as_ref().to_glib_none().0, border_width);
        }
    }

    fn set_focus_chain(&self, focusable_widgets: &[Widget]) {
        unsafe {
            ffi::gtk_container_set_focus_chain(
                self.as_ref().to_glib_none().0,
                focusable_widgets.to_glib_none().0,
            );
        }
    }

    fn set_focus_child<P: IsA<Widget>>(&self, child: Option<&P>) {
        unsafe {
            ffi::gtk_container_set_focus_child(
                self.as_ref().to_glib_none().0,
                child.map(|p| p.as_ref()).to_glib_none().0,
            );
        }
    }

    fn set_focus_hadjustment<P: IsA<Adjustment>>(&self, adjustment: &P) {
        unsafe {
            ffi::gtk_container_set_focus_hadjustment(
                self.as_ref().to_glib_none().0,
                adjustment.as_ref().to_glib_none().0,
            );
        }
    }

    fn set_focus_vadjustment<P: IsA<Adjustment>>(&self, adjustment: &P) {
        unsafe {
            ffi::gtk_container_set_focus_vadjustment(
                self.as_ref().to_glib_none().0,
                adjustment.as_ref().to_glib_none().0,
            );
        }
    }

    fn unset_focus_chain(&self) {
        unsafe {
            ffi::gtk_container_unset_focus_chain(self.as_ref().to_glib_none().0);
        }
    }

    fn set_property_child<P: IsA<Widget>>(&self, child: Option<&P>) {
        unsafe {
            glib::gobject_ffi::g_object_set_property(
                self.to_glib_none().0 as *mut glib::gobject_ffi::GObject,
                b"child\0".as_ptr() as *const _,
                glib::Value::from(child).to_glib_none().0,
            );
        }
    }

    fn get_property_resize_mode(&self) -> ResizeMode {
        unsafe {
            let mut value = glib::Value::from_type(<ResizeMode as StaticType>::static_type());
            glib::gobject_ffi::g_object_get_property(
                self.to_glib_none().0 as *mut glib::gobject_ffi::GObject,
                b"resize-mode\0".as_ptr() as *const _,
                value.to_glib_none_mut().0,
            );
            value
                .get()
                .expect("Return Value for property `resize-mode` getter")
                .unwrap()
        }
    }

    fn set_property_resize_mode(&self, resize_mode: ResizeMode) {
        unsafe {
            glib::gobject_ffi::g_object_set_property(
                self.to_glib_none().0 as *mut glib::gobject_ffi::GObject,
                b"resize-mode\0".as_ptr() as *const _,
                glib::Value::from(&resize_mode).to_glib_none().0,
            );
        }
    }

    fn connect_add<F: Fn(&Self, &Widget) + 'static>(&self, f: F) -> SignalHandlerId {
        unsafe extern "C" fn add_trampoline<P, F: Fn(&P, &Widget) + 'static>(
            this: *mut ffi::GtkContainer,
            object: *mut ffi::GtkWidget,
            f: glib::ffi::gpointer,
        ) where
            P: IsA<Container>,
        {
            let f: &F = &*(f as *const F);
            f(
                &Container::from_glib_borrow(this).unsafe_cast_ref(),
                &from_glib_borrow(object),
            )
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"add\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    add_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }

    fn connect_check_resize<F: Fn(&Self) + 'static>(&self, f: F) -> SignalHandlerId {
        unsafe extern "C" fn check_resize_trampoline<P, F: Fn(&P) + 'static>(
            this: *mut ffi::GtkContainer,
            f: glib::ffi::gpointer,
        ) where
            P: IsA<Container>,
        {
            let f: &F = &*(f as *const F);
            f(&Container::from_glib_borrow(this).unsafe_cast_ref())
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"check-resize\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    check_resize_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }

    fn connect_remove<F: Fn(&Self, &Widget) + 'static>(&self, f: F) -> SignalHandlerId {
        unsafe extern "C" fn remove_trampoline<P, F: Fn(&P, &Widget) + 'static>(
            this: *mut ffi::GtkContainer,
            object: *mut ffi::GtkWidget,
            f: glib::ffi::gpointer,
        ) where
            P: IsA<Container>,
        {
            let f: &F = &*(f as *const F);
            f(
                &Container::from_glib_borrow(this).unsafe_cast_ref(),
                &from_glib_borrow(object),
            )
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"remove\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    remove_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }

    fn connect_set_focus_child<F: Fn(&Self, &Widget) + 'static>(&self, f: F) -> SignalHandlerId {
        unsafe extern "C" fn set_focus_child_trampoline<P, F: Fn(&P, &Widget) + 'static>(
            this: *mut ffi::GtkContainer,
            object: *mut ffi::GtkWidget,
            f: glib::ffi::gpointer,
        ) where
            P: IsA<Container>,
        {
            let f: &F = &*(f as *const F);
            f(
                &Container::from_glib_borrow(this).unsafe_cast_ref(),
                &from_glib_borrow(object),
            )
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"set-focus-child\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    set_focus_child_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }

    fn connect_property_border_width_notify<F: Fn(&Self) + 'static>(
        &self,
        f: F,
    ) -> SignalHandlerId {
        unsafe extern "C" fn notify_border_width_trampoline<P, F: Fn(&P) + 'static>(
            this: *mut ffi::GtkContainer,
            _param_spec: glib::ffi::gpointer,
            f: glib::ffi::gpointer,
        ) where
            P: IsA<Container>,
        {
            let f: &F = &*(f as *const F);
            f(&Container::from_glib_borrow(this).unsafe_cast_ref())
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"notify::border-width\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    notify_border_width_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }

    fn connect_property_child_notify<F: Fn(&Self) + 'static>(&self, f: F) -> SignalHandlerId {
        unsafe extern "C" fn notify_child_trampoline<P, F: Fn(&P) + 'static>(
            this: *mut ffi::GtkContainer,
            _param_spec: glib::ffi::gpointer,
            f: glib::ffi::gpointer,
        ) where
            P: IsA<Container>,
        {
            let f: &F = &*(f as *const F);
            f(&Container::from_glib_borrow(this).unsafe_cast_ref())
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"notify::child\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    notify_child_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }

    fn connect_property_resize_mode_notify<F: Fn(&Self) + 'static>(&self, f: F) -> SignalHandlerId {
        unsafe extern "C" fn notify_resize_mode_trampoline<P, F: Fn(&P) + 'static>(
            this: *mut ffi::GtkContainer,
            _param_spec: glib::ffi::gpointer,
            f: glib::ffi::gpointer,
        ) where
            P: IsA<Container>,
        {
            let f: &F = &*(f as *const F);
            f(&Container::from_glib_borrow(this).unsafe_cast_ref())
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"notify::resize-mode\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    notify_resize_mode_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }
}

impl fmt::Display for Container {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("Container")
    }
}
