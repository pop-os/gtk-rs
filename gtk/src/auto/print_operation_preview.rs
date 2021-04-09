// This file was generated by gir (https://github.com/gtk-rs/gir)
// from gir-files (https://github.com/gtk-rs/gir-files)
// DO NOT EDIT

use crate::PageSetup;
use crate::PrintContext;
use glib::object::Cast;
use glib::object::IsA;
use glib::signal::connect_raw;
use glib::signal::SignalHandlerId;
use glib::translate::*;
use std::boxed::Box as Box_;
use std::fmt;
use std::mem::transmute;

glib::wrapper! {
    pub struct PrintOperationPreview(Interface<ffi::GtkPrintOperationPreview, ffi::GtkPrintOperationPreviewIface>);

    match fn {
        get_type => || ffi::gtk_print_operation_preview_get_type(),
    }
}

pub const NONE_PRINT_OPERATION_PREVIEW: Option<&PrintOperationPreview> = None;

pub trait PrintOperationPreviewExt: 'static {
    fn end_preview(&self);

    fn is_selected(&self, page_nr: i32) -> bool;

    fn render_page(&self, page_nr: i32);

    fn connect_got_page_size<F: Fn(&Self, &PrintContext, &PageSetup) + 'static>(
        &self,
        f: F,
    ) -> SignalHandlerId;

    fn connect_ready<F: Fn(&Self, &PrintContext) + 'static>(&self, f: F) -> SignalHandlerId;
}

impl<O: IsA<PrintOperationPreview>> PrintOperationPreviewExt for O {
    fn end_preview(&self) {
        unsafe {
            ffi::gtk_print_operation_preview_end_preview(self.as_ref().to_glib_none().0);
        }
    }

    fn is_selected(&self, page_nr: i32) -> bool {
        unsafe {
            from_glib(ffi::gtk_print_operation_preview_is_selected(
                self.as_ref().to_glib_none().0,
                page_nr,
            ))
        }
    }

    fn render_page(&self, page_nr: i32) {
        unsafe {
            ffi::gtk_print_operation_preview_render_page(self.as_ref().to_glib_none().0, page_nr);
        }
    }

    fn connect_got_page_size<F: Fn(&Self, &PrintContext, &PageSetup) + 'static>(
        &self,
        f: F,
    ) -> SignalHandlerId {
        unsafe extern "C" fn got_page_size_trampoline<
            P,
            F: Fn(&P, &PrintContext, &PageSetup) + 'static,
        >(
            this: *mut ffi::GtkPrintOperationPreview,
            context: *mut ffi::GtkPrintContext,
            page_setup: *mut ffi::GtkPageSetup,
            f: glib::ffi::gpointer,
        ) where
            P: IsA<PrintOperationPreview>,
        {
            let f: &F = &*(f as *const F);
            f(
                &PrintOperationPreview::from_glib_borrow(this).unsafe_cast_ref(),
                &from_glib_borrow(context),
                &from_glib_borrow(page_setup),
            )
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"got-page-size\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    got_page_size_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }

    fn connect_ready<F: Fn(&Self, &PrintContext) + 'static>(&self, f: F) -> SignalHandlerId {
        unsafe extern "C" fn ready_trampoline<P, F: Fn(&P, &PrintContext) + 'static>(
            this: *mut ffi::GtkPrintOperationPreview,
            context: *mut ffi::GtkPrintContext,
            f: glib::ffi::gpointer,
        ) where
            P: IsA<PrintOperationPreview>,
        {
            let f: &F = &*(f as *const F);
            f(
                &PrintOperationPreview::from_glib_borrow(this).unsafe_cast_ref(),
                &from_glib_borrow(context),
            )
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"ready\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    ready_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }
}

impl fmt::Display for PrintOperationPreview {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("PrintOperationPreview")
    }
}
