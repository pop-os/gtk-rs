// This file was generated by gir (https://github.com/gtk-rs/gir)
// from gir-files (https://github.com/gtk-rs/gir-files)
// DO NOT EDIT

use crate::AppInfo;
use crate::File;
use glib::object::Cast;
use glib::object::IsA;
use glib::signal::connect_raw;
use glib::signal::SignalHandlerId;
use glib::translate::*;
use std::boxed::Box as Box_;
use std::fmt;
use std::mem::transmute;

glib::wrapper! {
    pub struct AppLaunchContext(Object<ffi::GAppLaunchContext, ffi::GAppLaunchContextClass>);

    match fn {
        get_type => || ffi::g_app_launch_context_get_type(),
    }
}

impl AppLaunchContext {
    pub fn new() -> AppLaunchContext {
        unsafe { from_glib_full(ffi::g_app_launch_context_new()) }
    }
}

impl Default for AppLaunchContext {
    fn default() -> Self {
        Self::new()
    }
}

pub const NONE_APP_LAUNCH_CONTEXT: Option<&AppLaunchContext> = None;

pub trait AppLaunchContextExt: 'static {
    fn get_display<P: IsA<AppInfo>>(&self, info: &P, files: &[File]) -> Option<glib::GString>;

    fn get_environment(&self) -> Vec<std::ffi::OsString>;

    fn get_startup_notify_id<P: IsA<AppInfo>>(
        &self,
        info: &P,
        files: &[File],
    ) -> Option<glib::GString>;

    fn launch_failed(&self, startup_notify_id: &str);

    fn setenv<P: AsRef<std::ffi::OsStr>, Q: AsRef<std::ffi::OsStr>>(&self, variable: P, value: Q);

    fn unsetenv<P: AsRef<std::ffi::OsStr>>(&self, variable: P);

    fn connect_launch_failed<F: Fn(&Self, &str) + 'static>(&self, f: F) -> SignalHandlerId;

    fn connect_launched<F: Fn(&Self, &AppInfo, &glib::Variant) + 'static>(
        &self,
        f: F,
    ) -> SignalHandlerId;
}

impl<O: IsA<AppLaunchContext>> AppLaunchContextExt for O {
    fn get_display<P: IsA<AppInfo>>(&self, info: &P, files: &[File]) -> Option<glib::GString> {
        unsafe {
            from_glib_full(ffi::g_app_launch_context_get_display(
                self.as_ref().to_glib_none().0,
                info.as_ref().to_glib_none().0,
                files.to_glib_none().0,
            ))
        }
    }

    fn get_environment(&self) -> Vec<std::ffi::OsString> {
        unsafe {
            FromGlibPtrContainer::from_glib_full(ffi::g_app_launch_context_get_environment(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn get_startup_notify_id<P: IsA<AppInfo>>(
        &self,
        info: &P,
        files: &[File],
    ) -> Option<glib::GString> {
        unsafe {
            from_glib_full(ffi::g_app_launch_context_get_startup_notify_id(
                self.as_ref().to_glib_none().0,
                info.as_ref().to_glib_none().0,
                files.to_glib_none().0,
            ))
        }
    }

    fn launch_failed(&self, startup_notify_id: &str) {
        unsafe {
            ffi::g_app_launch_context_launch_failed(
                self.as_ref().to_glib_none().0,
                startup_notify_id.to_glib_none().0,
            );
        }
    }

    fn setenv<P: AsRef<std::ffi::OsStr>, Q: AsRef<std::ffi::OsStr>>(&self, variable: P, value: Q) {
        unsafe {
            ffi::g_app_launch_context_setenv(
                self.as_ref().to_glib_none().0,
                variable.as_ref().to_glib_none().0,
                value.as_ref().to_glib_none().0,
            );
        }
    }

    fn unsetenv<P: AsRef<std::ffi::OsStr>>(&self, variable: P) {
        unsafe {
            ffi::g_app_launch_context_unsetenv(
                self.as_ref().to_glib_none().0,
                variable.as_ref().to_glib_none().0,
            );
        }
    }

    fn connect_launch_failed<F: Fn(&Self, &str) + 'static>(&self, f: F) -> SignalHandlerId {
        unsafe extern "C" fn launch_failed_trampoline<P, F: Fn(&P, &str) + 'static>(
            this: *mut ffi::GAppLaunchContext,
            startup_notify_id: *mut libc::c_char,
            f: glib::ffi::gpointer,
        ) where
            P: IsA<AppLaunchContext>,
        {
            let f: &F = &*(f as *const F);
            f(
                &AppLaunchContext::from_glib_borrow(this).unsafe_cast_ref(),
                &glib::GString::from_glib_borrow(startup_notify_id),
            )
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"launch-failed\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    launch_failed_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }

    fn connect_launched<F: Fn(&Self, &AppInfo, &glib::Variant) + 'static>(
        &self,
        f: F,
    ) -> SignalHandlerId {
        unsafe extern "C" fn launched_trampoline<P, F: Fn(&P, &AppInfo, &glib::Variant) + 'static>(
            this: *mut ffi::GAppLaunchContext,
            info: *mut ffi::GAppInfo,
            platform_data: *mut glib::ffi::GVariant,
            f: glib::ffi::gpointer,
        ) where
            P: IsA<AppLaunchContext>,
        {
            let f: &F = &*(f as *const F);
            f(
                &AppLaunchContext::from_glib_borrow(this).unsafe_cast_ref(),
                &from_glib_borrow(info),
                &from_glib_borrow(platform_data),
            )
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"launched\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    launched_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }
}

impl fmt::Display for AppLaunchContext {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("AppLaunchContext")
    }
}
