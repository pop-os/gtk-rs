// This file was generated by gir (https://github.com/gtk-rs/gir)
// from gir-files (https://github.com/gtk-rs/gir-files)
// DO NOT EDIT

use crate::Drive;
use crate::Mount;
use crate::Volume;
use glib::object::Cast;
use glib::object::IsA;
use glib::signal::connect_raw;
use glib::signal::SignalHandlerId;
use glib::translate::*;
use std::boxed::Box as Box_;
use std::fmt;
use std::mem::transmute;

glib::wrapper! {
    pub struct VolumeMonitor(Object<ffi::GVolumeMonitor, ffi::GVolumeMonitorClass>);

    match fn {
        get_type => || ffi::g_volume_monitor_get_type(),
    }
}

impl VolumeMonitor {
    #[doc(alias = "g_volume_monitor_get")]
    pub fn get() -> VolumeMonitor {
        unsafe { from_glib_full(ffi::g_volume_monitor_get()) }
    }
}

pub const NONE_VOLUME_MONITOR: Option<&VolumeMonitor> = None;

pub trait VolumeMonitorExt: 'static {
    #[doc(alias = "g_volume_monitor_get_connected_drives")]
    fn get_connected_drives(&self) -> Vec<Drive>;

    #[doc(alias = "g_volume_monitor_get_mount_for_uuid")]
    fn get_mount_for_uuid(&self, uuid: &str) -> Option<Mount>;

    #[doc(alias = "g_volume_monitor_get_mounts")]
    fn get_mounts(&self) -> Vec<Mount>;

    #[doc(alias = "g_volume_monitor_get_volume_for_uuid")]
    fn get_volume_for_uuid(&self, uuid: &str) -> Option<Volume>;

    #[doc(alias = "g_volume_monitor_get_volumes")]
    fn get_volumes(&self) -> Vec<Volume>;

    fn connect_drive_changed<F: Fn(&Self, &Drive) + 'static>(&self, f: F) -> SignalHandlerId;

    fn connect_drive_connected<F: Fn(&Self, &Drive) + 'static>(&self, f: F) -> SignalHandlerId;

    fn connect_drive_disconnected<F: Fn(&Self, &Drive) + 'static>(&self, f: F) -> SignalHandlerId;

    fn connect_drive_eject_button<F: Fn(&Self, &Drive) + 'static>(&self, f: F) -> SignalHandlerId;

    fn connect_drive_stop_button<F: Fn(&Self, &Drive) + 'static>(&self, f: F) -> SignalHandlerId;

    fn connect_mount_added<F: Fn(&Self, &Mount) + 'static>(&self, f: F) -> SignalHandlerId;

    fn connect_mount_changed<F: Fn(&Self, &Mount) + 'static>(&self, f: F) -> SignalHandlerId;

    fn connect_mount_pre_unmount<F: Fn(&Self, &Mount) + 'static>(&self, f: F) -> SignalHandlerId;

    fn connect_mount_removed<F: Fn(&Self, &Mount) + 'static>(&self, f: F) -> SignalHandlerId;

    fn connect_volume_added<F: Fn(&Self, &Volume) + 'static>(&self, f: F) -> SignalHandlerId;

    fn connect_volume_changed<F: Fn(&Self, &Volume) + 'static>(&self, f: F) -> SignalHandlerId;

    fn connect_volume_removed<F: Fn(&Self, &Volume) + 'static>(&self, f: F) -> SignalHandlerId;
}

impl<O: IsA<VolumeMonitor>> VolumeMonitorExt for O {
    fn get_connected_drives(&self) -> Vec<Drive> {
        unsafe {
            FromGlibPtrContainer::from_glib_full(ffi::g_volume_monitor_get_connected_drives(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn get_mount_for_uuid(&self, uuid: &str) -> Option<Mount> {
        unsafe {
            from_glib_full(ffi::g_volume_monitor_get_mount_for_uuid(
                self.as_ref().to_glib_none().0,
                uuid.to_glib_none().0,
            ))
        }
    }

    fn get_mounts(&self) -> Vec<Mount> {
        unsafe {
            FromGlibPtrContainer::from_glib_full(ffi::g_volume_monitor_get_mounts(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn get_volume_for_uuid(&self, uuid: &str) -> Option<Volume> {
        unsafe {
            from_glib_full(ffi::g_volume_monitor_get_volume_for_uuid(
                self.as_ref().to_glib_none().0,
                uuid.to_glib_none().0,
            ))
        }
    }

    fn get_volumes(&self) -> Vec<Volume> {
        unsafe {
            FromGlibPtrContainer::from_glib_full(ffi::g_volume_monitor_get_volumes(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn connect_drive_changed<F: Fn(&Self, &Drive) + 'static>(&self, f: F) -> SignalHandlerId {
        unsafe extern "C" fn drive_changed_trampoline<P, F: Fn(&P, &Drive) + 'static>(
            this: *mut ffi::GVolumeMonitor,
            drive: *mut ffi::GDrive,
            f: glib::ffi::gpointer,
        ) where
            P: IsA<VolumeMonitor>,
        {
            let f: &F = &*(f as *const F);
            f(
                &VolumeMonitor::from_glib_borrow(this).unsafe_cast_ref(),
                &from_glib_borrow(drive),
            )
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"drive-changed\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    drive_changed_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }

    fn connect_drive_connected<F: Fn(&Self, &Drive) + 'static>(&self, f: F) -> SignalHandlerId {
        unsafe extern "C" fn drive_connected_trampoline<P, F: Fn(&P, &Drive) + 'static>(
            this: *mut ffi::GVolumeMonitor,
            drive: *mut ffi::GDrive,
            f: glib::ffi::gpointer,
        ) where
            P: IsA<VolumeMonitor>,
        {
            let f: &F = &*(f as *const F);
            f(
                &VolumeMonitor::from_glib_borrow(this).unsafe_cast_ref(),
                &from_glib_borrow(drive),
            )
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"drive-connected\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    drive_connected_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }

    fn connect_drive_disconnected<F: Fn(&Self, &Drive) + 'static>(&self, f: F) -> SignalHandlerId {
        unsafe extern "C" fn drive_disconnected_trampoline<P, F: Fn(&P, &Drive) + 'static>(
            this: *mut ffi::GVolumeMonitor,
            drive: *mut ffi::GDrive,
            f: glib::ffi::gpointer,
        ) where
            P: IsA<VolumeMonitor>,
        {
            let f: &F = &*(f as *const F);
            f(
                &VolumeMonitor::from_glib_borrow(this).unsafe_cast_ref(),
                &from_glib_borrow(drive),
            )
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"drive-disconnected\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    drive_disconnected_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }

    fn connect_drive_eject_button<F: Fn(&Self, &Drive) + 'static>(&self, f: F) -> SignalHandlerId {
        unsafe extern "C" fn drive_eject_button_trampoline<P, F: Fn(&P, &Drive) + 'static>(
            this: *mut ffi::GVolumeMonitor,
            drive: *mut ffi::GDrive,
            f: glib::ffi::gpointer,
        ) where
            P: IsA<VolumeMonitor>,
        {
            let f: &F = &*(f as *const F);
            f(
                &VolumeMonitor::from_glib_borrow(this).unsafe_cast_ref(),
                &from_glib_borrow(drive),
            )
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"drive-eject-button\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    drive_eject_button_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }

    fn connect_drive_stop_button<F: Fn(&Self, &Drive) + 'static>(&self, f: F) -> SignalHandlerId {
        unsafe extern "C" fn drive_stop_button_trampoline<P, F: Fn(&P, &Drive) + 'static>(
            this: *mut ffi::GVolumeMonitor,
            drive: *mut ffi::GDrive,
            f: glib::ffi::gpointer,
        ) where
            P: IsA<VolumeMonitor>,
        {
            let f: &F = &*(f as *const F);
            f(
                &VolumeMonitor::from_glib_borrow(this).unsafe_cast_ref(),
                &from_glib_borrow(drive),
            )
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"drive-stop-button\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    drive_stop_button_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }

    fn connect_mount_added<F: Fn(&Self, &Mount) + 'static>(&self, f: F) -> SignalHandlerId {
        unsafe extern "C" fn mount_added_trampoline<P, F: Fn(&P, &Mount) + 'static>(
            this: *mut ffi::GVolumeMonitor,
            mount: *mut ffi::GMount,
            f: glib::ffi::gpointer,
        ) where
            P: IsA<VolumeMonitor>,
        {
            let f: &F = &*(f as *const F);
            f(
                &VolumeMonitor::from_glib_borrow(this).unsafe_cast_ref(),
                &from_glib_borrow(mount),
            )
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"mount-added\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    mount_added_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }

    fn connect_mount_changed<F: Fn(&Self, &Mount) + 'static>(&self, f: F) -> SignalHandlerId {
        unsafe extern "C" fn mount_changed_trampoline<P, F: Fn(&P, &Mount) + 'static>(
            this: *mut ffi::GVolumeMonitor,
            mount: *mut ffi::GMount,
            f: glib::ffi::gpointer,
        ) where
            P: IsA<VolumeMonitor>,
        {
            let f: &F = &*(f as *const F);
            f(
                &VolumeMonitor::from_glib_borrow(this).unsafe_cast_ref(),
                &from_glib_borrow(mount),
            )
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"mount-changed\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    mount_changed_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }

    fn connect_mount_pre_unmount<F: Fn(&Self, &Mount) + 'static>(&self, f: F) -> SignalHandlerId {
        unsafe extern "C" fn mount_pre_unmount_trampoline<P, F: Fn(&P, &Mount) + 'static>(
            this: *mut ffi::GVolumeMonitor,
            mount: *mut ffi::GMount,
            f: glib::ffi::gpointer,
        ) where
            P: IsA<VolumeMonitor>,
        {
            let f: &F = &*(f as *const F);
            f(
                &VolumeMonitor::from_glib_borrow(this).unsafe_cast_ref(),
                &from_glib_borrow(mount),
            )
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"mount-pre-unmount\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    mount_pre_unmount_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }

    fn connect_mount_removed<F: Fn(&Self, &Mount) + 'static>(&self, f: F) -> SignalHandlerId {
        unsafe extern "C" fn mount_removed_trampoline<P, F: Fn(&P, &Mount) + 'static>(
            this: *mut ffi::GVolumeMonitor,
            mount: *mut ffi::GMount,
            f: glib::ffi::gpointer,
        ) where
            P: IsA<VolumeMonitor>,
        {
            let f: &F = &*(f as *const F);
            f(
                &VolumeMonitor::from_glib_borrow(this).unsafe_cast_ref(),
                &from_glib_borrow(mount),
            )
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"mount-removed\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    mount_removed_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }

    fn connect_volume_added<F: Fn(&Self, &Volume) + 'static>(&self, f: F) -> SignalHandlerId {
        unsafe extern "C" fn volume_added_trampoline<P, F: Fn(&P, &Volume) + 'static>(
            this: *mut ffi::GVolumeMonitor,
            volume: *mut ffi::GVolume,
            f: glib::ffi::gpointer,
        ) where
            P: IsA<VolumeMonitor>,
        {
            let f: &F = &*(f as *const F);
            f(
                &VolumeMonitor::from_glib_borrow(this).unsafe_cast_ref(),
                &from_glib_borrow(volume),
            )
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"volume-added\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    volume_added_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }

    fn connect_volume_changed<F: Fn(&Self, &Volume) + 'static>(&self, f: F) -> SignalHandlerId {
        unsafe extern "C" fn volume_changed_trampoline<P, F: Fn(&P, &Volume) + 'static>(
            this: *mut ffi::GVolumeMonitor,
            volume: *mut ffi::GVolume,
            f: glib::ffi::gpointer,
        ) where
            P: IsA<VolumeMonitor>,
        {
            let f: &F = &*(f as *const F);
            f(
                &VolumeMonitor::from_glib_borrow(this).unsafe_cast_ref(),
                &from_glib_borrow(volume),
            )
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"volume-changed\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    volume_changed_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }

    fn connect_volume_removed<F: Fn(&Self, &Volume) + 'static>(&self, f: F) -> SignalHandlerId {
        unsafe extern "C" fn volume_removed_trampoline<P, F: Fn(&P, &Volume) + 'static>(
            this: *mut ffi::GVolumeMonitor,
            volume: *mut ffi::GVolume,
            f: glib::ffi::gpointer,
        ) where
            P: IsA<VolumeMonitor>,
        {
            let f: &F = &*(f as *const F);
            f(
                &VolumeMonitor::from_glib_borrow(this).unsafe_cast_ref(),
                &from_glib_borrow(volume),
            )
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"volume-removed\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    volume_removed_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }
}

impl fmt::Display for VolumeMonitor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("VolumeMonitor")
    }
}
