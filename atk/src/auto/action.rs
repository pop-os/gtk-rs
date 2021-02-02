// This file was generated by gir (https://github.com/gtk-rs/gir)
// from gir-files (https://github.com/gtk-rs/gir-files)
// DO NOT EDIT

use glib::object::IsA;
use glib::translate::*;
use std::fmt;

glib::wrapper! {
    pub struct Action(Interface<ffi::AtkAction>);

    match fn {
        get_type => || ffi::atk_action_get_type(),
    }
}

pub const NONE_ACTION: Option<&Action> = None;

pub trait AtkActionExt: 'static {
    #[doc(alias = "atk_action_do_action")]
    fn do_action(&self, i: i32) -> bool;

    #[doc(alias = "atk_action_get_description")]
    fn get_description(&self, i: i32) -> Option<glib::GString>;

    #[doc(alias = "atk_action_get_keybinding")]
    fn get_keybinding(&self, i: i32) -> Option<glib::GString>;

    #[doc(alias = "atk_action_get_localized_name")]
    fn get_localized_name(&self, i: i32) -> Option<glib::GString>;

    #[doc(alias = "atk_action_get_n_actions")]
    fn get_n_actions(&self) -> i32;

    #[doc(alias = "atk_action_get_name")]
    fn get_name(&self, i: i32) -> Option<glib::GString>;

    #[doc(alias = "atk_action_set_description")]
    fn set_description(&self, i: i32, desc: &str) -> bool;
}

impl<O: IsA<Action>> AtkActionExt for O {
    fn do_action(&self, i: i32) -> bool {
        unsafe { from_glib(ffi::atk_action_do_action(self.as_ref().to_glib_none().0, i)) }
    }

    fn get_description(&self, i: i32) -> Option<glib::GString> {
        unsafe {
            from_glib_none(ffi::atk_action_get_description(
                self.as_ref().to_glib_none().0,
                i,
            ))
        }
    }

    fn get_keybinding(&self, i: i32) -> Option<glib::GString> {
        unsafe {
            from_glib_none(ffi::atk_action_get_keybinding(
                self.as_ref().to_glib_none().0,
                i,
            ))
        }
    }

    fn get_localized_name(&self, i: i32) -> Option<glib::GString> {
        unsafe {
            from_glib_none(ffi::atk_action_get_localized_name(
                self.as_ref().to_glib_none().0,
                i,
            ))
        }
    }

    fn get_n_actions(&self) -> i32 {
        unsafe { ffi::atk_action_get_n_actions(self.as_ref().to_glib_none().0) }
    }

    fn get_name(&self, i: i32) -> Option<glib::GString> {
        unsafe { from_glib_none(ffi::atk_action_get_name(self.as_ref().to_glib_none().0, i)) }
    }

    fn set_description(&self, i: i32, desc: &str) -> bool {
        unsafe {
            from_glib(ffi::atk_action_set_description(
                self.as_ref().to_glib_none().0,
                i,
                desc.to_glib_none().0,
            ))
        }
    }
}

impl fmt::Display for Action {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("Action")
    }
}
