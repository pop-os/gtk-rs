// This file was generated by gir (https://github.com/gtk-rs/gir)
// from gir-files (https://github.com/gtk-rs/gir-files)
// DO NOT EDIT

use crate::Align;
use crate::Application;
use crate::Bin;
use crate::Buildable;
use crate::Container;
use crate::Dialog;
use crate::FileChooser;
use crate::FileChooserAction;
use crate::FileFilter;
use crate::ResizeMode;
use crate::Widget;
use crate::Window;
use crate::WindowPosition;
use crate::WindowType;
use glib::object::Cast;
use glib::object::IsA;
use glib::StaticType;
use glib::ToValue;
use std::fmt;

glib::wrapper! {
    pub struct FileChooserDialog(Object<ffi::GtkFileChooserDialog, ffi::GtkFileChooserDialogClass>) @extends Dialog, Window, Bin, Container, Widget, @implements Buildable, FileChooser;

    match fn {
        get_type => || ffi::gtk_file_chooser_dialog_get_type(),
    }
}

impl FileChooserDialog {
    //pub fn new<P: IsA<Window>>(title: Option<&str>, parent: Option<&P>, action: FileChooserAction, first_button_text: Option<&str>, : /*Unknown conversion*//*Unimplemented*/Fundamental: VarArgs) -> FileChooserDialog {
    //    unsafe { TODO: call ffi:gtk_file_chooser_dialog_new() }
    //}
}

#[derive(Clone, Default)]
pub struct FileChooserDialogBuilder {
    use_header_bar: Option<i32>,
    accept_focus: Option<bool>,
    application: Option<Application>,
    attached_to: Option<Widget>,
    decorated: Option<bool>,
    default_height: Option<i32>,
    default_width: Option<i32>,
    deletable: Option<bool>,
    destroy_with_parent: Option<bool>,
    focus_on_map: Option<bool>,
    focus_visible: Option<bool>,
    gravity: Option<gdk::Gravity>,
    hide_titlebar_when_maximized: Option<bool>,
    icon: Option<gdk_pixbuf::Pixbuf>,
    icon_name: Option<String>,
    mnemonics_visible: Option<bool>,
    modal: Option<bool>,
    resizable: Option<bool>,
    role: Option<String>,
    screen: Option<gdk::Screen>,
    skip_pager_hint: Option<bool>,
    skip_taskbar_hint: Option<bool>,
    startup_id: Option<String>,
    title: Option<String>,
    transient_for: Option<Window>,
    type_: Option<WindowType>,
    type_hint: Option<gdk::WindowTypeHint>,
    urgency_hint: Option<bool>,
    window_position: Option<WindowPosition>,
    border_width: Option<u32>,
    child: Option<Widget>,
    resize_mode: Option<ResizeMode>,
    app_paintable: Option<bool>,
    can_default: Option<bool>,
    can_focus: Option<bool>,
    events: Option<gdk::EventMask>,
    expand: Option<bool>,
    #[cfg(any(feature = "v3_20", feature = "dox"))]
    #[cfg_attr(feature = "dox", doc(cfg(feature = "v3_20")))]
    focus_on_click: Option<bool>,
    halign: Option<Align>,
    has_default: Option<bool>,
    has_focus: Option<bool>,
    has_tooltip: Option<bool>,
    height_request: Option<i32>,
    hexpand: Option<bool>,
    hexpand_set: Option<bool>,
    is_focus: Option<bool>,
    margin: Option<i32>,
    margin_bottom: Option<i32>,
    margin_end: Option<i32>,
    margin_start: Option<i32>,
    margin_top: Option<i32>,
    name: Option<String>,
    no_show_all: Option<bool>,
    opacity: Option<f64>,
    parent: Option<Container>,
    receives_default: Option<bool>,
    sensitive: Option<bool>,
    tooltip_markup: Option<String>,
    tooltip_text: Option<String>,
    valign: Option<Align>,
    vexpand: Option<bool>,
    vexpand_set: Option<bool>,
    visible: Option<bool>,
    width_request: Option<i32>,
    action: Option<FileChooserAction>,
    create_folders: Option<bool>,
    do_overwrite_confirmation: Option<bool>,
    extra_widget: Option<Widget>,
    filter: Option<FileFilter>,
    local_only: Option<bool>,
    preview_widget: Option<Widget>,
    preview_widget_active: Option<bool>,
    select_multiple: Option<bool>,
    show_hidden: Option<bool>,
    use_preview_label: Option<bool>,
}

impl FileChooserDialogBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn build(self) -> FileChooserDialog {
        let mut properties: Vec<(&str, &dyn ToValue)> = vec![];
        if let Some(ref use_header_bar) = self.use_header_bar {
            properties.push(("use-header-bar", use_header_bar));
        }
        if let Some(ref accept_focus) = self.accept_focus {
            properties.push(("accept-focus", accept_focus));
        }
        if let Some(ref application) = self.application {
            properties.push(("application", application));
        }
        if let Some(ref attached_to) = self.attached_to {
            properties.push(("attached-to", attached_to));
        }
        if let Some(ref decorated) = self.decorated {
            properties.push(("decorated", decorated));
        }
        if let Some(ref default_height) = self.default_height {
            properties.push(("default-height", default_height));
        }
        if let Some(ref default_width) = self.default_width {
            properties.push(("default-width", default_width));
        }
        if let Some(ref deletable) = self.deletable {
            properties.push(("deletable", deletable));
        }
        if let Some(ref destroy_with_parent) = self.destroy_with_parent {
            properties.push(("destroy-with-parent", destroy_with_parent));
        }
        if let Some(ref focus_on_map) = self.focus_on_map {
            properties.push(("focus-on-map", focus_on_map));
        }
        if let Some(ref focus_visible) = self.focus_visible {
            properties.push(("focus-visible", focus_visible));
        }
        if let Some(ref gravity) = self.gravity {
            properties.push(("gravity", gravity));
        }
        if let Some(ref hide_titlebar_when_maximized) = self.hide_titlebar_when_maximized {
            properties.push(("hide-titlebar-when-maximized", hide_titlebar_when_maximized));
        }
        if let Some(ref icon) = self.icon {
            properties.push(("icon", icon));
        }
        if let Some(ref icon_name) = self.icon_name {
            properties.push(("icon-name", icon_name));
        }
        if let Some(ref mnemonics_visible) = self.mnemonics_visible {
            properties.push(("mnemonics-visible", mnemonics_visible));
        }
        if let Some(ref modal) = self.modal {
            properties.push(("modal", modal));
        }
        if let Some(ref resizable) = self.resizable {
            properties.push(("resizable", resizable));
        }
        if let Some(ref role) = self.role {
            properties.push(("role", role));
        }
        if let Some(ref screen) = self.screen {
            properties.push(("screen", screen));
        }
        if let Some(ref skip_pager_hint) = self.skip_pager_hint {
            properties.push(("skip-pager-hint", skip_pager_hint));
        }
        if let Some(ref skip_taskbar_hint) = self.skip_taskbar_hint {
            properties.push(("skip-taskbar-hint", skip_taskbar_hint));
        }
        if let Some(ref startup_id) = self.startup_id {
            properties.push(("startup-id", startup_id));
        }
        if let Some(ref title) = self.title {
            properties.push(("title", title));
        }
        if let Some(ref transient_for) = self.transient_for {
            properties.push(("transient-for", transient_for));
        }
        if let Some(ref type_) = self.type_ {
            properties.push(("type", type_));
        }
        if let Some(ref type_hint) = self.type_hint {
            properties.push(("type-hint", type_hint));
        }
        if let Some(ref urgency_hint) = self.urgency_hint {
            properties.push(("urgency-hint", urgency_hint));
        }
        if let Some(ref window_position) = self.window_position {
            properties.push(("window-position", window_position));
        }
        if let Some(ref border_width) = self.border_width {
            properties.push(("border-width", border_width));
        }
        if let Some(ref child) = self.child {
            properties.push(("child", child));
        }
        if let Some(ref resize_mode) = self.resize_mode {
            properties.push(("resize-mode", resize_mode));
        }
        if let Some(ref app_paintable) = self.app_paintable {
            properties.push(("app-paintable", app_paintable));
        }
        if let Some(ref can_default) = self.can_default {
            properties.push(("can-default", can_default));
        }
        if let Some(ref can_focus) = self.can_focus {
            properties.push(("can-focus", can_focus));
        }
        if let Some(ref events) = self.events {
            properties.push(("events", events));
        }
        if let Some(ref expand) = self.expand {
            properties.push(("expand", expand));
        }
        #[cfg(any(feature = "v3_20", feature = "dox"))]
        if let Some(ref focus_on_click) = self.focus_on_click {
            properties.push(("focus-on-click", focus_on_click));
        }
        if let Some(ref halign) = self.halign {
            properties.push(("halign", halign));
        }
        if let Some(ref has_default) = self.has_default {
            properties.push(("has-default", has_default));
        }
        if let Some(ref has_focus) = self.has_focus {
            properties.push(("has-focus", has_focus));
        }
        if let Some(ref has_tooltip) = self.has_tooltip {
            properties.push(("has-tooltip", has_tooltip));
        }
        if let Some(ref height_request) = self.height_request {
            properties.push(("height-request", height_request));
        }
        if let Some(ref hexpand) = self.hexpand {
            properties.push(("hexpand", hexpand));
        }
        if let Some(ref hexpand_set) = self.hexpand_set {
            properties.push(("hexpand-set", hexpand_set));
        }
        if let Some(ref is_focus) = self.is_focus {
            properties.push(("is-focus", is_focus));
        }
        if let Some(ref margin) = self.margin {
            properties.push(("margin", margin));
        }
        if let Some(ref margin_bottom) = self.margin_bottom {
            properties.push(("margin-bottom", margin_bottom));
        }
        if let Some(ref margin_end) = self.margin_end {
            properties.push(("margin-end", margin_end));
        }
        if let Some(ref margin_start) = self.margin_start {
            properties.push(("margin-start", margin_start));
        }
        if let Some(ref margin_top) = self.margin_top {
            properties.push(("margin-top", margin_top));
        }
        if let Some(ref name) = self.name {
            properties.push(("name", name));
        }
        if let Some(ref no_show_all) = self.no_show_all {
            properties.push(("no-show-all", no_show_all));
        }
        if let Some(ref opacity) = self.opacity {
            properties.push(("opacity", opacity));
        }
        if let Some(ref parent) = self.parent {
            properties.push(("parent", parent));
        }
        if let Some(ref receives_default) = self.receives_default {
            properties.push(("receives-default", receives_default));
        }
        if let Some(ref sensitive) = self.sensitive {
            properties.push(("sensitive", sensitive));
        }
        if let Some(ref tooltip_markup) = self.tooltip_markup {
            properties.push(("tooltip-markup", tooltip_markup));
        }
        if let Some(ref tooltip_text) = self.tooltip_text {
            properties.push(("tooltip-text", tooltip_text));
        }
        if let Some(ref valign) = self.valign {
            properties.push(("valign", valign));
        }
        if let Some(ref vexpand) = self.vexpand {
            properties.push(("vexpand", vexpand));
        }
        if let Some(ref vexpand_set) = self.vexpand_set {
            properties.push(("vexpand-set", vexpand_set));
        }
        if let Some(ref visible) = self.visible {
            properties.push(("visible", visible));
        }
        if let Some(ref width_request) = self.width_request {
            properties.push(("width-request", width_request));
        }
        if let Some(ref action) = self.action {
            properties.push(("action", action));
        }
        if let Some(ref create_folders) = self.create_folders {
            properties.push(("create-folders", create_folders));
        }
        if let Some(ref do_overwrite_confirmation) = self.do_overwrite_confirmation {
            properties.push(("do-overwrite-confirmation", do_overwrite_confirmation));
        }
        if let Some(ref extra_widget) = self.extra_widget {
            properties.push(("extra-widget", extra_widget));
        }
        if let Some(ref filter) = self.filter {
            properties.push(("filter", filter));
        }
        if let Some(ref local_only) = self.local_only {
            properties.push(("local-only", local_only));
        }
        if let Some(ref preview_widget) = self.preview_widget {
            properties.push(("preview-widget", preview_widget));
        }
        if let Some(ref preview_widget_active) = self.preview_widget_active {
            properties.push(("preview-widget-active", preview_widget_active));
        }
        if let Some(ref select_multiple) = self.select_multiple {
            properties.push(("select-multiple", select_multiple));
        }
        if let Some(ref show_hidden) = self.show_hidden {
            properties.push(("show-hidden", show_hidden));
        }
        if let Some(ref use_preview_label) = self.use_preview_label {
            properties.push(("use-preview-label", use_preview_label));
        }
        let ret = glib::Object::new::<FileChooserDialog>(&properties).expect("object new");
        ret
    }

    pub fn use_header_bar(mut self, use_header_bar: i32) -> Self {
        self.use_header_bar = Some(use_header_bar);
        self
    }

    pub fn accept_focus(mut self, accept_focus: bool) -> Self {
        self.accept_focus = Some(accept_focus);
        self
    }

    pub fn application<P: IsA<Application>>(mut self, application: &P) -> Self {
        self.application = Some(application.clone().upcast());
        self
    }

    pub fn attached_to<P: IsA<Widget>>(mut self, attached_to: &P) -> Self {
        self.attached_to = Some(attached_to.clone().upcast());
        self
    }

    pub fn decorated(mut self, decorated: bool) -> Self {
        self.decorated = Some(decorated);
        self
    }

    pub fn default_height(mut self, default_height: i32) -> Self {
        self.default_height = Some(default_height);
        self
    }

    pub fn default_width(mut self, default_width: i32) -> Self {
        self.default_width = Some(default_width);
        self
    }

    pub fn deletable(mut self, deletable: bool) -> Self {
        self.deletable = Some(deletable);
        self
    }

    pub fn destroy_with_parent(mut self, destroy_with_parent: bool) -> Self {
        self.destroy_with_parent = Some(destroy_with_parent);
        self
    }

    pub fn focus_on_map(mut self, focus_on_map: bool) -> Self {
        self.focus_on_map = Some(focus_on_map);
        self
    }

    pub fn focus_visible(mut self, focus_visible: bool) -> Self {
        self.focus_visible = Some(focus_visible);
        self
    }

    pub fn gravity(mut self, gravity: gdk::Gravity) -> Self {
        self.gravity = Some(gravity);
        self
    }

    pub fn hide_titlebar_when_maximized(mut self, hide_titlebar_when_maximized: bool) -> Self {
        self.hide_titlebar_when_maximized = Some(hide_titlebar_when_maximized);
        self
    }

    pub fn icon(mut self, icon: &gdk_pixbuf::Pixbuf) -> Self {
        self.icon = Some(icon.clone());
        self
    }

    pub fn icon_name(mut self, icon_name: &str) -> Self {
        self.icon_name = Some(icon_name.to_string());
        self
    }

    pub fn mnemonics_visible(mut self, mnemonics_visible: bool) -> Self {
        self.mnemonics_visible = Some(mnemonics_visible);
        self
    }

    pub fn modal(mut self, modal: bool) -> Self {
        self.modal = Some(modal);
        self
    }

    pub fn resizable(mut self, resizable: bool) -> Self {
        self.resizable = Some(resizable);
        self
    }

    pub fn role(mut self, role: &str) -> Self {
        self.role = Some(role.to_string());
        self
    }

    pub fn screen(mut self, screen: &gdk::Screen) -> Self {
        self.screen = Some(screen.clone());
        self
    }

    pub fn skip_pager_hint(mut self, skip_pager_hint: bool) -> Self {
        self.skip_pager_hint = Some(skip_pager_hint);
        self
    }

    pub fn skip_taskbar_hint(mut self, skip_taskbar_hint: bool) -> Self {
        self.skip_taskbar_hint = Some(skip_taskbar_hint);
        self
    }

    pub fn startup_id(mut self, startup_id: &str) -> Self {
        self.startup_id = Some(startup_id.to_string());
        self
    }

    pub fn title(mut self, title: &str) -> Self {
        self.title = Some(title.to_string());
        self
    }

    pub fn transient_for<P: IsA<Window>>(mut self, transient_for: &P) -> Self {
        self.transient_for = Some(transient_for.clone().upcast());
        self
    }

    pub fn type_(mut self, type_: WindowType) -> Self {
        self.type_ = Some(type_);
        self
    }

    pub fn type_hint(mut self, type_hint: gdk::WindowTypeHint) -> Self {
        self.type_hint = Some(type_hint);
        self
    }

    pub fn urgency_hint(mut self, urgency_hint: bool) -> Self {
        self.urgency_hint = Some(urgency_hint);
        self
    }

    pub fn window_position(mut self, window_position: WindowPosition) -> Self {
        self.window_position = Some(window_position);
        self
    }

    pub fn border_width(mut self, border_width: u32) -> Self {
        self.border_width = Some(border_width);
        self
    }

    pub fn child<P: IsA<Widget>>(mut self, child: &P) -> Self {
        self.child = Some(child.clone().upcast());
        self
    }

    pub fn resize_mode(mut self, resize_mode: ResizeMode) -> Self {
        self.resize_mode = Some(resize_mode);
        self
    }

    pub fn app_paintable(mut self, app_paintable: bool) -> Self {
        self.app_paintable = Some(app_paintable);
        self
    }

    pub fn can_default(mut self, can_default: bool) -> Self {
        self.can_default = Some(can_default);
        self
    }

    pub fn can_focus(mut self, can_focus: bool) -> Self {
        self.can_focus = Some(can_focus);
        self
    }

    pub fn events(mut self, events: gdk::EventMask) -> Self {
        self.events = Some(events);
        self
    }

    pub fn expand(mut self, expand: bool) -> Self {
        self.expand = Some(expand);
        self
    }

    #[cfg(any(feature = "v3_20", feature = "dox"))]
    #[cfg_attr(feature = "dox", doc(cfg(feature = "v3_20")))]
    pub fn focus_on_click(mut self, focus_on_click: bool) -> Self {
        self.focus_on_click = Some(focus_on_click);
        self
    }

    pub fn halign(mut self, halign: Align) -> Self {
        self.halign = Some(halign);
        self
    }

    pub fn has_default(mut self, has_default: bool) -> Self {
        self.has_default = Some(has_default);
        self
    }

    pub fn has_focus(mut self, has_focus: bool) -> Self {
        self.has_focus = Some(has_focus);
        self
    }

    pub fn has_tooltip(mut self, has_tooltip: bool) -> Self {
        self.has_tooltip = Some(has_tooltip);
        self
    }

    pub fn height_request(mut self, height_request: i32) -> Self {
        self.height_request = Some(height_request);
        self
    }

    pub fn hexpand(mut self, hexpand: bool) -> Self {
        self.hexpand = Some(hexpand);
        self
    }

    pub fn hexpand_set(mut self, hexpand_set: bool) -> Self {
        self.hexpand_set = Some(hexpand_set);
        self
    }

    pub fn is_focus(mut self, is_focus: bool) -> Self {
        self.is_focus = Some(is_focus);
        self
    }

    pub fn margin(mut self, margin: i32) -> Self {
        self.margin = Some(margin);
        self
    }

    pub fn margin_bottom(mut self, margin_bottom: i32) -> Self {
        self.margin_bottom = Some(margin_bottom);
        self
    }

    pub fn margin_end(mut self, margin_end: i32) -> Self {
        self.margin_end = Some(margin_end);
        self
    }

    pub fn margin_start(mut self, margin_start: i32) -> Self {
        self.margin_start = Some(margin_start);
        self
    }

    pub fn margin_top(mut self, margin_top: i32) -> Self {
        self.margin_top = Some(margin_top);
        self
    }

    pub fn name(mut self, name: &str) -> Self {
        self.name = Some(name.to_string());
        self
    }

    pub fn no_show_all(mut self, no_show_all: bool) -> Self {
        self.no_show_all = Some(no_show_all);
        self
    }

    pub fn opacity(mut self, opacity: f64) -> Self {
        self.opacity = Some(opacity);
        self
    }

    pub fn parent<P: IsA<Container>>(mut self, parent: &P) -> Self {
        self.parent = Some(parent.clone().upcast());
        self
    }

    pub fn receives_default(mut self, receives_default: bool) -> Self {
        self.receives_default = Some(receives_default);
        self
    }

    pub fn sensitive(mut self, sensitive: bool) -> Self {
        self.sensitive = Some(sensitive);
        self
    }

    pub fn tooltip_markup(mut self, tooltip_markup: &str) -> Self {
        self.tooltip_markup = Some(tooltip_markup.to_string());
        self
    }

    pub fn tooltip_text(mut self, tooltip_text: &str) -> Self {
        self.tooltip_text = Some(tooltip_text.to_string());
        self
    }

    pub fn valign(mut self, valign: Align) -> Self {
        self.valign = Some(valign);
        self
    }

    pub fn vexpand(mut self, vexpand: bool) -> Self {
        self.vexpand = Some(vexpand);
        self
    }

    pub fn vexpand_set(mut self, vexpand_set: bool) -> Self {
        self.vexpand_set = Some(vexpand_set);
        self
    }

    pub fn visible(mut self, visible: bool) -> Self {
        self.visible = Some(visible);
        self
    }

    pub fn width_request(mut self, width_request: i32) -> Self {
        self.width_request = Some(width_request);
        self
    }

    pub fn action(mut self, action: FileChooserAction) -> Self {
        self.action = Some(action);
        self
    }

    pub fn create_folders(mut self, create_folders: bool) -> Self {
        self.create_folders = Some(create_folders);
        self
    }

    pub fn do_overwrite_confirmation(mut self, do_overwrite_confirmation: bool) -> Self {
        self.do_overwrite_confirmation = Some(do_overwrite_confirmation);
        self
    }

    pub fn extra_widget<P: IsA<Widget>>(mut self, extra_widget: &P) -> Self {
        self.extra_widget = Some(extra_widget.clone().upcast());
        self
    }

    pub fn filter(mut self, filter: &FileFilter) -> Self {
        self.filter = Some(filter.clone());
        self
    }

    pub fn local_only(mut self, local_only: bool) -> Self {
        self.local_only = Some(local_only);
        self
    }

    pub fn preview_widget<P: IsA<Widget>>(mut self, preview_widget: &P) -> Self {
        self.preview_widget = Some(preview_widget.clone().upcast());
        self
    }

    pub fn preview_widget_active(mut self, preview_widget_active: bool) -> Self {
        self.preview_widget_active = Some(preview_widget_active);
        self
    }

    pub fn select_multiple(mut self, select_multiple: bool) -> Self {
        self.select_multiple = Some(select_multiple);
        self
    }

    pub fn show_hidden(mut self, show_hidden: bool) -> Self {
        self.show_hidden = Some(show_hidden);
        self
    }

    pub fn use_preview_label(mut self, use_preview_label: bool) -> Self {
        self.use_preview_label = Some(use_preview_label);
        self
    }
}

pub const NONE_FILE_CHOOSER_DIALOG: Option<&FileChooserDialog> = None;

impl fmt::Display for FileChooserDialog {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("FileChooserDialog")
    }
}
