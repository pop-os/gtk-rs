// Take a look at the license at the top of the repository in the LICENSE file.

mod downgrade_derive;
mod gboxed_derive;
mod genum_derive;
mod gflags_attribute;
mod utils;

use proc_macro::TokenStream;
use proc_macro_error::proc_macro_error;
use syn::{parse_macro_input, DeriveInput, LitStr};

#[proc_macro_derive(GEnum, attributes(genum))]
#[proc_macro_error]
pub fn genum_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let gen = genum_derive::impl_genum(&input);
    gen.into()
}

/// Derive macro for defining a [`BoxedType`]`::get_type` function and
/// the [`glib::Value`] traits.
///
/// # Example
///
/// ```
/// use glib::prelude::*;
/// use glib::subclass::prelude::*;
///
/// #[derive(Clone, Debug, PartialEq, Eq, glib::GBoxed)]
/// #[gboxed(type_name = "MyBoxed")]
/// struct MyBoxed(String);
/// ```
///
/// [`BoxedType`]: subclass/boxed/trait.BoxedType.html
/// [`glib::Value`]: value/struct.Value.html
#[proc_macro_derive(GBoxed, attributes(gboxed))]
#[proc_macro_error]
pub fn gboxed_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let gen = gboxed_derive::impl_gboxed(&input);
    gen.into()
}

/// Attribute macro for defining flags using the `bitflags` crate.
/// This macro will also define a `GFlags::get_type` function and
/// the [`glib::Value`] traits.
///
/// The expected `GType` name has to be passed as macro attribute.
/// The name and nick of each flag can also be optionally defined.
/// Default name is the flag identifier in CamelCase and default nick
/// is the identifier in kebab-case.
/// Combined flags should not be registered with the `GType` system
/// and so needs to be tagged with the `#[gflags(skip)]` attribute.
///
/// # Example
///
/// ```
/// use glib::prelude::*;
/// use glib::subclass::prelude::*;
///
/// #[glib::gflags("MyFlags")]
/// enum MyFlags {
///     #[gflags(name = "Flag A", nick = "nick-a")]
///     A = 0b00000001,
///     #[gflags(name = "Flag B")]
///     B = 0b00000010,
///     #[gflags(skip)]
///     AB = Self::A.bits() | Self::B.bits(),
///     C = 0b00000100,
/// }
/// ```
///
/// [`glib::Value`]: value/struct.Value.html
#[proc_macro_attribute]
#[proc_macro_error]
pub fn gflags(attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as DeriveInput);
    let gtype_name = parse_macro_input!(attr as LitStr);
    let gen = gflags_attribute::impl_gflags(&input, &gtype_name);
    gen.into()
}

/// Macro for deriving implementations of [`glib::clone::Downgrade`] and
/// [`glib::clone::Upgrade`] traits and a weak type.
///
/// # Examples
///
/// ## New Type Idiom
///
/// ```rust,ignore
/// #[derive(glib::Downgrade)]
/// pub struct FancyLabel(gtk::Label);
///
/// impl FancyLabel {
///     pub fn new(label: &str) -> Self {
///         Self(gtk::LabelBuilder::new().label(label).build())
///     }
///
///     pub fn flip(&self) {
///         self.0.set_angle(180.0 - self.0.get_angle());
///     }
/// }
///
/// let fancy_label = FancyLabel::new("Look at me!");
/// let button = gtk::ButtonBuilder::new().label("Click me!").build();
/// button.connect_clicked(clone!(@weak fancy_label => move || fancy_label.flip()));
/// ```
///
/// ## Generic New Type
///
/// ```rust,ignore
/// #[derive(glib::Downgrade)]
/// pub struct TypedEntry<T>(gtk::Entry, std::marker::PhantomData<T>);
///
/// impl<T: ToString + FromStr> for TypedEntry<T> {
///     // ...
/// }
/// ```
///
/// ## Structures and Enums
///
/// ```rust,ignore
/// #[derive(Clone, glib::Downgrade)]
/// pub struct ControlButtons {
///     pub up: gtk::Button,
///     pub down: gtk::Button,
///     pub left: gtk::Button,
///     pub right: gtk::Button,
/// }
///
/// #[derive(Clone, glib::Downgrade)]
/// pub enum DirectionButton {
///     Left(gtk::Button),
///     Right(gtk::Button),
///     Up(gtk::Button),
///     Down(gtk::Button),
/// }
/// ```
///
/// [`glib::clone::Downgrade`]: clone/trait.Downgrade.html
/// [`glib::clone::Upgrade`]: clone/trait.Upgrade.html
#[proc_macro_derive(Downgrade)]
pub fn downgrade(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    downgrade_derive::impl_downgrade(input)
}
