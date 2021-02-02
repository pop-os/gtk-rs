// This file was generated by gir (https://github.com/gtk-rs/gir)
// from gir-files (https://github.com/gtk-rs/gir-files)
// DO NOT EDIT

use crate::AsyncResult;
use crate::Cancellable;
use crate::SocketConnectable;
use crate::TlsCertificate;
use crate::TlsCertificateFlags;
use crate::TlsDatabaseLookupFlags;
use crate::TlsDatabaseVerifyFlags;
use crate::TlsInteraction;
use glib::object::IsA;
use glib::translate::*;
use std::boxed::Box as Box_;
use std::fmt;
use std::pin::Pin;
use std::ptr;

glib::wrapper! {
    pub struct TlsDatabase(Object<ffi::GTlsDatabase, ffi::GTlsDatabaseClass>);

    match fn {
        get_type => || ffi::g_tls_database_get_type(),
    }
}

pub const NONE_TLS_DATABASE: Option<&TlsDatabase> = None;

pub trait TlsDatabaseExt: 'static {
    fn create_certificate_handle<P: IsA<TlsCertificate>>(
        &self,
        certificate: &P,
    ) -> Option<glib::GString>;

    fn lookup_certificate_for_handle<P: IsA<TlsInteraction>, Q: IsA<Cancellable>>(
        &self,
        handle: &str,
        interaction: Option<&P>,
        flags: TlsDatabaseLookupFlags,
        cancellable: Option<&Q>,
    ) -> Result<Option<TlsCertificate>, glib::Error>;

    fn lookup_certificate_for_handle_async<
        P: IsA<TlsInteraction>,
        Q: IsA<Cancellable>,
        R: FnOnce(Result<TlsCertificate, glib::Error>) + Send + 'static,
    >(
        &self,
        handle: &str,
        interaction: Option<&P>,
        flags: TlsDatabaseLookupFlags,
        cancellable: Option<&Q>,
        callback: R,
    );

    fn lookup_certificate_for_handle_async_future<P: IsA<TlsInteraction> + Clone + 'static>(
        &self,
        handle: &str,
        interaction: Option<&P>,
        flags: TlsDatabaseLookupFlags,
    ) -> Pin<Box_<dyn std::future::Future<Output = Result<TlsCertificate, glib::Error>> + 'static>>;

    fn lookup_certificate_issuer<
        P: IsA<TlsCertificate>,
        Q: IsA<TlsInteraction>,
        R: IsA<Cancellable>,
    >(
        &self,
        certificate: &P,
        interaction: Option<&Q>,
        flags: TlsDatabaseLookupFlags,
        cancellable: Option<&R>,
    ) -> Result<TlsCertificate, glib::Error>;

    fn lookup_certificate_issuer_async<
        P: IsA<TlsCertificate>,
        Q: IsA<TlsInteraction>,
        R: IsA<Cancellable>,
        S: FnOnce(Result<TlsCertificate, glib::Error>) + Send + 'static,
    >(
        &self,
        certificate: &P,
        interaction: Option<&Q>,
        flags: TlsDatabaseLookupFlags,
        cancellable: Option<&R>,
        callback: S,
    );

    fn lookup_certificate_issuer_async_future<
        P: IsA<TlsCertificate> + Clone + 'static,
        Q: IsA<TlsInteraction> + Clone + 'static,
    >(
        &self,
        certificate: &P,
        interaction: Option<&Q>,
        flags: TlsDatabaseLookupFlags,
    ) -> Pin<Box_<dyn std::future::Future<Output = Result<TlsCertificate, glib::Error>> + 'static>>;

    fn lookup_certificates_issued_by<P: IsA<TlsInteraction>, Q: IsA<Cancellable>>(
        &self,
        issuer_raw_dn: &glib::ByteArray,
        interaction: Option<&P>,
        flags: TlsDatabaseLookupFlags,
        cancellable: Option<&Q>,
    ) -> Result<Vec<TlsCertificate>, glib::Error>;

    fn lookup_certificates_issued_by_async<
        P: IsA<TlsInteraction>,
        Q: IsA<Cancellable>,
        R: FnOnce(Result<Vec<TlsCertificate>, glib::Error>) + Send + 'static,
    >(
        &self,
        issuer_raw_dn: &glib::ByteArray,
        interaction: Option<&P>,
        flags: TlsDatabaseLookupFlags,
        cancellable: Option<&Q>,
        callback: R,
    );

    fn lookup_certificates_issued_by_async_future<P: IsA<TlsInteraction> + Clone + 'static>(
        &self,
        issuer_raw_dn: &glib::ByteArray,
        interaction: Option<&P>,
        flags: TlsDatabaseLookupFlags,
    ) -> Pin<
        Box_<dyn std::future::Future<Output = Result<Vec<TlsCertificate>, glib::Error>> + 'static>,
    >;

    fn verify_chain<
        P: IsA<TlsCertificate>,
        Q: IsA<SocketConnectable>,
        R: IsA<TlsInteraction>,
        S: IsA<Cancellable>,
    >(
        &self,
        chain: &P,
        purpose: &str,
        identity: Option<&Q>,
        interaction: Option<&R>,
        flags: TlsDatabaseVerifyFlags,
        cancellable: Option<&S>,
    ) -> Result<TlsCertificateFlags, glib::Error>;

    fn verify_chain_async<
        P: IsA<TlsCertificate>,
        Q: IsA<SocketConnectable>,
        R: IsA<TlsInteraction>,
        S: IsA<Cancellable>,
        T: FnOnce(Result<TlsCertificateFlags, glib::Error>) + Send + 'static,
    >(
        &self,
        chain: &P,
        purpose: &str,
        identity: Option<&Q>,
        interaction: Option<&R>,
        flags: TlsDatabaseVerifyFlags,
        cancellable: Option<&S>,
        callback: T,
    );

    fn verify_chain_async_future<
        P: IsA<TlsCertificate> + Clone + 'static,
        Q: IsA<SocketConnectable> + Clone + 'static,
        R: IsA<TlsInteraction> + Clone + 'static,
    >(
        &self,
        chain: &P,
        purpose: &str,
        identity: Option<&Q>,
        interaction: Option<&R>,
        flags: TlsDatabaseVerifyFlags,
    ) -> Pin<
        Box_<dyn std::future::Future<Output = Result<TlsCertificateFlags, glib::Error>> + 'static>,
    >;
}

impl<O: IsA<TlsDatabase>> TlsDatabaseExt for O {
    fn create_certificate_handle<P: IsA<TlsCertificate>>(
        &self,
        certificate: &P,
    ) -> Option<glib::GString> {
        unsafe {
            from_glib_full(ffi::g_tls_database_create_certificate_handle(
                self.as_ref().to_glib_none().0,
                certificate.as_ref().to_glib_none().0,
            ))
        }
    }

    fn lookup_certificate_for_handle<P: IsA<TlsInteraction>, Q: IsA<Cancellable>>(
        &self,
        handle: &str,
        interaction: Option<&P>,
        flags: TlsDatabaseLookupFlags,
        cancellable: Option<&Q>,
    ) -> Result<Option<TlsCertificate>, glib::Error> {
        unsafe {
            let mut error = ptr::null_mut();
            let ret = ffi::g_tls_database_lookup_certificate_for_handle(
                self.as_ref().to_glib_none().0,
                handle.to_glib_none().0,
                interaction.map(|p| p.as_ref()).to_glib_none().0,
                flags.to_glib(),
                cancellable.map(|p| p.as_ref()).to_glib_none().0,
                &mut error,
            );
            if error.is_null() {
                Ok(from_glib_full(ret))
            } else {
                Err(from_glib_full(error))
            }
        }
    }

    fn lookup_certificate_for_handle_async<
        P: IsA<TlsInteraction>,
        Q: IsA<Cancellable>,
        R: FnOnce(Result<TlsCertificate, glib::Error>) + Send + 'static,
    >(
        &self,
        handle: &str,
        interaction: Option<&P>,
        flags: TlsDatabaseLookupFlags,
        cancellable: Option<&Q>,
        callback: R,
    ) {
        let user_data: Box_<R> = Box_::new(callback);
        unsafe extern "C" fn lookup_certificate_for_handle_async_trampoline<
            R: FnOnce(Result<TlsCertificate, glib::Error>) + Send + 'static,
        >(
            _source_object: *mut glib::gobject_ffi::GObject,
            res: *mut crate::ffi::GAsyncResult,
            user_data: glib::ffi::gpointer,
        ) {
            let mut error = ptr::null_mut();
            let ret = ffi::g_tls_database_lookup_certificate_for_handle_finish(
                _source_object as *mut _,
                res,
                &mut error,
            );
            let result = if error.is_null() {
                Ok(from_glib_full(ret))
            } else {
                Err(from_glib_full(error))
            };
            let callback: Box_<R> = Box_::from_raw(user_data as *mut _);
            callback(result);
        }
        let callback = lookup_certificate_for_handle_async_trampoline::<R>;
        unsafe {
            ffi::g_tls_database_lookup_certificate_for_handle_async(
                self.as_ref().to_glib_none().0,
                handle.to_glib_none().0,
                interaction.map(|p| p.as_ref()).to_glib_none().0,
                flags.to_glib(),
                cancellable.map(|p| p.as_ref()).to_glib_none().0,
                Some(callback),
                Box_::into_raw(user_data) as *mut _,
            );
        }
    }

    fn lookup_certificate_for_handle_async_future<P: IsA<TlsInteraction> + Clone + 'static>(
        &self,
        handle: &str,
        interaction: Option<&P>,
        flags: TlsDatabaseLookupFlags,
    ) -> Pin<Box_<dyn std::future::Future<Output = Result<TlsCertificate, glib::Error>> + 'static>>
    {
        let handle = String::from(handle);
        let interaction = interaction.map(ToOwned::to_owned);
        Box_::pin(crate::GioFuture::new(self, move |obj, send| {
            let cancellable = Cancellable::new();
            obj.lookup_certificate_for_handle_async(
                &handle,
                interaction.as_ref().map(::std::borrow::Borrow::borrow),
                flags,
                Some(&cancellable),
                move |res| {
                    send.resolve(res);
                },
            );

            cancellable
        }))
    }

    fn lookup_certificate_issuer<
        P: IsA<TlsCertificate>,
        Q: IsA<TlsInteraction>,
        R: IsA<Cancellable>,
    >(
        &self,
        certificate: &P,
        interaction: Option<&Q>,
        flags: TlsDatabaseLookupFlags,
        cancellable: Option<&R>,
    ) -> Result<TlsCertificate, glib::Error> {
        unsafe {
            let mut error = ptr::null_mut();
            let ret = ffi::g_tls_database_lookup_certificate_issuer(
                self.as_ref().to_glib_none().0,
                certificate.as_ref().to_glib_none().0,
                interaction.map(|p| p.as_ref()).to_glib_none().0,
                flags.to_glib(),
                cancellable.map(|p| p.as_ref()).to_glib_none().0,
                &mut error,
            );
            if error.is_null() {
                Ok(from_glib_full(ret))
            } else {
                Err(from_glib_full(error))
            }
        }
    }

    fn lookup_certificate_issuer_async<
        P: IsA<TlsCertificate>,
        Q: IsA<TlsInteraction>,
        R: IsA<Cancellable>,
        S: FnOnce(Result<TlsCertificate, glib::Error>) + Send + 'static,
    >(
        &self,
        certificate: &P,
        interaction: Option<&Q>,
        flags: TlsDatabaseLookupFlags,
        cancellable: Option<&R>,
        callback: S,
    ) {
        let user_data: Box_<S> = Box_::new(callback);
        unsafe extern "C" fn lookup_certificate_issuer_async_trampoline<
            S: FnOnce(Result<TlsCertificate, glib::Error>) + Send + 'static,
        >(
            _source_object: *mut glib::gobject_ffi::GObject,
            res: *mut crate::ffi::GAsyncResult,
            user_data: glib::ffi::gpointer,
        ) {
            let mut error = ptr::null_mut();
            let ret = ffi::g_tls_database_lookup_certificate_issuer_finish(
                _source_object as *mut _,
                res,
                &mut error,
            );
            let result = if error.is_null() {
                Ok(from_glib_full(ret))
            } else {
                Err(from_glib_full(error))
            };
            let callback: Box_<S> = Box_::from_raw(user_data as *mut _);
            callback(result);
        }
        let callback = lookup_certificate_issuer_async_trampoline::<S>;
        unsafe {
            ffi::g_tls_database_lookup_certificate_issuer_async(
                self.as_ref().to_glib_none().0,
                certificate.as_ref().to_glib_none().0,
                interaction.map(|p| p.as_ref()).to_glib_none().0,
                flags.to_glib(),
                cancellable.map(|p| p.as_ref()).to_glib_none().0,
                Some(callback),
                Box_::into_raw(user_data) as *mut _,
            );
        }
    }

    fn lookup_certificate_issuer_async_future<
        P: IsA<TlsCertificate> + Clone + 'static,
        Q: IsA<TlsInteraction> + Clone + 'static,
    >(
        &self,
        certificate: &P,
        interaction: Option<&Q>,
        flags: TlsDatabaseLookupFlags,
    ) -> Pin<Box_<dyn std::future::Future<Output = Result<TlsCertificate, glib::Error>> + 'static>>
    {
        let certificate = certificate.clone();
        let interaction = interaction.map(ToOwned::to_owned);
        Box_::pin(crate::GioFuture::new(self, move |obj, send| {
            let cancellable = Cancellable::new();
            obj.lookup_certificate_issuer_async(
                &certificate,
                interaction.as_ref().map(::std::borrow::Borrow::borrow),
                flags,
                Some(&cancellable),
                move |res| {
                    send.resolve(res);
                },
            );

            cancellable
        }))
    }

    fn lookup_certificates_issued_by<P: IsA<TlsInteraction>, Q: IsA<Cancellable>>(
        &self,
        issuer_raw_dn: &glib::ByteArray,
        interaction: Option<&P>,
        flags: TlsDatabaseLookupFlags,
        cancellable: Option<&Q>,
    ) -> Result<Vec<TlsCertificate>, glib::Error> {
        unsafe {
            let mut error = ptr::null_mut();
            let ret = ffi::g_tls_database_lookup_certificates_issued_by(
                self.as_ref().to_glib_none().0,
                issuer_raw_dn.to_glib_none().0,
                interaction.map(|p| p.as_ref()).to_glib_none().0,
                flags.to_glib(),
                cancellable.map(|p| p.as_ref()).to_glib_none().0,
                &mut error,
            );
            if error.is_null() {
                Ok(FromGlibPtrContainer::from_glib_full(ret))
            } else {
                Err(from_glib_full(error))
            }
        }
    }

    fn lookup_certificates_issued_by_async<
        P: IsA<TlsInteraction>,
        Q: IsA<Cancellable>,
        R: FnOnce(Result<Vec<TlsCertificate>, glib::Error>) + Send + 'static,
    >(
        &self,
        issuer_raw_dn: &glib::ByteArray,
        interaction: Option<&P>,
        flags: TlsDatabaseLookupFlags,
        cancellable: Option<&Q>,
        callback: R,
    ) {
        let user_data: Box_<R> = Box_::new(callback);
        unsafe extern "C" fn lookup_certificates_issued_by_async_trampoline<
            R: FnOnce(Result<Vec<TlsCertificate>, glib::Error>) + Send + 'static,
        >(
            _source_object: *mut glib::gobject_ffi::GObject,
            res: *mut crate::ffi::GAsyncResult,
            user_data: glib::ffi::gpointer,
        ) {
            let mut error = ptr::null_mut();
            let ret = ffi::g_tls_database_lookup_certificates_issued_by_finish(
                _source_object as *mut _,
                res,
                &mut error,
            );
            let result = if error.is_null() {
                Ok(FromGlibPtrContainer::from_glib_full(ret))
            } else {
                Err(from_glib_full(error))
            };
            let callback: Box_<R> = Box_::from_raw(user_data as *mut _);
            callback(result);
        }
        let callback = lookup_certificates_issued_by_async_trampoline::<R>;
        unsafe {
            ffi::g_tls_database_lookup_certificates_issued_by_async(
                self.as_ref().to_glib_none().0,
                issuer_raw_dn.to_glib_none().0,
                interaction.map(|p| p.as_ref()).to_glib_none().0,
                flags.to_glib(),
                cancellable.map(|p| p.as_ref()).to_glib_none().0,
                Some(callback),
                Box_::into_raw(user_data) as *mut _,
            );
        }
    }

    fn lookup_certificates_issued_by_async_future<P: IsA<TlsInteraction> + Clone + 'static>(
        &self,
        issuer_raw_dn: &glib::ByteArray,
        interaction: Option<&P>,
        flags: TlsDatabaseLookupFlags,
    ) -> Pin<
        Box_<dyn std::future::Future<Output = Result<Vec<TlsCertificate>, glib::Error>> + 'static>,
    > {
        let issuer_raw_dn = issuer_raw_dn.clone();
        let interaction = interaction.map(ToOwned::to_owned);
        Box_::pin(crate::GioFuture::new(self, move |obj, send| {
            let cancellable = Cancellable::new();
            obj.lookup_certificates_issued_by_async(
                &issuer_raw_dn,
                interaction.as_ref().map(::std::borrow::Borrow::borrow),
                flags,
                Some(&cancellable),
                move |res| {
                    send.resolve(res);
                },
            );

            cancellable
        }))
    }

    fn verify_chain<
        P: IsA<TlsCertificate>,
        Q: IsA<SocketConnectable>,
        R: IsA<TlsInteraction>,
        S: IsA<Cancellable>,
    >(
        &self,
        chain: &P,
        purpose: &str,
        identity: Option<&Q>,
        interaction: Option<&R>,
        flags: TlsDatabaseVerifyFlags,
        cancellable: Option<&S>,
    ) -> Result<TlsCertificateFlags, glib::Error> {
        unsafe {
            let mut error = ptr::null_mut();
            let ret = ffi::g_tls_database_verify_chain(
                self.as_ref().to_glib_none().0,
                chain.as_ref().to_glib_none().0,
                purpose.to_glib_none().0,
                identity.map(|p| p.as_ref()).to_glib_none().0,
                interaction.map(|p| p.as_ref()).to_glib_none().0,
                flags.to_glib(),
                cancellable.map(|p| p.as_ref()).to_glib_none().0,
                &mut error,
            );
            if error.is_null() {
                Ok(from_glib(ret))
            } else {
                Err(from_glib_full(error))
            }
        }
    }

    fn verify_chain_async<
        P: IsA<TlsCertificate>,
        Q: IsA<SocketConnectable>,
        R: IsA<TlsInteraction>,
        S: IsA<Cancellable>,
        T: FnOnce(Result<TlsCertificateFlags, glib::Error>) + Send + 'static,
    >(
        &self,
        chain: &P,
        purpose: &str,
        identity: Option<&Q>,
        interaction: Option<&R>,
        flags: TlsDatabaseVerifyFlags,
        cancellable: Option<&S>,
        callback: T,
    ) {
        let user_data: Box_<T> = Box_::new(callback);
        unsafe extern "C" fn verify_chain_async_trampoline<
            T: FnOnce(Result<TlsCertificateFlags, glib::Error>) + Send + 'static,
        >(
            _source_object: *mut glib::gobject_ffi::GObject,
            res: *mut crate::ffi::GAsyncResult,
            user_data: glib::ffi::gpointer,
        ) {
            let mut error = ptr::null_mut();
            let ret =
                ffi::g_tls_database_verify_chain_finish(_source_object as *mut _, res, &mut error);
            let result = if error.is_null() {
                Ok(from_glib(ret))
            } else {
                Err(from_glib_full(error))
            };
            let callback: Box_<T> = Box_::from_raw(user_data as *mut _);
            callback(result);
        }
        let callback = verify_chain_async_trampoline::<T>;
        unsafe {
            ffi::g_tls_database_verify_chain_async(
                self.as_ref().to_glib_none().0,
                chain.as_ref().to_glib_none().0,
                purpose.to_glib_none().0,
                identity.map(|p| p.as_ref()).to_glib_none().0,
                interaction.map(|p| p.as_ref()).to_glib_none().0,
                flags.to_glib(),
                cancellable.map(|p| p.as_ref()).to_glib_none().0,
                Some(callback),
                Box_::into_raw(user_data) as *mut _,
            );
        }
    }

    fn verify_chain_async_future<
        P: IsA<TlsCertificate> + Clone + 'static,
        Q: IsA<SocketConnectable> + Clone + 'static,
        R: IsA<TlsInteraction> + Clone + 'static,
    >(
        &self,
        chain: &P,
        purpose: &str,
        identity: Option<&Q>,
        interaction: Option<&R>,
        flags: TlsDatabaseVerifyFlags,
    ) -> Pin<
        Box_<dyn std::future::Future<Output = Result<TlsCertificateFlags, glib::Error>> + 'static>,
    > {
        let chain = chain.clone();
        let purpose = String::from(purpose);
        let identity = identity.map(ToOwned::to_owned);
        let interaction = interaction.map(ToOwned::to_owned);
        Box_::pin(crate::GioFuture::new(self, move |obj, send| {
            let cancellable = Cancellable::new();
            obj.verify_chain_async(
                &chain,
                &purpose,
                identity.as_ref().map(::std::borrow::Borrow::borrow),
                interaction.as_ref().map(::std::borrow::Borrow::borrow),
                flags,
                Some(&cancellable),
                move |res| {
                    send.resolve(res);
                },
            );

            cancellable
        }))
    }
}

impl fmt::Display for TlsDatabase {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("TlsDatabase")
    }
}
