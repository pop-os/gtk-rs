// This file was generated by gir (https://github.com/gtk-rs/gir)
// from gir-files (https://github.com/gtk-rs/gir-files)
// DO NOT EDIT

use crate::AsyncResult;
use crate::Cancellable;
use crate::FileInfo;
use crate::IOStream;
use crate::Seekable;
use glib::object::IsA;
use glib::translate::*;
use std::boxed::Box as Box_;
use std::fmt;
use std::pin::Pin;
use std::ptr;

glib::wrapper! {
    pub struct FileIOStream(Object<ffi::GFileIOStream, ffi::GFileIOStreamClass>) @extends IOStream, @implements Seekable;

    match fn {
        get_type => || ffi::g_file_io_stream_get_type(),
    }
}

pub const NONE_FILE_IO_STREAM: Option<&FileIOStream> = None;

pub trait FileIOStreamExt: 'static {
    fn get_etag(&self) -> Option<glib::GString>;

    fn query_info<P: IsA<Cancellable>>(
        &self,
        attributes: &str,
        cancellable: Option<&P>,
    ) -> Result<FileInfo, glib::Error>;

    fn query_info_async<
        P: IsA<Cancellable>,
        Q: FnOnce(Result<FileInfo, glib::Error>) + Send + 'static,
    >(
        &self,
        attributes: &str,
        io_priority: glib::Priority,
        cancellable: Option<&P>,
        callback: Q,
    );

    fn query_info_async_future(
        &self,
        attributes: &str,
        io_priority: glib::Priority,
    ) -> Pin<Box_<dyn std::future::Future<Output = Result<FileInfo, glib::Error>> + 'static>>;
}

impl<O: IsA<FileIOStream>> FileIOStreamExt for O {
    fn get_etag(&self) -> Option<glib::GString> {
        unsafe {
            from_glib_full(ffi::g_file_io_stream_get_etag(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn query_info<P: IsA<Cancellable>>(
        &self,
        attributes: &str,
        cancellable: Option<&P>,
    ) -> Result<FileInfo, glib::Error> {
        unsafe {
            let mut error = ptr::null_mut();
            let ret = ffi::g_file_io_stream_query_info(
                self.as_ref().to_glib_none().0,
                attributes.to_glib_none().0,
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

    fn query_info_async<
        P: IsA<Cancellable>,
        Q: FnOnce(Result<FileInfo, glib::Error>) + Send + 'static,
    >(
        &self,
        attributes: &str,
        io_priority: glib::Priority,
        cancellable: Option<&P>,
        callback: Q,
    ) {
        let user_data: Box_<Q> = Box_::new(callback);
        unsafe extern "C" fn query_info_async_trampoline<
            Q: FnOnce(Result<FileInfo, glib::Error>) + Send + 'static,
        >(
            _source_object: *mut glib::gobject_ffi::GObject,
            res: *mut crate::ffi::GAsyncResult,
            user_data: glib::ffi::gpointer,
        ) {
            let mut error = ptr::null_mut();
            let ret =
                ffi::g_file_io_stream_query_info_finish(_source_object as *mut _, res, &mut error);
            let result = if error.is_null() {
                Ok(from_glib_full(ret))
            } else {
                Err(from_glib_full(error))
            };
            let callback: Box_<Q> = Box_::from_raw(user_data as *mut _);
            callback(result);
        }
        let callback = query_info_async_trampoline::<Q>;
        unsafe {
            ffi::g_file_io_stream_query_info_async(
                self.as_ref().to_glib_none().0,
                attributes.to_glib_none().0,
                io_priority.to_glib(),
                cancellable.map(|p| p.as_ref()).to_glib_none().0,
                Some(callback),
                Box_::into_raw(user_data) as *mut _,
            );
        }
    }

    fn query_info_async_future(
        &self,
        attributes: &str,
        io_priority: glib::Priority,
    ) -> Pin<Box_<dyn std::future::Future<Output = Result<FileInfo, glib::Error>> + 'static>> {
        let attributes = String::from(attributes);
        Box_::pin(crate::GioFuture::new(self, move |obj, send| {
            let cancellable = Cancellable::new();
            obj.query_info_async(&attributes, io_priority, Some(&cancellable), move |res| {
                send.resolve(res);
            });

            cancellable
        }))
    }
}

impl fmt::Display for FileIOStream {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("FileIOStream")
    }
}
