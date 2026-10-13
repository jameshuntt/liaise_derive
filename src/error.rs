//! The derive's own diagnostics, as liaise codes. `liaise_derive` cannot depend
//! on `liaise` (liaise depends on this crate), so the table comes from
//! `liaise_proc_macro`, which renders exactly as liaise does.

use liaise_proc_macro::liaise_codes;
use quote::ToTokens;

liaise_codes! {
    prefix: "LIAISE",
    pub(crate) E {
        OnlyEnums      = 1 => "LiaiseCodes can only be derived for enums",
        MissingCode    = 2 => "missing liaise(code = <u16>) on variant",
        MissingMessage = 3 => "missing liaise(msg = \"...\") or liaise(error = \"...\") on variant",
        BadCode        = 4 => "liaise(code = <u16 integer literal or const>) expected",
        BadString      = 5 => "liaise string literal expected",
        BadBool        = 6 => "liaise(urgent = true|false) expected",
        Unsupported    = 7 => "unsupported #[liaise(...)] item",
        Source         = 8 => "`source` is only valid on a single-field tuple variant like `Io(std::io::Error)`",
    }
}

pub(crate) fn errors<T: ToTokens>(t: T, e: E) -> syn::Error {
    liaise_proc_macro::err_spanned(t, e)
}
