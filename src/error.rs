use quote::ToTokens;

#[derive(Copy, Clone)]
pub(crate) enum E {
    OnlyEnums,
    MissingCode,
    MissingMessage,
    BadCode,
    BadString,
    BadBool,
    Unsupported,
    Source,
}

pub(crate) fn errors<T: ToTokens>(t: T, e: E) -> syn::Error {
    let msg = match e {
        E::OnlyEnums => "LiaiseCodes can only be derived for enums",
        E::MissingCode => "missing liaise(code = <u16>) on variant",
        E::MissingMessage => "missing liaise(msg = \"...\") or liaise(error = \"...\") on variant",
        E::BadCode => "liaise(code = <u16 integer literal>) expected",
        E::BadString => "liaise string literal expected",
        E::BadBool => "liaise(urgent = true|false) expected",
        E::Unsupported => "unsupported #[liaise(...)] item",
        E::Source => "`source` is only valid on a single-field tuple variant like `Io(std::io::Error)`",
    };
    syn::Error::new_spanned(t, msg)
}
