use quote::ToTokens;
#[allow(non_camel_case_types)]
#[derive(Copy, Clone)]
pub(crate)enum E {
    msg, msg_variant,
    code, code_variant,
    unsupported,
    error_prefix, error_prefix_2,
    #[cfg(feature = "std")]prefix,
    #[cfg(feature = "std")]enum_prefix,
    #[cfg(feature = "std")]source,
    only_enums, 
    only_enums_2,
}
pub(crate) fn errors<T: ToTokens>(t: T, e: E) -> syn::Error {let m = match e {
    E::msg => "liaise(msg = \"...\") expected",
    E::msg_variant => "missing #[liaise(msg = \"...\")] on variant",
    E::code => "liaise(code = <u16 integer literal>) expected",
    E::code_variant => "missing #[liaise(code = ...)] on variant",
    E::unsupported => "unsupported #[liaise(...)] item; expected code=, msg=, and/or source",
    #[cfg(feature = "std")]
    E::prefix => "liaise(prefix = \"...\") expected",
    #[cfg(feature = "std")]
    E::enum_prefix => "on the enum, #[liaise(...)] only supports prefix = \"...\"",
    #[cfg(feature = "std")]
    E::source => "`source` is only valid on single-field tuple variants like `Io(std::io::Error)`",
    E::error_prefix => "expected #[error_prefix = \"...\"]",
    E::error_prefix_2 => "expected #[error_prefix(\"...\")] or #[error_prefix = \"...\"]",
    E::only_enums => "RegisterErrors can only be derived for enums",
    E::only_enums_2 => "LiaiseCodes can only be derived for enums",

};syn::Error::new_spanned(t, m)}
