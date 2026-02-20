use proc_macro::TokenStream;
// use quote::quote;
// use syn::{parse_macro_input, DeriveInput, Data};

fn parse_error_prefix(attrs: &[syn::Attribute]) -> syn::Result<String> {
    for attr in attrs {
        if !attr.path().is_ident("error_prefix") {
            continue;
        }

        // Accept: #[error_prefix("ABUT")]
        if let syn::Meta::List(list) = &attr.meta {
            let lit: syn::LitStr = syn::parse2(list.tokens.clone())?;
            return Ok(lit.value());
        }

        // Accept: #[error_prefix = "ABUT"]
        if let syn::Meta::NameValue(nv) = &attr.meta {
            if let syn::Expr::Lit(expr_lit) = &nv.value {
                if let syn::Lit::Str(lit) = &expr_lit.lit {
                    return Ok(lit.value());
                }
            }
            return Err(syn::Error::new_spanned(
                nv,
                "expected #[error_prefix = \"ABUT\"]",
            ));
        }

        return Err(syn::Error::new_spanned(
            attr,
            "expected #[error_prefix(\"ABUT\")] or #[error_prefix = \"ABUT\"]",
        ));
    }

    Ok("ERR".to_string())
}

use quote::{quote, ToTokens};
use syn::{
    parse_macro_input, spanned::Spanned, Data, DeriveInput, Expr, Fields, Lit, Meta, MetaNameValue,
    Token,
};
use syn::punctuated::Punctuated;


#[derive(Default)]
struct LiaiseVariantAttr {
    code: Option<u16>,
    msg: Option<syn::LitStr>,
    source: bool,
}

fn parse_liaise_variant_attr(attrs: &[syn::Attribute]) -> syn::Result<LiaiseVariantAttr> {
    let mut out = LiaiseVariantAttr::default();

    for attr in attrs {
        if !attr.path().is_ident("liaise") {
            continue;
        }

        // #[liaise(code = 1, msg = "...", source)]
        let items: Punctuated<Meta, Token![,]> =
            attr.parse_args_with(Punctuated::parse_terminated)?;

        for meta in items {
            match meta {
                Meta::NameValue(MetaNameValue { path, value, .. }) if path.is_ident("code") => {
                    let lit = match value {
                        Expr::Lit(el) => el.lit,
                        other => {
                            return Err(syn::Error::new_spanned(
                                other,
                                "liaise(code = <u16 integer literal>) expected",
                            ));
                        }
                    };
                    let n = match lit {
                        Lit::Int(li) => li.base10_parse::<u16>()?,
                        other => {
                            return Err(syn::Error::new_spanned(
                                other,
                                "liaise(code = <u16 integer literal>) expected",
                            ));
                        }
                    };
                    out.code = Some(n);
                }

                Meta::NameValue(MetaNameValue { path, value, .. })
                    if path.is_ident("msg") || path.is_ident("message") =>
                {
                    let lit = match value {
                        Expr::Lit(el) => el.lit,
                        other => {
                            return Err(syn::Error::new_spanned(
                                other,
                                "liaise(msg = \"...\") expected",
                            ));
                        }
                    };
                    let s = match lit {
                        Lit::Str(ls) => ls,
                        other => {
                            return Err(syn::Error::new_spanned(other, "liaise(msg = \"...\") expected"));
                        }
                    };
                    out.msg = Some(s);
                }

                Meta::Path(p) if p.is_ident("source") => {
                    out.source = true;
                }

                other => {
                    return Err(syn::Error::new_spanned(
                        other,
                        "unsupported #[liaise(...)] item; expected code=, msg=, and/or source",
                    ));
                }
            }
        }
    }

    Ok(out)
}

#[derive(Default)]
struct LiaiseMeta {
    code: Option<u16>,
    msg: Option<syn::LitStr>,
}

fn parse_liaise_meta(attrs: &[syn::Attribute]) -> syn::Result<LiaiseMeta> {
    let mut out = LiaiseMeta::default();

    for attr in attrs {
        if !attr.path().is_ident("liaise") {
            continue;
        }

        // #[liaise(code = 1, msg = "...")]
        let items: Punctuated<Meta, Token![,]> =
            attr.parse_args_with(Punctuated::parse_terminated)?;

        for meta in items {
            match meta {
                Meta::NameValue(MetaNameValue { path, value, .. }) if path.is_ident("code") => {
                    let lit = match value {
                        Expr::Lit(el) => el.lit,
                        other => {
                            return Err(syn::Error::new_spanned(
                                other,
                                "liaise(code = <u16 integer literal>) expected",
                            ));
                        }
                    };
                    let n = match lit {
                        Lit::Int(li) => li.base10_parse::<u16>()?,
                        other => {
                            return Err(syn::Error::new_spanned(
                                other,
                                "liaise(code = <u16 integer literal>) expected",
                            ));
                        }
                    };
                    out.code = Some(n);
                }

                Meta::NameValue(MetaNameValue { path, value, .. })
                    if path.is_ident("msg") || path.is_ident("message") =>
                {
                    let lit = match value {
                        Expr::Lit(el) => el.lit,
                        other => {
                            return Err(syn::Error::new_spanned(
                                other,
                                "liaise(msg = \"...\") expected",
                            ));
                        }
                    };
                    let s = match lit {
                        Lit::Str(ls) => ls,
                        other => {
                            return Err(syn::Error::new_spanned(
                                other,
                                "liaise(msg = \"...\") expected",
                            ));
                        }
                    };
                    out.msg = Some(s);
                }

                other => {
                    return Err(syn::Error::new_spanned(
                        other,
                        "unsupported #[liaise(...)] item; expected code= and msg=",
                    ));
                }
            }
        }
    }

    Ok(out)
}

#[proc_macro_derive(RegisterErrors, attributes(error_prefix))]
pub fn derive_register_errors(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    let prefix = input
        .attrs
        .iter()
        .find(|a| a.path().is_ident("error_prefix"))
        .and_then(|a| a.parse_args::<syn::LitStr>().ok())
        .map(|s| s.value())
        .unwrap_or_else(|| "ERR".to_string());

    let variants = if let Data::Enum(data) = &input.data {
        &data.variants
    } else {
        return syn::Error::new_spanned(name, "RegisterErrors can only be derived for enums")
            .to_compile_error()
            .into();
    };

    let variant_idents: Vec<_> = variants.iter().map(|v| &v.ident).collect();

    let expanded = quote! {
        impl ::liaise::ErrorRegistry for #name {
            const PREFIX: &'static str = #prefix;
            const ALL_CODES: &'static [u16] = &[
                #( #name::#variant_idents as u16 ),*
            ];
        }

        const _: () = <#name as ::liaise::ErrorRegistry>::VALIDATE;
    };

    TokenStream::from(expanded)
}

#[proc_macro_derive(LiaiseCodes, attributes(error_prefix, liaise))]
pub fn derive_liaise_codes(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    let prefix = match parse_error_prefix(&input.attrs) {
        Ok(p) => p,
        Err(e) => return e.to_compile_error().into(),
    };

    let variants = match &input.data {
        Data::Enum(d) => &d.variants,
        _ => {
            return syn::Error::new_spanned(name, "LiaiseCodes can only be derived for enums")
                .to_compile_error()
                .into();
        }
    };

    struct Info {
        ident: syn::Ident,
        fields: Fields,
        code: u16,
        msg: syn::LitStr,
        span: proc_macro2::Span,
    }

    let mut infos = Vec::<Info>::new();

    for v in variants {
        let meta = match parse_liaise_meta(&v.attrs) {
            Ok(m) => m,
            Err(e) => return e.to_compile_error().into(),
        };

        let code = match meta.code {
            Some(c) => c,
            None => {
                return syn::Error::new(v.span(), "missing #[liaise(code = ...)] on variant")
                    .to_compile_error()
                    .into();
            }
        };

        let msg = match meta.msg {
            Some(m) => m,
            None => {
                return syn::Error::new(v.span(), "missing #[liaise(msg = \"...\")] on variant")
                    .to_compile_error()
                    .into();
            }
        };

        infos.push(Info {
            ident: v.ident.clone(),
            fields: v.fields.clone(),
            code,
            msg,
            span: v.span(),
        });
    }

    // ALL_CODES
    let all_codes: Vec<u16> = infos.iter().map(|i| i.code).collect();

    // match arms
    let code_arms = infos.iter().map(|i| {
        let vi = &i.ident;
        let code = i.code;
        match &i.fields {
            Fields::Unit => quote! { Self::#vi => #code, },
            Fields::Unnamed(_) => quote! { Self::#vi(..) => #code, },
            Fields::Named(_) => quote! { Self::#vi { .. } => #code, },
        }
    });

    let msg_arms = infos.iter().map(|i| {
        let vi = &i.ident;
        let msg = &i.msg;
        match &i.fields {
            Fields::Unit => quote! { Self::#vi => ::alloc::format!(#msg), },
            // tuple variants can't bind named fields; keep it simple
            Fields::Unnamed(_) => quote! { Self::#vi(..) => ::alloc::format!(#msg), },
            Fields::Named(named) => {
                let binds = named.named.iter().map(|f| f.ident.as_ref().unwrap());
                quote! { Self::#vi { #(#binds),* } => ::alloc::format!(#msg), }
            }
        }
    });

    let expanded = quote! {
        impl ::liaise::ErrorRegistry for #name {
            const PREFIX: &'static str = #prefix;
            const ALL_CODES: &'static [u16] = &[ #(#all_codes),* ];
        }

        const _: () = <#name as ::liaise::ErrorRegistry>::VALIDATE;

        impl ::liaise::Liaise for #name {
            fn code_id(&self) -> u16 {
                match self {
                    #(#code_arms)*
                }
            }

            fn message(&self) -> ::alloc::string::String {
                match self {
                    #(#msg_arms)*
                }
            }
        }

        impl ::core::fmt::Display for #name {
            fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                // Liaise::render takes self by value; your traits require Copy, so this is fine.
                write!(f, "{}", (*self).render())
            }
        }
    };

    TokenStream::from(expanded)
}