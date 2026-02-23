/// IMPORTS
use proc_macro::TokenStream;
use quote::quote;
use syn::{
    Data, DeriveInput, Expr, Fields, Lit, Meta,
    MetaNameValue, Path, Token, parse_macro_input,
    punctuated::Punctuated, spanned::Spanned
};
mod error;
use error::{E, errors};

/// MACROS
macro_rules! unwrap_or_report {
    ($opt:expr, $span:expr, $variant:expr) => {
        match $opt {
            Some(val) => val,
            // Pass $variant as an argument, don't try to "call" it
            None => return errors($span, $variant).to_compile_error().into(),
        }
    };
}
#[allow(unused)]macro_rules! unwrap_and_map {
    ($opt:expr, $span:expr, $variant:expr) => {
        match &$opt {
            Some(val) => &val, // Access the field here
            other => return errors(error, $variant)
        }
    };
    ($opt:expr, $span:expr, $variant:expr, $access:ident) => {
        match &$opt {
            Some(val) => val.$access, // Access the field here
            other => return errors(error, $variant)
        }
    };
}

/// HELPERS
fn has_attribute(attr: &Attribute, name: &str) -> bool { is_ident(attr.path(), name) }

fn is_ident(path: &Path, name: &str) -> bool { path.is_ident(name) }

fn parse_lit<T, F>(expr: &syn::Expr, variant: E, parser: F) -> syn::Result<T>
where
    F: FnOnce(&syn::Lit) -> syn::Result<T>,
{
    match expr {
        syn::Expr::Lit(el) => parser(&el.lit),
        other => Err(errors(other, variant)),
    }
}

fn expect_str(expr: &syn::Expr, var: E) -> syn::Result<syn::LitStr> {
    parse_lit(expr, var, |l| if let Lit::Str(s) = l {
        Ok(s.clone())} else { Err(errors(l, var))
    })
}

fn expect_u16(expr: &syn::Expr, var: E) -> syn::Result<u16> {
    parse_lit(expr, var, |l| if let Lit::Int(i) = l {
        i.base10_parse() } else { Err(errors(l, var))
    })
}

/// PARSERS
#[derive(Default)]
struct LiaiseVariantAttr {
    code: Option<u16>,
    msg: Option<syn::LitStr>,
    #[cfg(feature = "std")]
    source: bool,
}

fn parse_liaise_variant_attr(attrs: &[syn::Attribute]) -> syn::Result<LiaiseVariantAttr> {
    let mut out = LiaiseVariantAttr::default();
    for attr in attrs {
        if !has_attribute(attr, "liaise") { continue; }
        // #[liaise(code = 1, msg = "...", source)]
        let items: Punctuated<Meta, Token![,]> =
            attr.parse_args_with(Punctuated::parse_terminated)?;
        for meta in items {
            match meta {
                // Using expect_u16
                Meta::NameValue(nv) if is_ident(&nv.path, "code") => {
                    out.code = Some(expect_u16(&nv.value, E::code)?);
                }
                // Using expect_str
                Meta::NameValue(nv) if is_ident(&nv.path, "msg") || is_ident(&nv.path, "message") => {
                    out.msg = Some(expect_str(&nv.value, E::msg)?);
                }
                #[cfg(feature = "std")]
                Meta::Path(p) if is_ident(&p, "source") => { out.source = true; }
                #[cfg(feature = "std")]
                // IMPORTANT: ignore prefix= on variants if someone accidentally copies it
                Meta::NameValue(MetaNameValue { path, .. }) if is_ident(&path, "prefix") => {}
                other => return Err(errors(other, E::unsupported)),
            }
        }
    } Ok(out)
}

use syn::{Attribute, Result as SynResult};
// 'std' needs Option for the 3-step fallback resolution.
#[cfg(feature = "std")]      type ParserReturn = SynResult<Option<String>>;
// 'no_std' just wants the string result directly.
#[cfg(not(feature = "std"))] type ParserReturn = SynResult<String>;
fn parse_error_prefix(attrs: &[Attribute]) -> ParserReturn {
    // Shared Logic: The loop is the same for both, 
    // we just change what we wrap the return value in.
    for attr in attrs {
        if !has_attribute(attr, "error_prefix") { continue; }
        // Handle #[error_prefix("...")]
        if let syn::Meta::List(list) = &attr.meta {
            let lit: syn::LitStr = syn::parse2(list.tokens.clone())?;
            #[cfg(feature = "std")]        return Ok(Some(lit.value()));
            #[cfg(not(feature = "std"))]   return Ok(lit.value());
        }
        // Handle #[error_prefix = "..."]
        if let syn::Meta::NameValue(nv) = &attr.meta {
            if let syn::Expr::Lit(expr_lit) = &nv.value {
                if let syn::Lit::Str(lit) = &expr_lit.lit {
                    #[cfg(feature = "std")]        return Ok(Some(lit.value()));
                    #[cfg(not(feature = "std"))]   return Ok(lit.value());
                }
            } return Err(errors(nv, E::error_prefix))
        } return Err(errors(attr, E::error_prefix_2));
    }
    // --- FINAL DEFAULT ---
    #[cfg(feature = "std")]       { Ok(None) } // Return None so the caller can check the next fallback
    #[cfg(not(feature = "std"))]  { Ok("ERR".to_string()) } // Return the hard default for no_std
}

#[cfg(feature = "std")]
fn parse_liaise_prefix(attrs: &[Attribute]) -> syn::Result<Option<String>> {
    for attr in attrs {
        if !has_attribute(attr, "liaise") { continue; }

        // #[liaise(prefix = "ABUT")]
        let items: Punctuated<Meta, Token![,]> = attr.parse_args_with(Punctuated::parse_terminated)?;
        for meta in items {
            match meta {
                Meta::NameValue(MetaNameValue { path, value, .. }) if path.is_ident("prefix") => {
                    let lit = match value {
                        Expr::Lit(el) => el.lit,
                        other => return Err(errors(other, E::prefix))
                    };
                    // let s = unwrap_and_map!(lit, v, E::prefix, value());
                    let s = match lit {
                        Lit::Str(ls) => ls.value(),
                        other => return Err(errors(other, E::prefix))
                    };
                    // let s = unwrap_and_map!(lit, v, E::prefix, value());
                    return Ok(Some(s));
                }
                other => return Err(errors(other, E::enum_prefix))
            }
        }
    }

    Ok(None)
}

/// PROC MACROS
#[proc_macro_derive(RegisterErrors, attributes(error_prefix))]
pub fn derive_register_errors(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    let prefix = input
        .attrs
        .iter()
        .find(|a| has_attribute(a, "error_prefix"))
        .and_then(|a| a.parse_args::<syn::LitStr>().ok())
        .map(|s| s.value())
        .unwrap_or_else(|| "ERR".to_string());

    let variants =
        if let Data::Enum(data) = &input.data {
            &data.variants
        } else {
            return errors(name, E::only_enums).to_compile_error().into();
        };

    let variant_idents: Vec<_> = variants
        .iter().map(|v| &v.ident).collect();

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

    // 1. Unified Prefix Resolution
    let prefix = {
        // prefix resolution order:
        // 1) #[liaise(prefix="...")]
        // 2) #[error_prefix("...")] (your old form)
        // 3) "ERR"
        #[cfg(feature = "std")] {
            match parse_liaise_prefix(&input.attrs) {
                Ok(Some(p)) => p,
                Ok(None) => match parse_error_prefix(&input.attrs) {
                    Ok(Some(p)) => p,
                    Ok(None) => "ERR".to_string(),
                    Err(e) => return e.to_compile_error().into(),
                },
                Err(e) => return e.to_compile_error().into(),
            }
        }
        #[cfg(not(feature = "std"))] {
            match parse_error_prefix(&input.attrs) {
                Ok(p) => p,
                Err(e) => return e.to_compile_error().into(),
            }
        }
    };

    let variants = match &input.data {
        Data::Enum(d) => &d.variants,
        _ => return errors(name, E::only_enums_2).to_compile_error().into()
    };

    struct Info {
        ident: syn::Ident,
        fields: Fields,
        code: u16,
        msg: syn::LitStr,
        #[cfg(feature = "std")]
        source: bool,
        #[cfg(feature = "std")]
        source_ty: Option<syn::Type>,
        span: proc_macro2::Span,
    }

    let mut infos = Vec::<Info>::new();

    for v in variants {
        let meta = match parse_liaise_variant_attr(&v.attrs) {
            Ok(m) => m,
            Err(e) => return e.to_compile_error().into(),
        };

        let code    = unwrap_or_report!(meta.code, v, E::code_variant);
        let msg  = unwrap_or_report!(meta.msg,  v, E::msg_variant);
        
        #[cfg(feature = "std")]
        let mut source_ty: Option<syn::Type> = None;

        #[cfg(feature = "std")]
        if meta.source {
            match &v.fields {
                Fields::Unnamed(u) if u.unnamed.len() == 1 => {
                    source_ty = Some(u.unnamed.first().unwrap().ty.clone());
                }
                _ => return errors(v, E::source).to_compile_error().into(),
            }
        }

        #[cfg(feature = "std")]
        infos.push(Info {
            ident: v.ident.clone(),
            fields: v.fields.clone(),
            code,
            msg,
            source: meta.source,
            source_ty,
            span: v.span(),
        });
        
        #[cfg(not(feature = "std"))]
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

    #[cfg(feature = "std")]
    let source_arms = infos.iter().filter_map(|i| {
        if !i.source {
            return None;
        }
        let vi = &i.ident;
        // match ergonomics will bind `src` as a reference because `self` is `&Self`
        Some(quote! { Self::#vi(src) => Some(src), })
    });
    

    #[cfg(feature = "std")]
    let from_impls = infos.iter().filter_map(|i| {
        let vi = &i.ident;
        let ty = i.source_ty.as_ref()?;
        Some(quote! {
            impl From<#ty> for #name {
                fn from(src: #ty) -> Self {
                    Self::#vi(src)
                }
            }
        })
    });

    #[cfg(not(feature = "std"))]
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
    
    #[cfg(feature = "std")]
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
                write!(f, "{}", self.render())
            }
        }

        // impl ::core::error::Error for #name {
        //     fn source(&self) -> Option<&(dyn ::core::error::Error + 'static)> {
        //         match self {
        //             #(#source_arms)*
        //             _ => None,
        //         }
        //     }
        // }

        impl ::std::error::Error for #name {
            fn source(&self) -> Option<&(dyn ::std::error::Error + 'static)> {
                match self {
                    #(#source_arms)*
                    _ => None,
                }
            }
        }

        #(#from_impls)*
    };

    TokenStream::from(expanded)
}
