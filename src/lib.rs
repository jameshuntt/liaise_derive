use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse_macro_input,
    spanned::Spanned,
    Data, DeriveInput,
    Expr,
    Fields,
    Lit,
    Meta,
    MetaNameValue,
    punctuated::Punctuated,
    Token,
};



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
        
        #[cfg(feature = "std")]
        let mut source_ty: Option<syn::Type> = None;

        #[cfg(feature = "std")]
        if meta.source {
            match &v.fields {
                Fields::Unnamed(u) if u.unnamed.len() == 1 => {
                    source_ty = Some(u.unnamed.first().unwrap().ty.clone());
                }
                _ => {
                    return syn::Error::new(
                        v.span(),
                        "`source` is only valid on single-field tuple variants like `Io(std::io::Error)`",
                    )
                    .to_compile_error()
                    .into();
                }
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

        impl ::core::error::Error for #name {
            fn source(&self) -> Option<&(dyn ::core::error::Error + 'static)> {
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





















































// ERRORS
fn msg_error<T: ToTokens>(other: T) -> syn::Error {
    syn::Error::new_spanned(
        other,
        "liaise(msg = \"...\") expected",
    )
}

fn code_error<T: ToTokens>(other: T) -> syn::Error {
    syn::Error::new_spanned(
        other,
        "liaise(code = <u16 integer literal>) expected",
    )
}

fn unsupported_error<T: ToTokens>(other: T) -> syn::Error {
    syn::Error::new_spanned(
        other,
        "unsupported #[liaise(...)] item; expected code=, msg=, and/or source",
    )
}





#[derive(Default)]
struct LiaiseVariantAttr {
    code: Option<u16>,
    msg: Option<syn::LitStr>,
    #[cfg(feature = "std")]
    source: bool,
}

// done i think
fn parse_liaise_variant_attr(attrs: &[syn::Attribute]) -> syn::Result<LiaiseVariantAttr> {
    let mut out = LiaiseVariantAttr::default();

    for attr in attrs {
        if !attr.path().is_ident("liaise") { continue; }

        // #[liaise(code = 1, msg = "...", source)]
        let items: Punctuated<Meta, Token![,]> =
            attr.parse_args_with(Punctuated::parse_terminated)?;

        for meta in items {
            match meta {
                Meta::NameValue(MetaNameValue { path, value, .. }) if path.is_ident("code") => {
                    let lit = match value {
                        Expr::Lit(el) => el.lit,
                        other => return Err(code_error(other)),
                    };
                    let n = match lit {
                        Lit::Int(li) => li.base10_parse::<u16>()?,
                        other => return Err(code_error(other)),
                    };
                    out.code = Some(n);
                }

                Meta::NameValue(MetaNameValue { path, value, .. })
                    if path.is_ident("msg") || path.is_ident("message") =>
                {
                    let lit = match value {
                        Expr::Lit(el) => el.lit,
                        other => return Err(msg_error(other)),
                    };
                    let s = match lit {
                        Lit::Str(ls) => ls,
                        other => return Err(msg_error(other)),
                    };
                    out.msg = Some(s);
                }

                #[cfg(feature = "std")]
                Meta::Path(p) if p.is_ident("source") => {
                    out.source = true;
                }

                #[cfg(feature = "std")]
                // IMPORTANT: ignore prefix= on variants if someone accidentally copies it
                Meta::NameValue(MetaNameValue { path, .. }) if path.is_ident("prefix") => {}

                other => return Err(unsupported_error(other)),

            }
        }
    }

    Ok(out)
}









use syn::{Attribute, Result as SynResult};

// --- CORRECTED GATES ---
// 'std' needs Option for the 3-step fallback resolution.
#[cfg(feature = "std")]
type ParserReturn = SynResult<Option<String>>;

// 'no_std' just wants the string result directly.
#[cfg(not(feature = "std"))]
type ParserReturn = SynResult<String>;

fn parse_error_prefix(attrs: &[Attribute]) -> ParserReturn {
    // Shared Logic: The loop is the same for both, 
    // we just change what we wrap the return value in.
    for attr in attrs {
        if !attr.path().is_ident("error_prefix") {
            continue;
        }

        // Handle #[error_prefix("...")]
        if let syn::Meta::List(list) = &attr.meta {
            let lit: syn::LitStr = syn::parse2(list.tokens.clone())?;
            #[cfg(feature = "std")]
            return Ok(Some(lit.value()));
            #[cfg(not(feature = "std"))]
            return Ok(lit.value());
        }

        // Handle #[error_prefix = "..."]
        if let syn::Meta::NameValue(nv) = &attr.meta {
            if let syn::Expr::Lit(expr_lit) = &nv.value {
                if let syn::Lit::Str(lit) = &expr_lit.lit {
                    #[cfg(feature = "std")]
                    return Ok(Some(lit.value()));
                    #[cfg(not(feature = "std"))]
                    return Ok(lit.value());
                }
            }
            return Err(syn::Error::new_spanned(nv, "expected #[error_prefix = \"ABUT\"]"));
        }

        return Err(syn::Error::new_spanned(
            attr,
            "expected #[error_prefix(\"ABUT\")] or #[error_prefix = \"ABUT\"]",
        ));
    }

    // --- FINAL DEFAULT ---
    #[cfg(feature = "std")]
    { Ok(None) } // Return None so the caller can check the next fallback
    #[cfg(not(feature = "std"))]
    { Ok("ERR".to_string()) } // Return the hard default for no_std
}


#[cfg(feature = "std")]
fn parse_liaise_prefix(attrs: &[Attribute]) -> syn::Result<Option<String>> {
    for attr in attrs {
        if !attr.path().is_ident("liaise") {
            continue;
        }

        // #[liaise(prefix = "ABUT")]
        let items: Punctuated<Meta, Token![,]> = attr.parse_args_with(Punctuated::parse_terminated)?;
        for meta in items {
            match meta {
                Meta::NameValue(MetaNameValue { path, value, .. }) if path.is_ident("prefix") => {
                    let lit = match value {
                        Expr::Lit(el) => el.lit,
                        other => {
                            return Err(syn::Error::new_spanned(
                                other,
                                "liaise(prefix = \"ABUT\") expected",
                            ));
                        }
                    };
                    let s = match lit {
                        Lit::Str(ls) => ls.value(),
                        other => {
                            return Err(syn::Error::new_spanned(
                                other,
                                "liaise(prefix = \"ABUT\") expected",
                            ));
                        }
                    };
                    return Ok(Some(s));
                }
                other => {
                    // On the enum itself we only allow prefix=...
                    return Err(syn::Error::new_spanned(
                        other,
                        "on the enum, #[liaise(...)] only supports prefix = \"...\"",
                    ));
                }
            }
        }
    }

    Ok(None)
}

