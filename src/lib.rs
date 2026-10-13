use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, punctuated::Punctuated, Data, DeriveInput, Expr, Fields, Lit, Meta, MetaNameValue, Path, Token};

mod error;
use error::{errors, E};

fn has_attribute(attr: &syn::Attribute, name: &str) -> bool { attr.path().is_ident(name) }
fn is_ident(path: &Path, name: &str) -> bool { path.is_ident(name) }

/// Peel invisible `Expr::Group` wrappers. `macro_rules!` metavariables arrive
/// wrapped in a `Delimiter::None` group, which `syn` parses as `Expr::Group`;
/// without unwrapping, a literal fed through a macro fails the `Expr::Lit` match
/// and the derive can't be used from any macro_rules caller.
fn peel_groups(mut expr: &Expr) -> &Expr {
    while let Expr::Group(group) = expr {
        expr = &group.expr;
    }
    expr
}

fn expect_str(expr: &Expr) -> syn::Result<String> {
    match peel_groups(expr) {
        Expr::Lit(el) => match &el.lit {
            Lit::Str(s) => Ok(s.value()),
            other => Err(errors(other, E::BadString)),
        },
        other => Err(errors(other, E::BadString)),
    }
}

fn expect_bool(expr: &Expr) -> syn::Result<bool> {
    match peel_groups(expr) {
        Expr::Lit(el) => match &el.lit {
            Lit::Bool(b) => Ok(b.value),
            other => Err(errors(other, E::BadBool)),
        },
        other => Err(errors(other, E::BadBool)),
    }
}

fn expect_u16(expr: &Expr) -> syn::Result<u16> {
    match peel_groups(expr) {
        Expr::Lit(el) => match &el.lit {
            Lit::Int(i) => i.base10_parse().map_err(|_| errors(i, E::BadCode)),
            other => Err(errors(other, E::BadCode)),
        },
        other => Err(errors(other, E::BadCode)),
    }
}

#[derive(Clone)]
enum ValueSpec {
    Literal(String),
    Field(syn::Ident),
}

#[derive(Clone)]
struct TokenSpec {
    kind: String,
    key: String,
    value: ValueSpec,
}

#[derive(Default, Clone)]
struct EnumAttr {
    prefix: Option<String>,
    domain: Option<String>,
    version: Option<String>,
}

#[derive(Default, Clone)]
struct VariantAttr {
    code: Option<u16>,
    msg: Option<String>,
    slug: Option<String>,
    severity: Option<String>,
    category: Option<String>,
    stability: Option<String>,
    help: Option<String>,
    urgent: Option<bool>,
    source: bool,
    tokens: Vec<TokenSpec>,
    evidence: Vec<syn::Ident>,
    digest: Vec<syn::Ident>,
}

fn parse_enum_attr(attrs: &[syn::Attribute]) -> syn::Result<EnumAttr> {
    let mut out = EnumAttr::default();
    for attr in attrs {
        if has_attribute(attr, "error_prefix") {
            if let syn::Meta::List(list) = &attr.meta {
                let lit: syn::LitStr = syn::parse2(list.tokens.clone())?;
                out.prefix = Some(lit.value());
            }
            continue;
        }
        if !has_attribute(attr, "liaise") { continue; }
        let items: Punctuated<Meta, Token![,]> = attr.parse_args_with(Punctuated::parse_terminated)?;
        for meta in items {
            match meta {
                Meta::NameValue(MetaNameValue { path, value, .. }) if is_ident(&path, "prefix") => out.prefix = Some(expect_str(&value)?),
                Meta::NameValue(MetaNameValue { path, value, .. }) if is_ident(&path, "domain") => out.domain = Some(expect_str(&value)?),
                Meta::NameValue(MetaNameValue { path, value, .. }) if is_ident(&path, "version") => out.version = Some(expect_str(&value)?),
                other => return Err(errors(other, E::Unsupported)),
            }
        }
    }
    Ok(out)
}

fn parse_value_spec(expr: &Expr) -> syn::Result<ValueSpec> {
    match expr {
        Expr::Lit(el) => match &el.lit {
            Lit::Str(s) => Ok(ValueSpec::Literal(s.value())),
            Lit::Int(i) => Ok(ValueSpec::Literal(i.base10_digits().to_string())),
            other => Err(errors(other, E::BadString)),
        },
        Expr::Path(path) if path.path.segments.len() == 1 => Ok(ValueSpec::Field(path.path.segments.first().unwrap().ident.clone())),
        other => Err(errors(other, E::Unsupported)),
    }
}

fn parse_token_list(list: &syn::MetaList) -> syn::Result<Vec<TokenSpec>> {
    let items: Punctuated<Meta, Token![,]> = list.parse_args_with(Punctuated::parse_terminated)?;
    let mut out = Vec::new();
    for item in items {
        match item {
            Meta::NameValue(MetaNameValue { path, value, .. }) => {
                let key = path.segments.last().unwrap().ident.to_string();
                out.push(TokenSpec { kind: key.clone(), key, value: parse_value_spec(&value)? });
            }
            other => return Err(errors(other, E::Unsupported)),
        }
    }
    Ok(out)
}

fn parse_ident_list(list: &syn::MetaList) -> syn::Result<Vec<syn::Ident>> {
    let paths: Punctuated<Path, Token![,]> = list.parse_args_with(Punctuated::parse_terminated)?;
    Ok(paths.into_iter().map(|path| path.segments.last().unwrap().ident.clone()).collect())
}

fn parse_liaise_variant_attr(attrs: &[syn::Attribute]) -> syn::Result<VariantAttr> {
    let mut out = VariantAttr::default();
    for attr in attrs {
        if !has_attribute(attr, "liaise") { continue; }
        let items: Punctuated<Meta, Token![,]> = attr.parse_args_with(Punctuated::parse_terminated)?;
        for meta in items {
            match meta {
                Meta::NameValue(MetaNameValue { path, value, .. }) if is_ident(&path, "code") => out.code = Some(expect_u16(&value)?),
                Meta::NameValue(MetaNameValue { path, value, .. }) if is_ident(&path, "msg") || is_ident(&path, "message") || is_ident(&path, "error") => out.msg = Some(expect_str(&value)?),
                Meta::NameValue(MetaNameValue { path, value, .. }) if is_ident(&path, "slug") => out.slug = Some(expect_str(&value)?),
                Meta::NameValue(MetaNameValue { path, value, .. }) if is_ident(&path, "severity") => out.severity = Some(expect_str(&value)?),
                Meta::NameValue(MetaNameValue { path, value, .. }) if is_ident(&path, "category") => out.category = Some(expect_str(&value)?),
                Meta::NameValue(MetaNameValue { path, value, .. }) if is_ident(&path, "stability") => out.stability = Some(expect_str(&value)?),
                Meta::NameValue(MetaNameValue { path, value, .. }) if is_ident(&path, "help") => out.help = Some(expect_str(&value)?),
                Meta::NameValue(MetaNameValue { path, value, .. }) if is_ident(&path, "urgent") => out.urgent = Some(expect_bool(&value)?),
                Meta::Path(path) if is_ident(&path, "source") => out.source = true,
                Meta::List(list) if list.path.is_ident("token") => out.tokens.extend(parse_token_list(&list)?),
                Meta::List(list) if list.path.is_ident("evidence") => out.evidence.extend(parse_ident_list(&list)?),
                Meta::List(list) if list.path.is_ident("digest") => out.digest.extend(parse_ident_list(&list)?),
                other => return Err(errors(other, E::Unsupported)),
            }
        }
    }
    Ok(out)
}

fn kebab(input: &str) -> String {
    let mut out = String::new();
    for (index, ch) in input.chars().enumerate() {
        if ch.is_uppercase() {
            if index != 0 { out.push('-'); }
            for lower in ch.to_lowercase() { out.push(lower); }
        } else {
            out.push(ch);
        }
    }
    out
}

#[proc_macro_derive(RegisterErrors, attributes(error_prefix))]
pub fn derive_register_errors(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;
    let prefix = parse_enum_attr(&input.attrs).ok().and_then(|a| a.prefix).unwrap_or_else(|| "ERR".to_string());
    let variants = match &input.data { Data::Enum(data) => &data.variants, _ => return errors(name, E::OnlyEnums).to_compile_error().into() };
    let variant_idents: Vec<_> = variants.iter().map(|v| &v.ident).collect();
    let expanded = quote! {
        impl ::liaise::ErrorRegistry for #name {
            const PREFIX: &'static str = #prefix;
            const ALL_CODES: &'static [u16] = &[ #( #name::#variant_idents as u16 ),* ];
        }
        const _: () = <#name as ::liaise::ErrorRegistry>::VALIDATE;
    };
    expanded.into()
}

#[proc_macro_derive(LiaiseCodes, attributes(error_prefix, liaise))]
pub fn derive_liaise_codes(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;
    let enum_attr = match parse_enum_attr(&input.attrs) { Ok(v) => v, Err(e) => return e.to_compile_error().into() };
    let prefix = enum_attr.prefix.unwrap_or_else(|| "ERR".to_string());
    let domain = enum_attr.domain.unwrap_or_else(|| "diagnostic".to_string());
    let _version = enum_attr.version;
    let variants = match &input.data { Data::Enum(d) => &d.variants, _ => return errors(name, E::OnlyEnums).to_compile_error().into() };

    struct Info { ident: syn::Ident, fields: Fields, code: u16, msg: String, slug: String, severity: String, category: Option<String>, stability: String, help: Option<String>, urgent: Option<bool>, source_ty: Option<syn::Type>, tokens: Vec<TokenSpec>, evidence: Vec<syn::Ident> }
    let mut infos = Vec::<Info>::new();
    for v in variants {
        let meta = match parse_liaise_variant_attr(&v.attrs) { Ok(m) => m, Err(e) => return e.to_compile_error().into() };
        let code = match meta.code { Some(c) => c, None => return errors(v, E::MissingCode).to_compile_error().into() };
        let msg = match meta.msg { Some(m) => m, None => return errors(v, E::MissingMessage).to_compile_error().into() };
        // `source`: the variant's single tuple field is the underlying error. It becomes
        // `Error::source()` and a `From<field type>` for the enum, so `?` lifts it.
        let source_ty = if meta.source {
            match &v.fields {
                Fields::Unnamed(u) if u.unnamed.len() == 1 => Some(u.unnamed.first().unwrap().ty.clone()),
                _ => return errors(v, E::Source).to_compile_error().into(),
            }
        } else { None };
        infos.push(Info {
            ident: v.ident.clone(),
            fields: v.fields.clone(),
            code,
            msg,
            slug: meta.slug.unwrap_or_else(|| kebab(&v.ident.to_string())),
            severity: meta.severity.unwrap_or_else(|| "error".to_string()),
            category: meta.category,
            stability: meta.stability.unwrap_or_else(|| "experimental".to_string()),
            help: meta.help,
            urgent: meta.urgent,
            source_ty,
            tokens: meta.tokens,
            evidence: meta.evidence,
        });
    }

    let all_codes: Vec<_> = infos.iter().map(|i| i.code).collect();

    let code_arms = infos.iter().map(|i| { let vi=&i.ident; let code=i.code; match &i.fields { Fields::Unit => quote!{ Self::#vi => #code, }, Fields::Unnamed(_) => quote!{ Self::#vi(..) => #code, }, Fields::Named(_) => quote!{ Self::#vi { .. } => #code, }, } });
    let slug_arms = infos.iter().map(|i| { let vi=&i.ident; let slug=&i.slug; match &i.fields { Fields::Unit => quote!{ Self::#vi => #slug, }, Fields::Unnamed(_) => quote!{ Self::#vi(..) => #slug, }, Fields::Named(_) => quote!{ Self::#vi { .. } => #slug, }, } });
    let msg_arms = infos.iter().map(|i| { let vi=&i.ident; let msg=&i.msg; match &i.fields { Fields::Unit => quote!{ Self::#vi => ::std::format!(#msg), }, Fields::Unnamed(_) => quote!{ Self::#vi(..) => ::std::format!(#msg), }, Fields::Named(named) => { let binds = named.named.iter().map(|f| f.ident.as_ref().unwrap()); quote!{ Self::#vi { #(#binds),* } => ::std::format!(#msg), } } } });
    let severity_arms = infos.iter().map(|i| { let vi=&i.ident; let severity=&i.severity; match &i.fields { Fields::Unit => quote!{ Self::#vi => ::liaise::LiaiseSeverity::from_label(#severity), }, Fields::Unnamed(_) => quote!{ Self::#vi(..) => ::liaise::LiaiseSeverity::from_label(#severity), }, Fields::Named(_) => quote!{ Self::#vi { .. } => ::liaise::LiaiseSeverity::from_label(#severity), }, } });
    let stability_arms = infos.iter().map(|i| { let vi=&i.ident; let stability=&i.stability; match &i.fields { Fields::Unit => quote!{ Self::#vi => ::liaise::LiaiseStability::from_label(#stability), }, Fields::Unnamed(_) => quote!{ Self::#vi(..) => ::liaise::LiaiseStability::from_label(#stability), }, Fields::Named(_) => quote!{ Self::#vi { .. } => ::liaise::LiaiseStability::from_label(#stability), }, } });
    let category_arms = infos.iter().map(|i| { let vi=&i.ident; let cat=&i.category; match (cat, &i.fields) { (Some(cat), Fields::Unit) => quote!{ Self::#vi => Some(#cat), }, (Some(cat), Fields::Unnamed(_)) => quote!{ Self::#vi(..) => Some(#cat), }, (Some(cat), Fields::Named(_)) => quote!{ Self::#vi { .. } => Some(#cat), }, (None, Fields::Unit) => quote!{ Self::#vi => None, }, (None, Fields::Unnamed(_)) => quote!{ Self::#vi(..) => None, }, (None, Fields::Named(_)) => quote!{ Self::#vi { .. } => None, }, } });
    let help_arms = infos.iter().map(|i| { let vi=&i.ident; let help=&i.help; match (help, &i.fields) { (Some(help), Fields::Unit) => quote!{ Self::#vi => Some(#help), }, (Some(help), Fields::Unnamed(_)) => quote!{ Self::#vi(..) => Some(#help), }, (Some(help), Fields::Named(_)) => quote!{ Self::#vi { .. } => Some(#help), }, (None, Fields::Unit) => quote!{ Self::#vi => None, }, (None, Fields::Unnamed(_)) => quote!{ Self::#vi(..) => None, }, (None, Fields::Named(_)) => quote!{ Self::#vi { .. } => None, }, } });

    // A code's urgent mark (`urgent = true|false`), declared once with the code: only an enum that marks a code says so
    // (`urgent_mark`); every other keeps the trait's default — unmarked, urgent by its severity (`Liaise::is_urgent`).
    let urgent_marks = infos.iter().any(|i| i.urgent.is_some()).then(|| {
        let arms = infos.iter().map(|i| {
            let vi = &i.ident;
            let mark = match i.urgent { Some(urgent) => quote!{ Some(#urgent) }, None => quote!{ None } };
            match &i.fields { Fields::Unit => quote!{ Self::#vi => #mark, }, Fields::Unnamed(_) => quote!{ Self::#vi(..) => #mark, }, Fields::Named(_) => quote!{ Self::#vi { .. } => #mark, } }
        });
        quote!{ fn urgent_mark(&self) -> Option<bool> { match self { #(#arms)* } } }
    });

    let token_arms = infos.iter().map(|i| {
        let vi=&i.ident;
        let token_exprs = i.tokens.iter().map(|tok| {
            let kind=&tok.kind; let key=&tok.key;
            match &tok.value {
                ValueSpec::Literal(value) => quote!{ ::liaise::LiaiseToken::new(#kind, #key, #value) },
                ValueSpec::Field(field) => quote!{ ::liaise::LiaiseToken::new(#kind, #key, ::std::string::ToString::to_string(#field)) },
            }
        });
        match &i.fields {
            Fields::Unit => quote!{ Self::#vi => ::std::vec![ #(#token_exprs),* ], },
            Fields::Unnamed(_) => quote!{ Self::#vi(..) => ::std::vec![ #(#token_exprs),* ], },
            Fields::Named(named) => { let binds = named.named.iter().map(|f| f.ident.as_ref().unwrap()); quote!{ Self::#vi { #(#binds),* } => ::std::vec![ #(#token_exprs),* ], } }
        }
    });
    let evidence_arms = infos.iter().map(|i| {
        let vi=&i.ident;
        let evidence_exprs = i.evidence.iter().map(|field| { let key=field.to_string(); quote!{ ::liaise::LiaiseEvidence::new(#key, ::std::string::ToString::to_string(#field)) } });
        match &i.fields {
            Fields::Unit => quote!{ Self::#vi => ::std::vec![ #(#evidence_exprs),* ], },
            Fields::Unnamed(_) => quote!{ Self::#vi(..) => ::std::vec![ #(#evidence_exprs),* ], },
            Fields::Named(named) => { let binds = named.named.iter().map(|f| f.ident.as_ref().unwrap()); quote!{ Self::#vi { #(#binds),* } => ::std::vec![ #(#evidence_exprs),* ], } }
        }
    });

    let source_arms = infos.iter().filter(|i| i.source_ty.is_some()).map(|i| { let vi=&i.ident; quote!{ Self::#vi(src) => Some(src), } });
    let from_impls = infos.iter().filter_map(|i| { let vi=&i.ident; let ty=i.source_ty.as_ref()?; Some(quote!{
        impl ::core::convert::From<#ty> for #name { fn from(src: #ty) -> Self { Self::#vi(src) } }
    }) });

    let expanded = quote! {
        impl ::liaise::ErrorRegistry for #name {
            const PREFIX: &'static str = #prefix;
            const ALL_CODES: &'static [u16] = &[ #(#all_codes),* ];
        }
        const _: () = <#name as ::liaise::ErrorRegistry>::VALIDATE;

        impl ::liaise::Liaise for #name {
            fn code_id(&self) -> u16 { match self { #(#code_arms)* } }
            fn domain(&self) -> &'static str { #domain }
            fn slug(&self) -> &'static str { match self { #(#slug_arms)* } }
            fn category(&self) -> Option<&'static str> { match self { #(#category_arms)* } }
            fn severity(&self) -> ::liaise::LiaiseSeverity { match self { #(#severity_arms)* } }
            fn stability(&self) -> ::liaise::LiaiseStability { match self { #(#stability_arms)* } }
            fn message(&self) -> ::std::string::String { match self { #(#msg_arms)* } }
            fn help(&self) -> Option<&'static str> { match self { #(#help_arms)* } }
            #urgent_marks
            fn tokens(&self) -> ::std::vec::Vec<::liaise::LiaiseToken> { match self { #(#token_arms)* } }
            fn evidence(&self) -> ::std::vec::Vec<::liaise::LiaiseEvidence> { match self { #(#evidence_arms)* } }
        }

        impl ::core::fmt::Display for #name {
            fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                write!(f, "{}", ::liaise::Liaise::render(self))
            }
        }

        impl ::std::error::Error for #name {
            fn source(&self) -> Option<&(dyn ::std::error::Error + 'static)> {
                match self { #(#source_arms)* _ => None }
            }
        }

        #(#from_impls)*
    };
    expanded.into()
}
