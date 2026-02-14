use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Data};

#[proc_macro_derive(RegisterErrors, attributes(error_prefix))]
pub fn derive_register_errors(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    // 1. We STILL extract this because it's part of the registration
    let prefix = input.attrs.iter()
        .find(|a| a.path().is_ident("error_prefix"))
        .and_then(|a| a.parse_args::<syn::LitStr>().ok())
        .map(|s| s.value())
        .unwrap_or_else(|| "ERR".to_string());

    // 2. Introspection: Collect variant identifiers
    let variants = if let Data::Enum(data) = &input.data {
        &data.variants
    } else {
        panic!("RegisterErrors can only be derived for Enums");
    };

    let variant_idents: Vec<_> = variants.iter().map(|v| &v.ident).collect();

    let expanded = quote! {
        // Implement the Registry
        impl liaise::ErrorRegistry for #name {
            // We store the prefix here so Liaise can grab it later 
            // without the user re-typing it.
            const PREFIX: &'static str = #prefix;

            const ALL_CODES: &'static [u16] = &[
                #( #name::#variant_idents as u16 ),*
            ];
        }

        // The Sentinel check
        const _: () = <#name as liaise::ErrorRegistry>::VALIDATE;
    };

    TokenStream::from(expanded)
}