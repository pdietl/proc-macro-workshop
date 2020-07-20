use proc_macro::TokenStream;
use proc_macro2;
use quote::{format_ident, quote};
use syn::{self, Data, PathArguments, DeriveInput, Ident, Type, GenericArgument};


fn optional_argument_type_get_inner_type(field_type: &Type) -> Option<Type> {
    if let Type::Path(type_path) = field_type {
        if let None = type_path.qself {
            if type_path.path.segments.len() == 1 {
                let segment = type_path.path.segments.first().unwrap();
                if segment.ident.to_string() == "Option" {
                    if let PathArguments::AngleBracketed(angled_arg) = &segment.arguments {
                        if angled_arg.args.len() == 1 {
                            let generic_arg = angled_arg.args.first().unwrap();
                            if let GenericArgument::Type(ty) = generic_arg {
                                return Some(ty.clone());
                            }
                        }
                    }
                }
            }
        }
    }
    None
}

fn argument_type_is_option(field_type: &Type) -> bool {
    if let Some(_) = optional_argument_type_get_inner_type(field_type) {
        true
    } else {
        false
    }
}

fn make_build_function(
    struct_name: &Ident,
    builder_field_names: &Vec<Ident>,
    builder_field_types: &Vec<Type>,
) -> proc_macro2::TokenStream {

    let mut field_match_none_expr = vec!();

    for (field_type, field_name) in builder_field_types.iter().zip(builder_field_names) {
        if argument_type_is_option(&field_type) {
            field_match_none_expr.push(
                quote! {
                    None
                }
            )
        } else {
            field_match_none_expr.push(
                quote! {
                    let s = ::std::string::String::from(
                                ::std::concat!(
                                    "Error, field '",
                                    ::std::stringify!(#field_name),
                                    "' was never set!"
                                )
                            );
                    return ::std::result::Result::Err(
                        <::std::string::String as ::std::convert::Into<_>>::into(s)
                    )
                }
            )
        }
    }

    quote! {
        pub fn build(&mut self) -> std::result::Result<#struct_name, ::std::boxed::Box<dyn ::std::error::Error>> {
            #(
                let #builder_field_names =
                    match self.#builder_field_names {
                        Some(ref field) => (*field).clone(),
                        None => {
                            #field_match_none_expr
                        }
                    };
            )*

            Ok(#struct_name {
                #(#builder_field_names),*
            })
        }
    }
}

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse_macro_input!(input as DeriveInput);

    let name = ast.ident;
    let builder_name = format_ident!("{}Builder", name);
    let struct_fields = if let Data::Struct(struct_data) = ast.data {
        struct_data.fields
    } else {
        panic!("Expected a struct!")
    };

    let mut builder_field_names = vec![];
    let mut builder_field_types = vec![];

    for field in struct_fields {
        builder_field_names.push(field.ident.unwrap());
        builder_field_types.push(field.ty);
    }

    let build_function = make_build_function(&name, &builder_field_names, &builder_field_types);

    let mut builder_function_argument_types = vec!();
    let mut builder_function_assignment = vec!();

    for (field_type, field_name) in builder_field_types.iter().zip(&builder_field_names) {
        if let Some(inner_type) = optional_argument_type_get_inner_type(field_type) {
            builder_function_argument_types.push(inner_type);
            builder_function_assignment.push(
                quote! {
                   ::std::option::Option::Some(std::option::Option::Some(#field_name)); 
                }
            );
        } else {
            builder_function_argument_types.push(field_type.clone());
            builder_function_assignment.push(
                quote! {
                    ::std::option::Option::Some(#field_name);
                }
            );
        }
    }

    let ret = quote! {
        pub struct #builder_name {
            #(#builder_field_names: ::std::option::Option<#builder_field_types>),*
        }
        impl #builder_name {
            #(
                fn #builder_field_names(&mut self, #builder_field_names: #builder_function_argument_types) -> &mut Self {
                    self.#builder_field_names = #builder_function_assignment;
                    self
                }
            )*

            #build_function
        }

        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#builder_field_names: None),*
                }
            }
        }
    };
    ret.into()
}
