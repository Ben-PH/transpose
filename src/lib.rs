extern crate proc_macro;
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::{bracketed, Result, Token};
use syn::{parse_macro_input, Expr, ExprArray, ExprLit, Ident, Lit, LitInt, Type};

/// The Togens showing the pre-transposed matrix
struct MatrixInput {
    pub name: Ident,           // Variable identifier
    pub height: usize,         // The number of rows
    pub width: usize,          // The number of columns
    pub values: Vec<Vec<u16>>, // The values in the matrix
}

struct Row {
    entries: Punctuated<Expr, Token![,]>,
}
impl Parse for Row {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        bracketed!(content in input);
        let entries: Punctuated<Expr, Token![,]> =
            content.parse_terminated(Expr::parse, Token![,])?;
        Ok(Self { entries })
    }
}

impl Parse for MatrixInput {
    // varname: [TargetType ; WIDTH] ; HEIGHt] = <pre-transposed>
    fn parse(input: ParseStream) -> Result<Self> {
        // Parse the variable identifier
        let name: Ident = input.parse()?;

        // Parse the ":" token
        input.parse::<Token![:]>()?;

        let parsed_type: Type = input.parse()?;
        let (dtype, width, height) = match &parsed_type {
            Type::Array(row_array) => {
                // You now have an ArrayType
                let row_type: &Type = &*row_array.elem; // The type of the elements
                let height: &Expr = &row_array.len; // The length of the array (as an expression)

                match row_type {
                    Type::Array(values_array) => {
                        // The inner type is also an ArrayType
                        let mat_dtype: &Type = &values_array.elem;
                        let width: &Expr = &values_array.len;
                        (mat_dtype, width, height)

                        // Now you have the type and length of the inner array
                    }
                    _ => return Err(syn::Error::new_spanned(row_array, "Expected 2d array")),
                }
            }
            _ => return Err(syn::Error::new_spanned(&parsed_type, "Expected 2d array")),
        };

        // Turn height and width into the int literals
        let height = if let Expr::Lit(ExprLit {
            lit: Lit::Int(lit_int),
            ..
        }) = height.clone()
        {
            lit_int.base10_parse::<usize>()?
        } else {
            return Err(syn::Error::new_spanned(
                height,
                "Expected an integer literal",
            ));
        };

        let width = if let Expr::Lit(ExprLit {
            lit: Lit::Int(lit_int),
            ..
        }) = width.clone()
        {
            lit_int.base10_parse::<usize>()?
        } else {
            return Err(syn::Error::new_spanned(
                width,
                "Expected an integer literal",
            ));
        };

        // Parse the "=" token
        input.parse::<Token![=]>()?;

        // Parse the array values
        let content;
        let _ = bracketed!(content in input); // Get the content within the brackets

        // Parse the row-values, they should be comma-separated arrays
        let rows: Punctuated<ExprArray, Comma> =
            content.parse_terminated(ExprArray::parse, Comma)?;
        if rows.len() != height {
            return Err(syn::Error::new_spanned(
                &rows,
                format!("Expected {} elements", height),
            ));
        }
        let Some(fst) = rows.first() else {
            return Err(syn::Error::new_spanned(&rows, "Expected non-empty"));
        };
        if fst.elems.len() != width {
            return Err(syn::Error::new_spanned(
                &fst,
                format!("Expected {} elements", width),
            ));
        }

        let mut values = vec![];
        for expr_array in rows {
            for expr in expr_array.elems {
                match &expr {
                    syn::Expr::Lit(syn::ExprLit {
                        lit: syn::Lit::Int(int),
                        ..
                    }) => {
                        // It's an integer literal - do something with it
                    }
                    _ => {
                        return Err(syn::Error::new_spanned(
                            &expr,
                            "Expected an integer literal",
                        ));
                    }
                }
            }
        }

        // Return the parsed input
        Ok(MatrixInput {
            name,
            height,
            width,
            values,
        })
    }
}

#[proc_macro]
pub fn transpose(input: TokenStream) -> TokenStream {
    let MatrixInput {
        name,
        height,
        width,
        values,
    } = parse_macro_input!(input as MatrixInput);

    let mut transposed = vec![vec![0; height]; width];
    for (i, row) in values.iter().enumerate() {
        for (j, elem) in row.iter().enumerate(){
            transposed[j][i] = values[i][j];
        }
    }

    let transposed_tokens = transposed.into_iter().map(|row| {
        let elems = row
            .into_iter()
            .map(|value| LitInt::new(&value.to_string(), Span::call_site()));
        quote! { [ #(#elems),* ] }
    });

    TokenStream::from(quote! {
        let #name: [[u16; #height]; #width] = [ #(#transposed_tokens),* ];
    })
}
