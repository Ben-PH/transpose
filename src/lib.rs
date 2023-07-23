extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::{bracketed, Result, Token};
use syn::{parse_macro_input, Expr, ExprArray};

/// The Togens showing the pre-transposed matrix
struct MatrixInput {
    pub incoming_exprs: Vec<Vec<Expr>>, // The values in the matrix
}

struct Row {
    _entries: Punctuated<Expr, Token![,]>,
}
impl Parse for Row {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        bracketed!(content in input);
        let _entries: Punctuated<Expr, Token![,]> =
            content.parse_terminated(Expr::parse, Token![,])?;
        Ok(Self { _entries })
    }
}

impl Parse for MatrixInput {
    // varname: [TargetType ; WIDTH] ; HEIGHt] = <pre-transposed>
    fn parse(input: ParseStream) -> Result<Self> {

        // Parse the array values
        let content;
        let _ = bracketed!(content in input); // Get the content within the brackets

        // Parse the row-values, they should be comma-separated arrays
        let rows: Punctuated<ExprArray, Comma> =
            content.parse_terminated(ExprArray::parse, Comma)?;
        let Some(_) = rows.first() else {
            return Err(syn::Error::new_spanned(&rows, "Expected non-empty"));
        };

        let mut matrix = vec![];
        for expr_array in rows {
            let mut row = vec![];
            for expr in expr_array.elems {
                row.push(expr);
            }
            matrix.push(row);
        }

        // Return the parsed input
        Ok(MatrixInput {
            incoming_exprs: matrix,
        })
    }
}

#[proc_macro]
pub fn transpose(input: TokenStream) -> TokenStream {
    let MatrixInput {
        incoming_exprs,
    } = parse_macro_input!(input as MatrixInput);


    let mut transposed = vec![vec![] ; incoming_exprs[0].len()];
    for incoming_row in incoming_exprs.iter() {
        for (j, incoming_elem) in incoming_row.iter().enumerate() {
            transposed[j].push(incoming_elem);
        }
    }

    dbg!(&transposed);
    let transposed_tokens = transposed.into_iter().map(|row| {
        let elems = row
            .into_iter();
        quote! { [ #(#elems),* ] }
    });


    TokenStream::from(quote! {
         [ #(#transposed_tokens),* ];
    })
}
