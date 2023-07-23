extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::{bracketed, Result};
use syn::{parse_macro_input, Expr, ExprArray};

/// Parses the tokens for an array of an array of expressions into manupilable vec
struct MatrixInput {
    pub incoming_exprs: Vec<Vec<Expr>>, // The values in the matrix
}


impl Parse for MatrixInput {
    fn parse(input: ParseStream) -> Result<Self> {

        // pull out the bracket-content, i.e. the array expressions
        let content;
        let _ = bracketed!(content in input);

        // We are expecting a standard array-array syntax: comma-separated brackets, each
        // containyng a series of expressions
        let rows: Punctuated<ExprArray, Comma> =
            content.parse_terminated(ExprArray::parse, Comma)?;

        // Error-at-compile-time if we try transposing something empty
        let Some(fst) = rows.first() else {
            return Err(syn::Error::new_spanned(&rows, "Expected a non-empty matrix"));
        };
        if fst.elems.is_empty() {
            return Err(syn::Error::new_spanned(&fst, "Expected a non-empty matrix"));
        }


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

    // Post-parsing check
    if incoming_exprs.is_empty() || incoming_exprs[0].is_empty() {
        let error = syn::Error::new(proc_macro2::Span::call_site(), "The input matrix should not be empty");
        let compile_error = error.to_compile_error();
        return compile_error.into();
    }

    let mut still_sym = incoming_exprs.len() == incoming_exprs[0].len();
    let mut transposed = vec![vec![] ; incoming_exprs[0].len()];
    for (i, incoming_row) in incoming_exprs.iter().enumerate() {
        for (j, incoming_elem) in incoming_row.iter().enumerate() {
            if still_sym && incoming_exprs[i][j].ne(&incoming_exprs[j][i]) {
                dbg!(i, j, "not sym");
                still_sym = false;
            }

            transposed[j].push(incoming_elem);
        }
    }

    if still_sym {
        return syn::Error::new(proc_macro2::Span::call_site(), "Uneccisary transpose: Matrix is symmetrical").to_compile_error().into();
    }

    let transposed_tokens = transposed.into_iter().map(|row| {
        let elems = row
            .into_iter();
        quote! { [ #(#elems),* ] }
    });


    TokenStream::from(quote! {
         [ #(#transposed_tokens),* ]
    })
}
