extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::{bracketed, Result};
use syn::{parse_macro_input, Expr, ExprArray};

/// Parses the tokens for an array of an array of expressions into a nested vec, which can then
/// itself be transposed
struct MatrixInput {
    pub incoming_exprs: Vec<Vec<Expr>>,
}

impl Parse for MatrixInput {
    fn parse(input: ParseStream) -> Result<Self> {
        // pull out the bracket-content, i.e. the array of the array of the expressions
        let content;
        let _ = bracketed!(content in input);

        // We are expecting a standard array-array syntax: comma-separated brackets, each
        // containyng a series of expressions
        let rows: Punctuated<ExprArray, Comma> =
            content.parse_terminated(ExprArray::parse, Comma)?;

        // Error-at-compile-time if we try transposing something empty, or a 1x1 matrix
        let Some(fst) = rows.first() else {
            return Err(syn::Error::new_spanned(&rows, "Expected a non-empty matrix"));
        };
        if fst.elems.is_empty() {
            return Err(syn::Error::new_spanned(fst, "Expected a non-empty matrix"));
        }
        if rows.len() == 1 && fst.elems.len() == 1 {
            return Err(syn::Error::new_spanned(
                fst,
                "1x1 matrix transpose is a no-op",
            ));
        }

        // We parsed the matrix, and sanity checked its dimensions. Now to return the expresions in
        // vec-of-vec form to setup the data
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
    // post-condition: the dimensions have been sanity-checked to be suitable for transposition
    let MatrixInput { incoming_exprs } = parse_macro_input!(input as MatrixInput);

    // Do the actual transposition of expressions. We assume the (square) matrix is symmetrical
    // untill contradicted. If never contradicted, we can error out: This macro is just over-head
    // for no benefit.
    let mut still_sym = incoming_exprs.len() == incoming_exprs[0].len();
    let mut transposed = vec![vec![]; incoming_exprs[0].len()];
    for (i, incoming_row) in incoming_exprs.iter().enumerate() {
        for (j, incoming_elem) in incoming_row.iter().enumerate() {
            if still_sym && incoming_exprs[i][j].ne(&incoming_exprs[j][i]) {
                still_sym = false;
            }

            transposed[j].push(incoming_elem);
        }
    }

    if still_sym {
        // Transposition being a no-op isn't depenedent on dimensions, but also symetry of
        // values...
        return syn::Error::new(
            proc_macro2::Span::call_site(),
            "Uneccisary transpose: Matrix is symmetrical",
        )
        .to_compile_error()
        .into();
    }

    let transposed_tokens = transposed.into_iter().map(|row| {
        let elems = row.into_iter();
        quote! { [ #(#elems),* ] }
    });

    TokenStream::from(quote! {
         [ #(#transposed_tokens),* ]
    })
}
