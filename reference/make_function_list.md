# Create structural functions for a causal model

Converts a named causal model into a named list of R functions. Each
variable in the causal model is converted using
[`create_structural_function()`](https://tadegquillien.github.io/causaljudgment/reference/create_structural_function.md).

## Usage

``` r
make_function_list(vars)
```

## Arguments

- vars:

  A named list specifying the causal model. Elements representing
  structural equations should be strings, while exogenous variables are
  specified by their probabilities.

## Value

A named list of functions corresponding to the variables in the causal
model.
