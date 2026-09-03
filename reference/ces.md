# Computes a judgment with the CES model

Computes a judgment with the CES model

## Usage

``` r
ces(var1, var2, aw_values, d, p_col = "p")
```

## Arguments

- var1:

  Character string giving the candidate cause variable.

- var2:

  Character string giving the outcome variable.

- aw_values:

  A named list specifying the values of variables in the actual world.

- d:

  The dataframe containing the counterfactual-based joint distribution

- p_col:

  Indicates which column contains the joint probability.

## Value

A numeric causal judgment.
