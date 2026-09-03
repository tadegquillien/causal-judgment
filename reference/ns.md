# Function computing a judgment for the NS model

Function computing a judgment for the NS model

## Usage

``` r
ns(var, outcome, actual_world, d, causal_model = cm)
```

## Arguments

- var:

  Character string giving the candidate cause variable.

- outcome:

  Character string giving the outcome variable.

- actual_world:

  A named list specifying the values of variables in the actual world.

- d:

  The dataframe containing the counterfactual-based joint distribution.

- causal_model:

  A named list specifying the causal model.

## Value

A numeric causal judgment.
