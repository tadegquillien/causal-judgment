# Computes the sufficiency of a candidate cause for an outcome

Computes the sufficiency of a candidate cause for an outcome

## Usage

``` r
compute_sufficiency(var, outcome, actual_world, causal_model, d)
```

## Arguments

- var:

  Character string giving the candidate cause variable.

- outcome:

  Character string giving the outcome variable.

- actual_world:

  A named list giving the values of variables in the actual world.

- causal_model:

  A named list of structural functions defining the causal model.

- d:

  A data frame containing the joint probability distribution over
  possible worlds.

## Value

A numeric value representing the sufficiency of the candidate cause for
the outcome.
