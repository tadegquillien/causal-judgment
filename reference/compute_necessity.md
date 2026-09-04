# compute_necessity(): compute whether X was necessary for Y in the actual world.

We check whether intervening on X in the actual world flips the value of
Y.

## Usage

``` r
compute_necessity(x_var, y_var, causal_model, actual_world)
```

## Arguments

- x_var:

  Character string giving the candidate cause variable.

- y_var:

  Character string giving the outcome variable.

- causal_model:

  A named list of structural functions defining the causal model.

- actual_world:

  A named list giving the values of variables in the actual world.

## Value

A logical value indicating whether the candidate cause is necessary for
the outcome in the actual world.
