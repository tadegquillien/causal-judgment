# The general causal judgment function. It is essentially a wrapper over the ces() and ns() functions.

The general causal judgment function. It is essentially a wrapper over
the ces() and ns() functions.

## Usage

``` r
compute_judgment(var, outcome, causal_model, actual_world, model, s = 0)
```

## Arguments

- var:

  Character string giving the candidate cause variable.

- outcome:

  Character string giving the outcome variable.

- causal_model:

  A named list specifying the causal model.

- actual_world:

  A named list specifying the values of variables in the actual world.
  The names must match those in `causal_model`.

- model:

  Character string specifying the causal judgment model. Currently,
  `"ces"` and `"ns"` are supported.

- s:

  Numeric or Named List parameter(s) controlling the adjustment of
  exogenous variable probabilities toward their actual-world values.
  Defaults to `0`. Usually this is a scalar that applies to all
  variables in the model, but one can also use a named list that
  specifies a separate parameter for each variable.

## Value

A numeric causal judgment.

## Examples

``` r
# Define a causal model
causal_model <- list(e = "a & b", a = .1, b = .9)

# Define the actual world
actual_world <- list(e = 1, a = 1, b = 1)

# Compute the CES judgment for A causing E
compute_judgment(
  var = "a",
  outcome = "e",
  causal_model = causal_model,
  actual_world = actual_world,
  model = "ces",
  s = .7
)
#> [1] 0.9472197
```
