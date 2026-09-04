# compute_probability(): compute the probability distribution over counterfactual worlds induced by the causal model and the state of the actual world.

We compute the distribution using the factorization of the causal model,
by computing the marginal probability of exogenous variables and the
conditional probabilities of the endogenous variables. Then we take the
product of these probabilities to compute the joint distribution.

## Usage

``` r
compute_probabilities(structural_functions, actual_world, s = 0)
```

## Arguments

- structural_functions:

  A named list of functions defining the structural equations and
  probability distributions of the variables in the causal model.

- actual_world:

  A named list giving the values of variables in the actual world.

- s:

  Numeric or Named List parameter(s) controlling the adjustment of
  exogenous variable probabilities toward their actual-world values.
  Defaults to `0`. Usually this is a scalar that applies to all
  variables in the model, but one can also use a named list that
  specifies a separate parameter for each variable.

## Value

A data frame containing one row for each possible world, probability
columns for each variable, and a column `p` giving the probability of
each world.
