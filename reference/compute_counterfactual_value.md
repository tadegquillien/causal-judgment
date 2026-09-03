# This helper function computes the value of Y conditioned on a counterfactual intervention on X, starting from a given world. We will use that function for computing both necessity and sufficiency. This function works recursively. Suppose Y depends directly on X: then the function can directly compute the effect of the intervention. But Y can also indirectly depend on X, for example via a chain X -\> Z -\> Y. To accommodate this kind of cases, we recursively call the compute_counterfactual_value() function on intermediate values, until we reach X.

This helper function computes the value of Y conditioned on a
counterfactual intervention on X, starting from a given world. We will
use that function for computing both necessity and sufficiency. This
function works recursively. Suppose Y depends directly on X: then the
function can directly compute the effect of the intervention. But Y can
also indirectly depend on X, for example via a chain X -\> Z -\> Y. To
accommodate this kind of cases, we recursively call the
compute_counterfactual_value() function on intermediate values, until we
reach X.

## Usage

``` r
compute_counterfactual_value(
  intervention_var,
  intervention_value,
  target_var,
  causal_model,
  aw_values
)
```

## Arguments

- intervention_var:

  Character string: the variable we intervene upon

- intervention_value:

  Numeric: the post-intervention value of the variable we intervene upon

- target_var:

  Character string: the target variable. We want to compute the value it
  has after we've intervened on the intervention variable

- causal_model:

  A named list specifying the causal model

- aw_values:

  A named list with the values of the variables in the actual world

## Value

A numeric specifying the counterfactual value of the target variable.
