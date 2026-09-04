# ces(): compute a judgment with the CES model.

This function computes a CES judgment on the basis of a probability
distribution over counterfactual worlds.

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

  The dataframe containing the probability distribution over
  counterfactual worlds.

- p_col:

  Indicates which column contains the joint probability.

## Value

A numeric causal judgment.
