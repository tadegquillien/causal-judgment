# verif(): verify consistency of an endogenous variable

Checks whether the value of an endogenous variable is consistent with
the values of its parent variables under its structural function.

## Usage

``` r
verif(outcome, args, fun)
```

## Arguments

- outcome:

  Numeric value of the endogenous variable.

- args:

  Values of the parent variables, supplied as a list or list-like object
  suitable for passing to `fun`.

- fun:

  A structural function defining the value of the endogenous variable as
  a function of its parents.

## Value

A logical value indicating whether the structural function produces the
specified value of `outcome`.
