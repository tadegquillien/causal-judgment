# Parse a structural function (or exogenous probability)

Parses a string representing a structural equation or probability and
converts it into an R function. Variable names appearing in the equation
become arguments to the resulting function. Numeric inputs are
interpreted as exogenous probabilities.

## Usage

``` r
create_structural_function(equation_string)
```

## Arguments

- equation_string:

  A string representing a structural equation, or a numeric value
  representing an exogenous probability.

## Value

An R function implementing the structural equation or, for an exogenous
variable, a function returning its probability.

## Details

For example, `"a & b"` is converted into a function equivalent to
`function(a, b) a & b`.
