# Package index

## All functions

- [`ces()`](https://tadegquillien.github.io/causaljudgment/reference/ces.md)
  : Computes a judgment with the CES model
- [`compute_counterfactual_value()`](https://tadegquillien.github.io/causaljudgment/reference/compute_counterfactual_value.md)
  : This helper function computes the value of Y conditioned on a
  counterfactual intervention on X, starting from a given world. We will
  use that function for computing both necessity and sufficiency. This
  function works recursively. Suppose Y depends directly on X: then the
  function can directly compute the effect of the intervention. But Y
  can also indirectly depend on X, for example via a chain X -\> Z
  -\> Y. To accommodate this kind of cases, we recursively call the
  compute_counterfactual_value() function on intermediate values, until
  we reach X.
- [`compute_judgment()`](https://tadegquillien.github.io/causaljudgment/reference/compute_judgment.md)
  : The general causal judgment function. It is essentially a wrapper
  over the ces() and ns() functions.
- [`compute_necessity()`](https://tadegquillien.github.io/causaljudgment/reference/compute_necessity.md)
  : Function to compute necessity: does intervening on X in the actual
  world flip the value of Y?
- [`compute_probabilities()`](https://tadegquillien.github.io/causaljudgment/reference/compute_probabilities.md)
  : Compute the counterfactual joint probability distribution induced by
  a causal model and the state of the actual world.
- [`compute_sufficiency()`](https://tadegquillien.github.io/causaljudgment/reference/compute_sufficiency.md)
  : Computes the sufficiency of a candidate cause for an outcome
- [`create_structural_function()`](https://tadegquillien.github.io/causaljudgment/reference/create_structural_function.md)
  : Parse a structural function (or exogenous probability)
- [`make_function_list()`](https://tadegquillien.github.io/causaljudgment/reference/make_function_list.md)
  : Create structural functions for a causal model
- [`ns()`](https://tadegquillien.github.io/causaljudgment/reference/ns.md)
  : Function computing a judgment for the NS model
- [`verif()`](https://tadegquillien.github.io/causaljudgment/reference/verif.md)
  : Verify consistency of an endogenous variable
