# causal-judgment

These R scripts allow one to compute the predictions of computational models of causal judgment. Currently two models are implemented: the Counterfactual Effect Size (CES) model (Quillien & Lucas, 2023) and the Necessity-Sufficiency (NS) model (Icard, Kominsky & Knobe, 2017).

In this file, we give examples of how to compute causal judgments, describe current limitations, and then (for interested readers) give a high-level explanation of how these scripts work.

### How to use

First, run everything in the `main.R`, `ces.R`, and `ns.R` files, to load the necessary functions. You can import the files by either cloning the repository (if familiar with git) or downloading the files by clicking on the Green 'Code' button and then 'Download ZIP'.

Computing a causal judgment requires three steps:

-Specify a causal model,

-Specify what happened in the actual world,

-Request a causal judgment.

For example, suppose that variable E happens if both A and B happen. We specify this causal model as:
```r
causalmodel <- list(e='a&b', a=.1, b=.9)
```

The string `'a&b'` specifies the structural equation for E. The numbers .1 and .9 are the exogenous probabilities for A and B.

Next we must specify what happens in the actual world:

```r
actual_world <- list(e=1, a=1, b=1).
```

This says that in the actual world, all variables have value 1.

Finally we request a causal judgment:
```r
compute_judgment('a', 'e', causalmodel, actual_world, 'ces', .7)
```

The first two arguments say that we want to see to what extent A caused E. The next two arguments specify the causal model and the actual-world value of the variables (defined above). The fifth argument specifies the computational model we want to use (here, CES). The last argument specifies the value of the stability parameter (how much counterfactual simulation is 'anchored' to the actual world). This argument is optional, by default s=0. 

Running this command will return a 'causal score' from -1 to 1 (for CES) or from 0 to 1 (for NS). Higher values indicate higher actual causal strength. For the CES model, a negative value like -.8 indicates a very weak causal score, not something like 'negative' causation. And -.8 is weaker than for example -.4.

References:

Icard, Kominsky & Knobe (2017). Normality and actual causal strength. <i>Cognition</i>.

Quillien & Lucas (2023). Counterfactuals and the logic of causal selection. <i>Psychological Review</i>.

### Limitations

We currently only support causal models that have binary variables. The value of these variables must be input as 0 or 1. 

Structural equations must be specified as strings that use symbols that will be recognized by R as logical symbols. 
For example, `'a or not b'` will not work, but `'a | !b'` will. In principle it should be possible to express any Boolean function this way, although for some of them it might be cumbersome.

Causal models must be expressed as Structural Causal Models (SCMs). Non-deterministic models like Causal Bayes Nets are not supported, but one can always formulate an SCM that emulates a Causal Bayes Net by introducing 'noise' variables.

On the positive side, any causal structure that can be expressed by an SCM with binary variables should be supported. This includes 'deep' chain structures like A --> B --> C --> D --> E, and confounded models where the effect of the cause C on the effect E is confounded by other variables.

### How these scripts work

We use an analytical approach to compute causal judgments for the CES and NS
models. Our goal is to compute a causal judgment for the extent to which $C=c$ caused $E=e$. Instead of explicitly simulating counterfactual worlds by sampling
from the SCM, we analytically compute the probability distribution
over counterfactual worlds that would follow from this sampling process. This makes computation much faster.
First it is useful to describe what this sampling process would look like
(this will help understand what the analytical computation is trying to
formalize).

We sample each counterfactual world by doing the following:

1) Sample each exogenous variable according to the Lucas-Kemp process
(see Lucas & Kemp, 2015; Quillien & Lucas, 2023).

2) If C is endogenous, sample the value of C by making a random intervention
on C, where C is sampled from $p(C)$. $p(C)$ is the marginal probability of C: it
is the probability we would get if we simply generated counterfactual worlds
without implementing step 2.

3) Determine the value of all other variables by applying the relevant
structural equations.

The distribution over counterfactual worlds that we wish to compute is the
relative frequency of worlds simulated this way, as the number of samples
goes to infinity.

To compute this probability distribution analytically, we do the following. We list each possible world (i.e. combination of variable values) as a row in a table. We then assign a probability to each world:

1) We compute the probability of each exogenous variable (defined by the Lucas-Kemp process).
   
2) For each endogenous variable X, we compute $p(X=x|pa(X))$ as 0 or 1, depending on whether $X=x$ is consistent with the value of the variable's parents (i.e. $pa(X)$ ) in the current world. (For example if we have $A := B$, and in the current world $A=1$ but $B=0$, then $p(B=0|pa(B))$ is 0). Note that since we are using SCMs with fully observed variables the conditional probabilities here are deterministic.
   
3) We compute a probability $p(w)$ for the whole world by using the factorization defined by the network structure, i.e. $p(w)=\prod_{X} p(X|pa(X))$.

4) If C is an endogenous variable, we must perform an additional step to ensure that the distribution reflects the fact that C's value is set by interventions. We first compute the marginal probability $p(C=c)$ in the distribution we just computed. Then we replace $p(C=c|pa(C))$ with $p(C=c)$ in every world. Doing this ensures that the probability of $C=c$ is now independent from the value of C's parents, as required by the fact that C is set by interventions. After doing this, we now re-compute the probability of each world.

After we have obtained the probability distribution over counterfactual worlds, it is easy to analytically compute the correlation between $C=c$ and $E=e$ in this distribution (for the CES model). We can also analytically compute Necessity and Sufficiency (for the NS model).

References:

Lucas & Kemp (2015). An improved probabilistic account of counterfactual reasoning. <i>Psychological Review</i>.
