# here we test the functions we developed in various cases, some taken from
# the literature, some developed for illustrative purposes


### example model call ---------------------------------------------------------

## in this example we assume a causal model with variables A,B,C,D,E,
## with E := A & (B or C) & ¬D,
## and exogenous probabilities p(A)=.1, p(B)=.2, p(C)=.4, p(D)=.1
## in the actual world, all variables except D have value 1.


## input model information
# causal model
causal_model <- list(e = 'a & (b | c) &!d', # structural equation(s)
                     a=.1, b=.2, c=.4, d=.1) # exogenous probabilities
actual_world <- list(e=1, a=1, b=1, c=1, d=0) # actual-world values

## did A cause E?
compute_judgment('a', 'e', causal_model, actual_world, 'ces', .7) # ces judgment
compute_judgment('a', 'e', causal_model, actual_world, 'ns') # ns judgment


### conjunctive and disjunctive structures (e.g. Morris et al., 2019) ----------

pa <- .1 # A is abnormal
pb <- .9 # B is normal
conjModel <- list(e='a&b', a=pa, b=pb)
disjModel <- list(e='a|b', a=pa, b=pb)
actual_world <- list(e=1, a=1, b=1)

# ces judgments
#   abnormal inflation
compute_judgment('a', 'e', conjModel, actual_world, 'ces', .7)
compute_judgment('b', 'e', conjModel, actual_world, 'ces', .7) 
#   abnormal deflation
compute_judgment('a', 'e', disjModel, actual_world, 'ces', .7) 
compute_judgment('b', 'e', disjModel, actual_world, 'ces', .7) 

# ns judgments
compute_judgment('a', 'e', conjModel, actual_world, 'ns') 
compute_judgment('b', 'e', conjModel, actual_world, 'ns') 
compute_judgment('a', 'e', disjModel, actual_world, 'ns') 
compute_judgment('b', 'e', disjModel, actual_world, 'ns') 


### Quillien & Lucas (2023) ----------------------------------------------------

## exp 2 in Quillien & Lucas (2023)

causal_model <- list(e='a+b+c>1.5', a=.05, b=.5, c=.95) # causal model

# actual-world values in study 2a and 2b
actual_world2a <- list(e=1, a=1, b=1, c=1) # study 2a
actual_world2b <- list(e=1, a=1, b=1, c=0) # study 2b

# causal judgments for study 2a, CES
compute_judgment('a', 'e', causal_model, actual_world2a, 'ces', s=.7)
compute_judgment('b', 'e', causal_model, actual_world2a, 'ces', s=.7)
compute_judgment('c', 'e', causal_model, actual_world2a, 'ces', s=.7)
# causal judgments for study 2b, CES
compute_judgment('a', 'e', causal_model, actual_world2b, 'ces', s=.7)
compute_judgment('b', 'e', causal_model, actual_world2b, 'ces', s=.7)

# causal judgments for study 2a, NS
compute_judgment('a', 'e', causal_model, actual_world2a, 'ns')
compute_judgment('b', 'e', causal_model, actual_world2a, 'ns')
compute_judgment('c', 'e', causal_model, actual_world2a, 'ns')
# causal judgments for study 2b, NS
compute_judgment('a', 'e', causal_model, actual_world2b, 'ns')
compute_judgment('b', 'e', causal_model, actual_world2b, 'ns')


## exp 3 in Quillien & Lucas (2023)

causal_model <- list(e='(a|b)&c', a=.05, b=.9, c=.95) # causal model
actual_world3a <- list(e=1, a=1, b=1, c=1)
actual_world3b <- list(e=1, a=1, b=0, c=1)

# study 3a, CES
compute_judgment('a', 'e', causal_model, actual_world3a, 'ces', .7)
compute_judgment('b', 'e', causal_model, actual_world3a, 'ces', .7)
compute_judgment('c', 'e', causal_model, actual_world3a, 'ces', .7)
# study 3b, CES
compute_judgment('a', 'e', causal_model, actual_world3b, 'ces', .7)
compute_judgment('c', 'e', causal_model, actual_world3b, 'ces', .7)

# study 3a, NS
compute_judgment('a', 'e', causal_model, actual_world3a, 'ns')
compute_judgment('b', 'e', causal_model, actual_world3a, 'ns')
compute_judgment('c', 'e', causal_model, actual_world3a, 'ns')
# study 3b, NS
compute_judgment('a', 'e', causal_model, actual_world3b, 'ns')
compute_judgment('c', 'e', causal_model, actual_world3b, 'ns')

## exp 4 in Quillien & Lucas (2023)

# sigmoid function
sigmoid <- function(x){
  return(exp(x)/(1+exp(x)))
}

stability <- .7

# C=1, p(a)=.1, p(c)=.1
causal_model <- list(e = '(a&b) | (!a&c)', a=.1, b=.5, c=.1)
actual_world <- list(e=1, a=1, b=1, c=1)

sigmoid(compute_judgment('a', 'e', causal_model, 
                         actual_world, 'ces', s=stability))

# C=1, p(a)=.1, p(c)=.9
causal_model <- list(e = '(a&b) | (!a&c)', a=.1, b=.5, c=.9)
actual_world <- list(e=1, a=1, b=1, c=1)

sigmoid(compute_judgment('a', 'e', causal_model,
                         actual_world, 'ces', s=stability))


# C=1, p(a)=.9, p(c)=.1
causal_model <- list(e = '(a&b) | (!a&c)', a=.9, b=.5, c=.1)
actual_world <- list(e=1, a=1, b=1, c=1)

sigmoid(compute_judgment('a', 'e', causal_model, 
                         actual_world, 'ces', s=stability))


# C=1, p(a)=.9, p(c)=.9
causal_model <- list(e = '(a&b) | (!a&c)', a=.9, b=.5, c=.9)
actual_world <- list(e=1, a=1, b=1, c=1)

sigmoid(compute_judgment('a', 'e', causal_model, 
                         actual_world, 'ces', s=stability))


# C=0, p(a)=.1, p(c)=.1
causal_model <- list(e = '(a&b) | (!a&c)', a=.1, b=.5, c=.1)
actual_world <- list(e=1, a=1, b=1, c=0)

sigmoid(compute_judgment('a', 'e', causal_model, 
                         actual_world, 'ces', s=stability))


# C=0, p(a)=.1, p(c)=.9
causal_model <- list(e = '(a&b) | (!a&c)', a=.1, b=.5, c=.9)
actual_world <- list(e=1, a=1, b=1, c=0)

sigmoid(compute_judgment('a', 'e', causal_model, 
                         actual_world, 'ces', s=stability))


# C=0, p(a)=.9, p(c)=.1
causal_model <- list(e = '(a&b) | (!a&c)', a=.9, b=.5, c=.1)
actual_world <- list(e=1, a=1, b=1, c=0)

sigmoid(compute_judgment('a', 'e', causal_model, 
                         actual_world, 'ces', s=stability))


# C=0, p(a)=.9, p(c)=.9
causal_model <- list(e = '(a&b) | (!a&c)', a=.9, b=.5, c=.9)
actual_world <- list(e=1, a=1, b=1, c=0)

sigmoid(compute_judgment('a', 'e', causal_model,
                         actual_world, 'ces', s=stability))



### xor condition in Gerstenberg & Icard (2020) --------------------------------

causal_model <- c(a=.8, b=.2, e='(a&!b)|(!a&b)')
actual_worldOn <- c(a=1, b=1, e=0)
actual_worldOff <- c(a=0, b=0, e=0)
compute_judgment('a', 'e', causal_model, actual_worldOn, 'ces', s=.7)
compute_judgment('b', 'e', causal_model, actual_worldOn, 'ces', s=.7)

compute_judgment('a', 'e', causal_model, actual_worldOff, 'ces', s=.7)
compute_judgment('b', 'e', causal_model, actual_worldOff, 'ces', s=.7)



### double prevention (e.g. O'Neill et al., 2025) ------------------------------

# U is the possible preventer, C the productive cause, D the double preventer,
# and R the realization of the possible preventer
causal_model <- list(e='c & !r', r='u & !d', c=.5, u=.1, d=.5)
actual_world <- list(e=1, u=1, c=1, r=0, d=1)

compute_judgment('c', 'e', causal_model, actual_world, 'ces', s=.7)
compute_judgment('d', 'e', causal_model, actual_world, 'ces', s=.7)

compute_judgment('c', 'e', causal_model, actual_world, 'ns')
compute_judgment('d', 'e', causal_model, actual_world, 'ns')


### confounded model (E <- A -> C) ---------------------------------------------
causal_model <- list(e='a', a=.9, c='a')
actual_world <- list(e=1, a=1, c=1)

# check that we don't judge C to cause E
compute_judgment('c', 'e', causal_model, actual_world, 'ces', s=.7)
compute_judgment('c', 'e', causal_model, actual_world, 'ns')


# non-confounded model
causal_model <- list(e='a & c', a=.3, c=.6)
actual_world <- list(e=1, a=1, c=1)

compute_judgment('c', 'e', causal_model, actual_world, 'ces', s=.7)

# demonstrate that we avoid judging e to be a cause of c
compute_judgment('e', 'c', causal_model, actual_world, 'ces', s=.7)
compute_judgment('e', 'c', causal_model, actual_world, 'ns')


### more interesting confounded model ------------------------------------------
causal_model <- list(e='a | c', c='a | b', a=.5, b=.5)
actual_world_onePath <- list(e=1, a=1, c=1, b=0)

stability <- .7
compute_judgment('c', 'e', causal_model, actual_world_onePath,
                 'ces', s=stability)
compute_judgment('a', 'e', causal_model, actual_world_onePath,
                 'ces', s=stability)

actual_world_twoPaths <- list(e=1, a=1, c=1, b=1)
compute_judgment('c', 'e', causal_model, actual_world_twoPaths,
                 'ces', s=stability)
compute_judgment('a', 'e', causal_model, actual_world_twoPaths,
                 'ces', s=stability)

# (with ns model)
compute_judgment('c', 'e', causal_model, actual_world_onePath, 'ns', s=0)
compute_judgment('a', 'e', causal_model, actual_world_onePath, 'ns', s=0)

compute_judgment('c', 'e', causal_model, actual_world_twoPaths, 'ns', s=0)
compute_judgment('a', 'e', causal_model, actual_world_twoPaths, 'ns', s=0)





