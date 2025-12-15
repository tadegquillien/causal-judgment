## this script contains the main functions used by both computational models

# -create_structural_function() parses the structural equations and exogenous 
# probabilities

# -make_function_list() applies create_structural_function() to each variable

# -compute_probabilities() computes the probability distribution over 
# counterfactual worlds

# -compute_judgment() is a wrapper that allows one to call a computational model
# (either the CES or NS model). The models themselves are defined in the 'ces.R'
# and 'ns.R' scripts

### load packages --------------------------------------------------------------

library(tidyverse)


### create_structural_function() -----------------------------------------------

# we define a causal model by using strings (for a structural equation)
# and exogenous probabilities. For example we would define the structural 
# equation 'E := A or B' by typing e='a|b'.
# the present function parses this string (or number, for exogenous 
# probabilities) and return a function implementing the structural equation or 
# exogenous probability


create_structural_function <- function(equation_string) {
  
  # this function creates a function in the following way:
  # if we enter create_structural_equation('a & b'), it will create the function
  # function(a,b){return(a&b)}
  # i.e. the created function turns the string into a boolean evaluation
  
  # the main challenge here is to identify the variables, e.g. extract a and b 
  # from 'a & b' so that they can be placed in the arguments slot of the 
  # function
  
  
  # extract potential variable names from the equation (including those with 
  # negation)
  all_matches <- regmatches(equation_string,
                            gregexpr("!?[a-zA-Z0-9_\\.]+", 
                                     equation_string))[[1]]
  # exogenous probabilities will be caught by the previous step, so we want
  # to filter them out
  is_pure_number <- grepl("^!?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)$", all_matches)
  filtered_vars <- all_matches[!is_pure_number]
  
  # remove the ! prefix if present to get just the variable names
  variables <- unique(gsub("^!", "", filtered_vars))
  
  # create the function arguments string
  args_string <- paste(variables, collapse = ", ")
  
  # create the function body
  function_body <- paste0("function(", args_string, ") {\n  return(", 
                          equation_string, ")\n}")
  
  # parse the function
  prob_function <- eval(parse(text = function_body))
  
  return(prob_function)
}



# example usage:
# create function for E = A & B
probE <- create_structural_function("!a_c_ & B")
print(probE)

# Create function for more complex equation: F = (A | B) & C
probF <- create_structural_function("(a2 | b) & c")
print(probF)

# for an exogenous probability
create_structural_function(.9)


### make_function_list() -------------------------------------------------------

# this function creates a function for each variable
# (for exogenous variables this is just a constant function returning the 
# exogenous probability; for endogenous variables this is the structural 
# equation)
make_function_list <- function(vars){
  functionList <- list() # initialize list
  for (var in names(vars)){ # cycle through variable
    # create function
    functionList[[var]] <- create_structural_function(vars[var]) 
  }
  return(functionList)
}

### compute_probabilities ------------------------------------------------------

# verification function: check whether an endogenous variable has a value that 
# is consistent with the value of its parents 
verif <- function(outcome, args, fun){
  return(do.call(fun, as.list(args))==outcome)
}


# function to create the joint probability distribution induced by the causal 
# model and actual world
compute_probabilities <- function(structural_functions, actual_world, s=0){
  
  # initialize the dataframe representing the joint distribution
  create_df <- function(structural_functions){
    # Extract variable names
    var_names <- names(structural_functions)
    
    # Create all combinations of 0/1 for each variable
    df <- expand.grid(rep(list(c(0, 1)), length(var_names)))
    
    # Set proper column names
    colnames(df) <- var_names
    
    # return the dataframe
    return(df)
  }
  
  # create dataframe
  d <- create_df(structural_functions)
  
  var_names <- names(structural_functions) # extract variable names from model
  # enter probabilities
  for (var in var_names){ # cycle over variable names
    fun <- structural_functions[[var]] # extract function
    args <- names(formals(fun)) # extract function arguments
    # initialize new column with probability of the current variable
    # it will be called pvar, where var is the variable name (e.g. if the 
    # variable it called 'a' this column will be called 'pa')
    d[[paste('p', var, sep='')]] <- 0 
    if(length(args)==0){ # if variable is exogenous
      for (i in 1:nrow(d)){
        prob <- fun() # extract p(var=1)
        prob <- s*actual_world[[var]]+(1-s)*prob # apply s parameter
        d[i,ncol(d)] <- ifelse(d[i,var], prob, 1-prob) # enter exogenous prob
      }
    }
    if(length(args)>0){ # if variable is endogenous
      for(i in 1:nrow(d)){
        outcome <- d[i,var] # extract value of the variable
        prob <- as.numeric(
          verif(outcome, d[i,args], fun) # check consistency
        )
        # enter conditional probability (1 if consistent, 0 otherwise)
        d[i,ncol(d)] <- prob 
      }
    }
  }
  
  # enter joint probability
  # we'll take the product of the colnumber/2 last columns, with colnumber the
  # existing number of columns: these are the probability columns
  # (note that by construction pvar for an endogenous variable is the 
  # probability of that variable value conditioned on the value of its parents, 
  # such that we naturally obtain p as the product of all probability columns)
  
  colnumber <- ncol(d)
  d$p <- 0
  for(i in 1:nrow(d)){
    d[i,]$p <- prod(d[i, ((colnumber/2)+1):colnumber])
  }
  return(d)
  
}


### compute_judgment() ---------------------------------------------------------

# this is the general causal judgment function. it is essentially a wrapper over
# the ces() and ns() function, so in order to use it we first need to define
# these functions (run the 'ces.R' and 'ns.R' files to do this).

compute_judgment <- function(var, outcome, causal_model, actual_world, 
                             model, s=0){
  
  ## check for errors in data entry
  
  var_names <- names(causal_model) # extract variable names
  
  # check that names of variables and actual_world are the same
  if(!setequal(var_names, names(actual_world))){
    stop('error: variable names must match across lists.')
  }
  
  # check that no variable starts with 'p'
  for (n in var_names){
    if (substr(n,1,1)=='p'){
      stop('error: please do not use variable names starting with P.\n
           Please rename variable ', n, '.')
    }
  }
  # check that the target and outcome variables belong to the causal model
  if(!(var %in% var_names)){
    stop('error: cause variable is not defined in the causal model.')
  }
  if(!(outcome %in% var_names)){
    stop('error: outcome variable is not defined in the causal model.')
  }  
  
  ## compute causal score
  structural_functions <- make_function_list(causal_model) # create causal model
  # enter joint probabilities
  df <- compute_probabilities(structural_functions, actual_world, s) 
  
  # detect if C is endogenous
  fun <- structural_functions[[var]] # extract function for C
  args <- names(formals(fun)) # extract function arguments
  # if variable is endogenous
  if(length(args)>0){ 
    # compute marginal probability
    marginal_pvar <- sum(df[[var]]*df[[paste('p', var, sep='')]]*df$p)
    # re-compute pc as the marginal probability
    df[[paste('p', var, sep='')]] <- ifelse(df[[var]]==actual_world[[var]], 
                                           marginal_pvar, 
                                           1-marginal_pvar)
    # re-compute p (the probability of each world) 
    colnumber <- ncol(df)
    df$p <- 0
    for(i in 1:nrow(df)){
      df[i,]$p <- prod(df[i, ((colnumber/2)+1):colnumber])
    }
  }
  
  
  # select model and compute score
  if(model == 'ces'){
    score <- ces(var, outcome, actual_world, df)
  }
  if(model == 'ns'){
    score <- ns(var, outcome, actual_world, df, structural_functions)
  }
  return(score)
  
}



