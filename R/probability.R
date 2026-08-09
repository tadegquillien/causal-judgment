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
