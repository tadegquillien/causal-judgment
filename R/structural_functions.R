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