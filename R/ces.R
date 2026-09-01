# ces model (quillien & lucas, 2023)

# this model works in the following way. we first generate a joint distribution
# over counterfactual worlds. then we compute the Pearson correlation between 
# C=c and E=e across these counterfactual worlds.

# we analytically compute the correlation between c and e, using the formula:
# cor(x,y) = E[x*y]-E[x]*E[y] / sqrt(Var[x]*Var[y]), where E[.] is expectation
# and Var[.] is variance

# if variables have different polarity, e.g. c=0 and e=1, then we multiply this
# correlation by -1.

#' Computes a judgment with the CES model

#' @param var1 Character string giving the candidate cause variable.
#' @param var2 Character string giving the outcome variable.
#' @param aw_values A named list specifying the values of variables
#'   in the actual world. 
#' @param d The dataframe containing the counterfactual-based joint distribution
#' @param p_col Indicates which column contains the joint probability.
#'
#' @return A numeric causal judgment.
#'
#' @export
#' 
ces <- function(var1, var2, aw_values, d, p_col = "p") {
  
  # extract variables
  x <- d[[var1]]
  y <- d[[var2]]
  p <- d[[p_col]]
  
  # compute expectations
  EX <- sum(x * p)
  EY <- sum(y * p)
  EXY <- sum(x * y * p)
  # compute variances
  VarX <- sum(x^2 * p) - EX^2
  VarY <- sum(y^2 * p) - EY^2
  
  # compute correlation
  score <- (EXY - EX * EY) / sqrt(VarX * VarY)
  
  # detect if variables have the same polarity in the actual world
  xpolarity <- ifelse(aw_values[[var1]]==1, 1, -1)
  ypolarity <- ifelse(aw_values[[var2]]==1, 1, -1)
  polarity_correction <- xpolarity * ypolarity
  
  # return ces score
  return(score*polarity_correction)
  
}


