#' Helper function for input checking
#'
#'
#' @description This functions checks the input to the double_select function for compliance with the default methods used, confirming that the inputs are viable. It is a helper - no interaction with the user should happen unless the user explicitly calls this function.
#'
#' @param X_matrix A matrix of covariates (must be all numeric)
#' @param Y_vector A vector of the target variable, of same length as the number of rows of X, must be numeric
#' @param W_vector A vector of the treatment variable, of same length as the number of rows of X, must be numeric
#' @param specify.custom disables input checking by default, and leaves it to the user.
#' @param test.inputs If set to FALSE disables input checking, the default is TRUE.
#' @return A message indicating the status of input checking, or lack thereof.
#' @details The function will throw a warning if it is disabled ( the input is not checked) or stop the processing if the input is checked, but incorrect. Disables checking for custom functions by default.
#' @export
#' @examples
#'
#'   n = 2000; p = 10
#'   X = matrix(rnorm(n*p), n, p)
#'   W = rbinom(n, 1, 0.4 + 0.2 * (X[,1] > 0))
#'   Y = pmax(X[,1], 0) * W + X[,2] + pmin(X[,3], 0) + rnorm(n)
#'
#' input_checker(X_matrix = X, Y_vector = Y, W_vector = W,
#' specify.custom = NULL, test.inputs = TRUE)
#'
#'



input_checker <- function(X_matrix,
                          Y_vector ,
                          W_vector,
                          specify.custom,
                          test.inputs = TRUE){
  if(!is.null(specify.custom) || test.inputs == FALSE)
  {
    # continue without any checking
    warning("Your inputs will not be checked for compliance with your method,
      please check your types are correct manually!")
  }
  else{
    if(test.inputs == TRUE)
    {
      # check input
      try_X <- is.matrix(X_matrix)
      if(try_X != TRUE ) stop("Your X is not a matrix. Disable input checking if this is not a problem.")

      test_all_X <- apply(X_matrix,2,is.numeric)
      if(sum(test_all_X) < ncol(X_matrix))
       {
        stop("Your covariates are not all numeric. Disable input checking if this is not a problem.")
       }





      try_Y <- is.numeric(Y_vector)
      if(try_Y != TRUE) stop("Please supply a numeric Y.")
      try_W <- is.numeric(W_vector)
      if(try_W != TRUE) stop("Please supply a numeric W.")

      message("Input is correct")
    }
  }
}
