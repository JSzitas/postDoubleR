#' Helper function(least squares).
#'
#' @description Helper function that dispatches to OLS for the post-double estimation.
#'
#' @param X A matrix of covariates (must be all numeric)
#' @param Y A vector of the target variable, of same length as the number of rows of Y, must be numeric
#' @param W A vector of the treatment variable, of same length as the number of rows of X, must be numeric
#' @return A list with two elements: The fitted W model and the fitted Y model.
#' @export
#' @details This function is mostly implemented to provide the option to use ols with cross fitting, though it is assumed that this will be rarely used.
#' @examples
#' \dontrun{
#'   n = 2000; p = 10
#'   X = matrix(rnorm(n*p), n, p)
#'   W = rbinom(n, 1, 0.4 + 0.2 * (X[,1] > 0))
#'   Y = pmax(X[,1], 0) * W + X[,2] + pmin(X[,3], 0) + rnorm(n)
#'
#' ols_helper(X = X,Y = Y, W = W)
#' }
#'


ols_helper <- function(X,
                       Y,
                       W ){
  if(is.matrix(X)==T){
    X <-  data.frame(X)
  }

  names(X) <- paste("X_t_", 1:ncol(X), sep = "")

  W_model <- stats::lm( formula = W ~.,
                 data = cbind(X, W) )


  Y_model <- stats::lm( formula = Y ~.,
                 data = cbind(Y, X) )



return(list( W_model, Y_model ))
}




