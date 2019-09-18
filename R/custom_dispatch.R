#' Custom generator
#'
#' @description Generates custom methods for use in post-double selection based on user specified inputs
#'
#' @param X A matrix of covariates
#' @param Y A vector of the target variable, of same length as the number of rows of Y
#' @param W A vector of the treatment variable, of same length as the number of rows of X
#' @param W.hat.model A function generates a model to predict from. (with a predict method)
#' @param Y.hat.model A function generates a model to predict from. (with a predict method)
#' @return A list with two elements: The fitted W model and the fitted Y model.
#' @export
#' @examples
#'
#' \dontrun{
#'  n = 2000; p = 10
#'  X = matrix(rnorm(n*p), n, p)
#'  W = rbinom(n, 1, 0.4 + 0.2 * (X[,1] > 0))
#'  Y = rbinom(n, 1,( W + 0.1 * (X[,3] > 0) ))
#'  Y[is.na(Y)] <- 1
#'
#' Y_est <- function(X,Y){
#' Y_model <- glm(Y~.,family = binomial,data = as.data.frame(cbind(X,Y)))
#' }
#'
#' W_est <- function(X,W){
#' W_model <- glm(W~.,family = binomial,data = as.data.frame(cbind(X,W)))
#' }
#'
#'  custom_helper(X = X, Y = Y, W = W,
#'                   Y.hat.model = substitute(Y_est),
#'                   W.hat.model = substitute(W_est))
#'
#'  custom_helper( X = X, Y = Y, W = W,
#'                 Y.hat.model =
#'                              expression(
#'                              glm(Y~.,family = "binomial",data = as.data.frame(cbind(X,Y)))
#'                              ),
#'                 W.hat.model =
#'                              expression(
#'                              glm(W~.,family = "gaussian",data = as.data.frame(cbind(X,Y)))
#'                              )
#'               )
#' }
#'


custom_helper <- function( X,
                           Y,
                           W,
                           Y.hat.model,
                           W.hat.model)
  {

  colnames(X) <- c(paste("X_t_", 1:ncol(X), sep = ""))
  names(Y) <- "Y_t"
  names(W) <- "W_t"

  help_envir <- new.env()
  help_envir$X <- X
  help_envir$Y <- Y
  help_envir$W <- W

  W_model <- eval(W.hat.model, envir = help_envir)

  Y_model <- eval(Y.hat.model, envir = help_envir)



  return(list(W_model, Y_model))
}

