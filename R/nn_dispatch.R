#' Helper function for neural networks fitted by neuralnet
#'
#' @description Helper function that dispatches to neuralnet for the double ML estimation (see details).
#'
#' @param X A matrix of covariates (must be all numeric)
#' @param Y A vector of the target variable, of same length as the number of rows of Y, must be numeric
#' @param W A vector of the treatment variable, of same length as the number of rows of X, must be numeric
#' @param neural.net.W A model specification for W, see \link[neuralnet]{neuralnet}
#' @param neural.net.Y A model specification for Y, see \link[neuralnet]{neuralnet}
#' @param standardize Whether to standardize the data before starting the computation, defaults to TRUE.
#' @param standardization.method How to standardize data, defaults to min-max, also offers "Z-transform", "Unit-Scale" and "Mean-Scale"
#' @return A list with two elements: The fitted W model and the fitted Y model.
#' @importFrom stats formula
#' @import neuralnet
#' @export
#' @details For a more steamlined usage, default arguments as implemented in the neuralnet package
#' are passed to both networks during fitting, unless specified otherwise. Also, any attempt to set
#' the formula or data arguments of neuralnet will be ignored and rewritted with internal structures.
#' The function will print a warning if this happens.
#' @examples
#' \dontrun{
#'   n = 2000; p = 10
#'   X = matrix(rnorm(n*p), n, p)
#'   W = rbinom(n, 1, 0.4 + 0.2 * (X[,1] > 0))
#'   Y = pmax(X[,1], 0) * W + X[,2] + pmin(X[,3], 0) + rnorm(n)
#'
#'  nn_helper( X,
#'             Y,
#'             W,
#'             neural.net.W = list( act.fct = "logistic" ),
#'             neural.net.Y = list( act.fc = "tanh" ))
#'
#' }
#'


nn_helper <- function(X,
                      Y,
                      W,
                      neural.net.Y = NULL,
                      neural.net.W = NULL,
                      standardize = TRUE,
                      standardization.method = "min-max"
                      ){
  if(is.matrix(X)==T){
    X <- data.frame(X)
  }

if( standardize == TRUE)
{
  if( standardization.method %in% c( "min-max" ) )
  {
    X <- lapply( colnames(X),
                 FUN = function(i){
                   X[,i] = (X[,i] - min(X[,i]) )/(max(X[,i]) - min(X[,i]))
                 }
    )
    X <- do.call( cbind, X)
  }
  else if( standardization.method %in% c( "Z-transform" ) )
  {
    X <- lapply( colnames(X),
                 FUN = function(i){
                   X[,i] = (X[,i] - mean(X[,i]) )/stats::sd(X[,i])
                 }
    )
    X <- do.call( cbind, X)
  }
  else if( standardization.method %in% c( "Unit-Scale" ) )
  {
    X <- lapply( colnames(X),
                 FUN = function(i){
                   X[,i] = X[,i]/sqrt(sum(X[,i]^2))
                 }
    )
    X <- do.call( cbind, X)
  }
  else if(standardization.method %in% c( "Mean-Scale" ) )
  {
    X <- lapply( colnames(X),
                 FUN = function(i){
                   X[,i] = (X[,i] - mean(X[,i]) )/(max(X[,i]) - min(X[,i]))
                 }
    )
    X <- do.call( cbind, X)
  }
  else
  {
    X <- X
  }
}
  colnames(X) <- paste("X_t_", 1:ncol(X), sep = "")
  if(is.null(neural.net.W))
    {
    neural.net.W <- list( hidden = 1,
                        threshold = 0.01,
                        stepmax = 1e+05,
                        rep = 1,
                        startweights = NULL,
                        learningrate.limit = NULL,
                        learningrate.factor = list( minus = 0.5,
                                                    plus = 1.2),
                        learningrate = NULL,
                        lifesign = "none",
                        lifesign.step = 1000,
                        algorithm = "rprop+",
                        err.fct = "sse",
                        act.fct = "logistic",
                        linear.output = TRUE,
                        exclude = NULL,
                        constant.weights = NULL,
                        likelihood = FALSE )
  }
  if(!is.null(neural.net.W$formula) || !is.null(neural.net.W$data))
  {
    warning("You specified a formula and/or a data argument to neuralnet.
    Ignoring them and replacing them with internal constructs.")
    neural.net.W$formula <- NULL
    neural.net.W$data <- NULL
  }



 W_model_formula <- formula(paste('W~',paste(colnames(X),collapse = '+'),sep=""))

 W_model <- do.call( neuralnet::neuralnet, c( list( formula = W_model_formula,
                                                   data = cbind(X,W)),
                                                   neural.net.W ),
                     quote = TRUE )





  if(is.null(neural.net.Y))
  {
    neural.net.Y <- list( hidden = 1,
                          threshold = 0.01,
                          stepmax = 1e+05,
                          rep = 1,
                          startweights = NULL,
                          learningrate.limit = NULL,
                          learningrate.factor = list( minus = 0.5,
                                                      plus = 1.2),
                          learningrate = NULL,
                          lifesign = "none",
                          lifesign.step = 1000,
                          algorithm = "rprop+",
                          err.fct = "sse",
                          act.fct = "logistic",
                          linear.output = TRUE,
                          exclude = NULL,
                          constant.weights = NULL,
                          likelihood = FALSE )
  }
  if(!is.null(neural.net.Y$formula) || !is.null(neural.net.Y$data))
  {
    warning("You specified a formula and/or a data argument to neuralnet.
    Ignoring them and replacing them with internal constructs.")
    neural.net.Y$formula <- NULL
    neural.net.Y$data <- NULL
  }

  Y_model_formula <- formula(paste('Y~',paste(colnames(X),collapse = '+'),sep=""))

  Y_model <- do.call( neuralnet::neuralnet, c( list( formula = Y_model_formula,
                                                     data = cbind(X,Y)),
                                               neural.net.Y ),
                      quote = TRUE )

  if( is.null( Y_model$weights ) || is.null( W_model$weights ) ){
    stop("Your neural network did not converge - stopping. Check your neuralnet arguments, or try something else.")
  }

  return(list( W_model, Y_model ))
}
