#' Helper function(randomforest).
#'
#' @description Helper function that dispatches to random forest (grf) for the post-double estimation.
#'
#' @param X A matrix of covariates (must be all numeric)
#' @param Y A vector of the target variable, of same length as the number of rows of Y, must be numeric
#' @param W A vector of the treatment variable, of same length as the number of rows of X, must be numeric
#' @param orthog.boost Whether to use orthogonal boosting, defaults to FALSE.
#' @param tree.n Controls the number of trees grown, defaults to 2000.
#' @param tune Whether to use hyperparameter tuning.
#' @param clustered Whether to use cluster robust forests, defaults to FALSE.
#' @return A list with two elements: The fitted W model and the fitted Y model.
#' @details If you need to use something that is not a default argument here, please refer to custom_generator. Not using honesty is heavily advised, though, as that could lead to a very high splitting, and the honesty is used for essentialy the same reason as crossfitting.
#' @import grf
#' @export
#' @examples
#' \dontrun{
#'   n = 2000; p = 10
#'   X = matrix(rnorm(n*p), n, p)
#'   W = rbinom(n, 1, 0.4 + 0.2 * (X[,1] > 0))
#'   Y = pmax(X[,1], 0) * W + X[,2] + pmin(X[,3], 0) + rnorm(n)
#'
#'
#'
#' rf_helper(X = X, Y = Y, W = W,tree.n = 1000)
#' rf_helper(X = X, Y = Y, W = Z, orthog.boost = T, tune = T)
#' }
#'



rf_helper <- function(X,
                      Y,
                      W,
                      orthog.boost = F,
                      tree.n = 2000,
                      tune = T,
                      clustered = NULL
                         ){

if(orthog.boost == F){
  # work without orthogonal boosting
  W_model <- grf::regression_forest(Y = W,
                                    X = X,
                                    num.trees = tree.n,
                                    tune.parameters = tune,
                                    clusters = clustered,
                                    honesty = FALSE)

  Y_model <- grf::regression_forest(Y = Y,
                                    X = X,
                                    num.trees = tree.n,
                                    tune.parameters = tune,
                                    clusters = clustered,
                                    honesty = FALSE)

  }
else {
  # use boosted_regression_forest instead
  W_model <- grf::boosted_regression_forest(Y = W, X = X,
                                            num.trees = tree.n,
                                            tune.parameters = tune,
                                            clusters = clustered,
                                            honesty = FALSE)

  Y_model <- grf::boosted_regression_forest(Y = Y, X = X,
                                            num.trees = tree.n,
                                            tune.parameters = tune,
                                            clusters = clustered,
                                            honesty = FALSE)

}
  return(list( W_model, Y_model ))
}


