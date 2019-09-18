#' Helper function(glmnet).
#'
#' @description Helper function that dispatches to glmnet for the post-double estimation.
#'
#' @param X A matrix of covariates (must be all numeric)
#' @param Y A vector of the target variable, of same length as the number of rows of Y, must be numeric
#' @param W A vector of the treatment variable, of same length as the number of rows of X, must be numeric
#' @param Z.trans A logical value indicating whether to standardize inputs, defaults to TRUE
#' @param cv.steps The number of folds for k-fold cross-validation of the hyperparameter tuning, defaults to 100
#' @param lambda.set.Y Allows the user to specify lambda for the Y model, defaults to null.
#' @param lambda.set.W Allows the user to specify lambda for the W model, defaults to null.
#' @param parallelize Whether to run the simulations in parallel, using every available core. Defaults to FALSE.
#' @param cores.to.use The number of cores to use. If NULL (the default), uses the maximum number of cores detected by detectCores.
#' @return A list with two elements: The fitted W model and the fitted Y model.
#' @importFrom parallel detectCores makeCluster stopCluster
#' @import doParallel
#' @details This function does not support the full range of arguments to glmnet, intentionally. If you need something specific please refer to custom_generator.
#' @export
#' @examples
#'
#' \dontrun{
#'   n = 2000; p = 10
#'   X = matrix(rnorm(n*p), n, p)
#'   W = rbinom(n, 1, 0.4 + 0.2 * (X[,1] > 0))
#'   Y = pmax(X[,1], 0) * W + X[,2] + pmin(X[,3], 0) + rnorm(n)
#'
#' glmnet_helper(X = X, Y = Y, W = W, cv.steps = 50)
#' glmnet_helper(X = X, Y = Y, W = W, Z.trans = F)
#' glmnet_helper(X = X, Y = Y, W = W, lambda.set.W = 0.7)
#'
#' }
#'
#'




glmnet_helper <- function(X,
                         Y,
                         W,
                         Z.trans = TRUE,
                         cv.steps = 100,
                         parallelize = FALSE,
                         cores.to.use = NULL,
                         lambda.set.Y = NULL,
                         lambda.set.W = NULL )
{
  colnames(X) <- paste("X_t_", 1:ncol(X), sep = "")
  names(Y) <- "Y_t"
  names(W) <- "W_t"
if(parallelize == TRUE && is.null(lambda.set.W) && is.null(lambda.set.W) )
{
  if(!is.null(cores.to.use))
  {
   cluster_used <- parallel::makeCluster(cores.to.use)
    doParallel::registerDoParallel(cluster_used)
  }
else{
  cores_used <- parallel::detectCores()
  cluster_used <- parallel::makeCluster(cores_used)
  doParallel::registerDoParallel(cluster_used)
}
    W_model <- glmnet::cv.glmnet( x = X,
                                  y = W,
                                  family = "gaussian",
                                  standardize = Z.trans,
                                  nfolds = cv.steps,
                                  parallel = TRUE)

    Y_model <- glmnet::cv.glmnet( x = X,
                                  y = Y,
                                  family = "gaussian",
                                  standardize = Z.trans,
                                  nfolds = cv.steps,
                                  parallel = TRUE)

  parallel::stopCluster(cluster_used)
}
  # no parallelism
else
  {
    if(!is.null(lambda.set.Y) && !is.null(lambda.set.W))
    {


      W_model <- glmnet::glmnet( x = X,
                                 y = W,
                                 family = "gaussian",
                                 standardize = Z.trans,
                                 lambda = lambda.set.W)

      Y_model <- glmnet::glmnet( x = X,
                                 y = Y,
                                 family = "gaussian",
                                 standardize = Z.trans,
                                 lambda = lambda.set.Y)

    }
    else if(!is.null(lambda.set.W) && is.null(lambda.set.Y))
    {
      W_model <- glmnet::glmnet( x = X,
                                 y = W,
                                 family = "gaussian",
                                 standardize = Z.trans,
                                 lambda = lambda.set.W)

      Y_model <- glmnet::cv.glmnet( x = X,
                                    y = Y,
                                    family = "gaussian",
                                    standardize = Z.trans,
                                    nfolds = cv.steps)

    }
    else if(is.null(lambda.set.W) && !is.null(lambda.set.Y))
    {
      W_model <- glmnet::cv.glmnet( x = X,
                                    y = W,
                                    family = "gaussian",
                                    standardize = Z.trans,
                                    nfolds = cv.steps)


      Y_model <- glmnet::glmnet( x = X,
                                 y = Y,
                                 family = "gaussian",
                                 standardize = Z.trans,
                                 lambda = lambda.set.Y)
    }
    else
    {
      W_model <- glmnet::cv.glmnet( x = X,
                                    y = W,
                                    family = "gaussian",
                                    standardize = Z.trans,
                                    nfolds = cv.steps)


      Y_model <- glmnet::cv.glmnet( x = X,
                                    y = Y,
                                    family = "gaussian",
                                    standardize = Z.trans,
                                    nfolds = cv.steps)

    }
  }
  return(list( W_model, Y_model ))
}


