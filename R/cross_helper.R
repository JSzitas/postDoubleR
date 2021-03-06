#' Helper function to for cross-fitting
#'
#' @description Fits estimated models to out of fold samples
#'
#' @param model_W The model used to produce the estimates of W_hat
#' @param model_Y The model used to produce the estimates of Y_hat
#' @param folds_to_fit The split folds for which the calculations are being run.
#' @param use The method to use when calculating the out of fold prediction (propagates from method).
#' @return A list with four elements: The mean estimate of \eqn{\theta}, the standard error of the mean estimate, the associated  moment conditions, and the estimated heterogenous effects for the single batch of a single run of the simulation.
#' @export
#' @importFrom stats predict
#' @examples
#'
#'
#' n = 1000
#' p = 10
#' X = matrix(rnorm(n*p),n,p)
#' W = rbinom(n, 1, 0.4 + 0.2 * (X[,1] > 0))
#' Y = pmax(X[,1], 0) * W + X[,2] + pmin(X[,3], 0) + rnorm(n)
#'
#' fit_on <- sample(1:1000, size = 333)
#' pred_on_1 <- sample(c(1:1000)[-fit_on], size = 333)
#' pred_on_2 <- c(1:1000)[-c(fit_on,pred_on_1)]
#' models <- ols_helper( X = X[fit_on,],
#'                       Y = Y[fit_on],
#'                       W = W[fit_on] )
#'
#' folds_fit <- list()
#' folds_fit[[1]] <- data.frame(cbind(pred_on_1, X[pred_on_1,], W[pred_on_1], Y[pred_on_1]))
#' folds_fit[[2]] <- data.frame(cbind(pred_on_2, X[pred_on_2,], W[pred_on_2], Y[pred_on_2]))
#'
#' for(i in 1:length(folds_fit)){
#'   names( folds_fit[[i]] ) <- c("sample_id","Y_t", paste("X_t_", 1:ncol(X), sep = ""), "W_t")
#'   }
#'
#' cross_fit_helper( model_W = models[[1]],
#'                   model_Y = models[[2]],
#'                   folds_to_fit = folds_fit,
#'                   use = "ols")
#'
#'



cross_fit_helper <- function( model_W,
                              model_Y,
                              folds_to_fit,
                              use)
{
  theta_est <- list()
  prediction_errors <- list()
  MPE <- list()
  heterogenous_effects <- list()


# Since predict is inconsistent, we have to use various calls, depending on the method.
  if(use == "ols")
{
    for(i in 1:length(folds_to_fit))
    {
      new_df <-  folds_to_fit[[i]][,!( names(folds_to_fit[[i]])
                                       %in% c("Y_t","W_t"))]

      pred_W <- predict( model_W, newdata = new_df )

      pred_Y <- predict( model_Y, newdata = new_df )

      W_curr <- folds_to_fit[[i]][,"W_t"]
      Y_curr <- folds_to_fit[[i]][,"Y_t"]

      W_res <- W_curr - pred_W
      # The ATE/APE prediction for the current fold based on the provided models
      theta_est[[i]] <- mean( W_res *(Y_curr - pred_Y) )/mean(W_res * W_curr)
      # The MSE of prediction for both models
      prediction_errors[[i]] <- cbind( mean((Y_curr - pred_Y)^2), mean((W_curr - pred_W)^2))
      # The moment conditions for both models
      MPE[[i]] <- cbind( mean((W_res *(Y_curr - pred_Y)/mean(W_res * W_curr)) - Y_curr),
                         mean(W_curr - pred_W))

      heterogenous_effect <- (W_res *(Y_curr - pred_Y))/mean(W_res * W_curr)
      observation_index <- folds_to_fit[[i]][,"sample_id"]
      heterogenous_effects[[i]] <- cbind(observation_index, heterogenous_effect)

    }
}
##########################################################################################
  if(use == "glmnet")
{
    for(i in 1:length(folds_to_fit))
    {
      new_df <- as.matrix(folds_to_fit[[i]][,!( names(folds_to_fit[[i]])
                                                 %in% c("sample_id","Y_t","W_t"))])


      pred_W <- predict( model_W, newx = new_df )

      pred_Y <- predict( model_Y, newx = new_df )

      W_curr <- folds_to_fit[[i]][,"W_t"]
      Y_curr <- folds_to_fit[[i]][,"Y_t"]

      W_res <- W_curr - pred_W

      theta_est[[i]] <- mean( W_res *(Y_curr - pred_Y) )/mean(W_res * W_curr)
      prediction_errors[[i]] <- cbind( mean((Y_curr - pred_Y)^2), mean((W_curr - pred_W)^2))
      MPE[[i]] <- cbind( mean((W_res *(Y_curr - pred_Y)/mean(W_res * W_curr)) - Y_curr),
                         mean(W_curr - pred_W))

      heterogenous_effect <- (W_res *(Y_curr - pred_Y))/mean(W_res * W_curr)
      observation_index <- folds_to_fit[[i]][,"sample_id"]
      heterogenous_effects[[i]] <- cbind(observation_index, heterogenous_effect)
    }
}
##########################################################################################
  if(use == "randomforest")
{
    for(i in 1:length(folds_to_fit))
    {
      new_df <- as.matrix( folds_to_fit[[i]][,!( names(folds_to_fit[[i]])
                                                 %in% c("sample_id","Y_t","W_t"))])
      pred_W <- predict( model_W, newdata = new_df )
      pred_W <- pred_W$predictions

      pred_Y <- predict( model_Y, newdata = new_df )
      pred_Y <- pred_Y$predictions

      W_curr <- folds_to_fit[[i]][,"W_t"]
      Y_curr <- folds_to_fit[[i]][,"Y_t"]

      W_res <- W_curr - pred_W

      theta_est[[i]] <- mean( W_res *(Y_curr - pred_Y) )/mean(W_res * W_curr)
      prediction_errors[[i]] <- cbind( mean((Y_curr - pred_Y)^2), mean((W_curr - pred_W)^2))
      MPE[[i]] <- cbind( mean((W_res *(Y_curr - pred_Y)/mean(W_res * W_curr)) - Y_curr),
                         mean(W_curr - pred_W))

      heterogenous_effect <- (W_res *(Y_curr - pred_Y))/mean(W_res * W_curr)
      observation_index <- folds_to_fit[[i]][,"sample_id"]
      heterogenous_effects[[i]] <- cbind(observation_index, heterogenous_effect)
    }
  }
##########################################################################################
  if(use == "nn")
  {
    for(i in 1:length(folds_to_fit))
    {
      new_df <-  folds_to_fit[[i]][,!( names(folds_to_fit[[i]])
                                       %in% c("sample_id","Y_t","W_t"))]

      pred_W <- predict( model_W, newdata = new_df )

      pred_Y <- predict( model_Y, newdata = new_df )

      W_curr <- folds_to_fit[[i]][,"W_t"]
      Y_curr <- folds_to_fit[[i]][,"Y_t"]

      W_res <- W_curr - pred_W
      # The ATE/APE prediction for the current fold based on the provided models
      theta_est[[i]] <- mean( W_res *(Y_curr - pred_Y) )/mean(W_res * W_curr)
      # The MSE of prediction for both models
      prediction_errors[[i]] <- cbind( mean((Y_curr - pred_Y)^2), mean((W_curr - pred_W)^2))
      # The moment conditions for both models
      MPE[[i]] <- cbind( mean((W_res *(Y_curr - pred_Y)/mean(W_res * W_curr)) - Y_curr),
                         mean(W_curr - pred_W))

      heterogenous_effect <- (W_res *(Y_curr - pred_Y))/mean(W_res * W_curr)
      observation_index <- folds_to_fit[[i]][,"sample_id"]
      heterogenous_effects[[i]] <- cbind(observation_index, heterogenous_effect)

    }
  }
##########################################################################################

# Due to the inconsistent behaviour of predict, the best thing to do is try calling it
# And then stopping if it fails to produce a numeric result.
# You could do the same thing with tryCatch - but I am unsure whether that would be better
# as this is easier to understand coming from the R side, and accomplishes the same thing.
  if(use == "custom")
{
    for(i in 1:length(folds_to_fit))
    {
      new_df <- as.matrix( folds_to_fit[[i]][,!( names(folds_to_fit[[i]])
                                                 %in% c("sample_id","Y_t","W_t"))])

      pred_W <- predict( model_W, as.data.frame(new_df) )

      pred_Y <- predict( model_Y, as.data.frame(new_df) )

      if(!is.numeric(pred_W) || !is.numeric(pred_Y))
      {
        stop("Predict call failed for custom method, please contact the maintainer.")
      }

      W_curr <- folds_to_fit[[i]][,"W_t"]
      Y_curr <- folds_to_fit[[i]][,"Y_t"]

      W_res <- W_curr - pred_W

      theta_est[[i]] <- mean( W_res *(Y_curr - pred_Y) )/mean(W_res * W_curr)
      prediction_errors[[i]] <- cbind( mean((Y_curr - pred_Y)^2), mean((W_curr - pred_W)^2))
      MPE[[i]] <- cbind( mean((W_res *(Y_curr - pred_Y)/mean(W_res * W_curr)) - Y_curr),
                         mean(W_curr - pred_W))

      heterogenous_effect <- (W_res *(Y_curr - pred_Y))/mean(W_res * W_curr)
      observation_index <- folds_to_fit[[i]][,"sample_id"]
      heterogenous_effects[[i]] <- cbind(observation_index, heterogenous_effect)
    }
}

return(list( theta_est, prediction_errors, MPE, heterogenous_effects ))
}
