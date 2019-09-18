#' Helper function to for cross-fitting
#'
#' @description Fits estimated models to out of fold samples
#'
#' @param model_W The model used to produce the estimates of W_hat
#' @param model_Y The model used to produce the estimates of Y_hat
#' @param folds_to_fit The split folds for which the calculations are being run.
#' @param use The method to use when calculating the out of fold prediction (propagates from method).
#' @export
#' @importFrom stats predict
#' @import grf
#' @import glmnet
#' @examples
#'
#' \dontrun{
#'  cross_fit_helper( model_W = W_fit_model,
#'                    model_Y = Y_fit_model,
#'                    folds_to_fit = other_folds,
#'                    use = "glmnet")
#'
#' }



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
