#' Post double selection.
#'
#' @description Provides a convenient function to calculate the double ML estimated debiased treatment effect \eqn{\theta}.
#'
#' @param X A matrix of covariates (must be all numeric)
#' @param Y A vector of the target variable, of same length as the number of rows of Y, must be numeric
#' @param W A vector of the treatment variable, of same length as the number of rows of X, must be numeric
#' @param method A selection of methods to use when doing post double selection.
#' @param show.progress Whether to display the simulation progress, defaults to TRUE.
#' @param k.fld How many fold crossfitting to use, defaults to 2.
#' @param simulations How many simulations to use for the final result.
#' @param validate.inputs A safety measure indicating whether the types of inputs should be checked, defaults to TRUE (disabled for custom methods).
#' @param seed.use The seed to use for simulations, defaults to 1071.
#' @param specify.own Allows the user to supply the method to calculate \eqn{\hat{W}} and \eqn{\hat{Y}}, please refer to \link[postDoubleR]{custom_helper}
#' @param ... Other arguments to be passed on, see \link[postDoubleR]{rf_helper}, \link[postDoubleR]{glmnet_helper} and \link[postDoubleR]{ols_helper} for details.
#' @return An object of class "ML_Treatment_Effects" that can be further manipulated (ie there is a plot method implemented).
#' @details Custom functions are currently implemented through a function called custom_generator. For these custom functions, refer to that function and usage examples.
#' @import progress
#' @importFrom stats sd
#' @export
#' @examples
#' \dontrun{
#'   n = 2000; p = 10
#'   X = matrix(rnorm(n*p), n, p)
#'   W = rbinom(n, 1, 0.4 + 0.2 * (X[,1] > 0))
#'   Y = pmax(X[,1], 0) * W + X[,2] + pmin(X[,3], 0) + rnorm(n)
#'
#' double_select(X, Y, W, method = c("lasso"),
#'               k.fld = 4, simulations = 1000,
#'               cv.steps = 50, Z.trans = F)
#' double_select(X, Y, W, method = c("randomforest"),
#'               k.fld = 2, simulations = 100,
#'               cv.steps = 50, Z.trans = F,
#'               num.trees = 1000, clustered = T)
#' }
#'


   double_ML <- function(X, Y, W, method = c( "glmnet",
                                              "randomforest",
                                              "nn",
                                              "ols",
                                              "custom"),
                          show.progress = TRUE,
                          specify.own = NULL,
                          k.fld = 2,
                          simulations = 100,
                          validate.inputs = TRUE,
                          seed.use = 1071,
                          ...)
{
  X <- X
  Y <- Y
  W <- W

# Check input compliance
  input_checker(X_matrix = X,
                Y_vector = Y,
                W_vector = W,
                specify.custom = specify.own,
                test.inputs = validate.inputs)

# Fit method usage, generate dispatch call
  if(method == "glmnet")
  {
    method_used <- substitute(glmnet_helper(X, Y, W, ...))
  }
  else if(method == "randomforest")
  {
    method_used <- substitute(rf_helper(X, Y, W, ...))
  }
  else if(method == "ols")
  {
    method_used <- substitute(ols_helper(X, Y, W, ...))
  }
  else if(method == "nn")
  {
    method_used <- substitute(nn_helper(X, Y, W, ...))
  }
  else if(!is.null(specify.own) && method == "custom")
  {
    method_used <- substitute(specify.own)
  }
# Or throw if the user did not actually specify a valid method
  else if(is.null(method) && is.null(specify.own)){
    stop("Please specify a method you wish to use (or supply your own function).")
  }

  set.seed(seed.use)
  dbl_ML <- list()
# Generate a progress bar so the user can watch the progress
  if(show.progress == TRUE){
    prog_bar <- progress::progress_bar$new( format = " Simulation progress [:bar] :percent eta: :eta",
                                            total = simulations)



# Generate the results. Since lists are being used the for loop should not lead to much, if any,
# inefficiency.
for( i in 1:simulations)
  {
      dbl_ML[[i]] <- fit_cross( Y_def = Y,
                                X_def = X,
                                W_def = W,
                                data.size = nrow(X),
                                fun.call = method_used,
                                k.folds = k.fld,
                                method_used = method)
      # This just increments the progress bar as each iteration of the simulation loop ends
      prog_bar$tick()
      }
  }

# Or just run without a progress bar
else
  {
  for( i in 1:simulations)
  {
    dbl_ML[[i]] <- fit_cross( Y_def = Y,
                              X_def = X,
                              W_def = W,
                              data.size = nrow(X),
                              fun.call = method_used,
                              k.folds = k.fld,
                              method_used = method)
  }
}


# Grab individual simulation estimates and generate a mean simulation estimate, plus a standard error
    dbl_means <- list()
    for(j in 1:length(dbl_ML)){
      dbl_means[[j]] <- dbl_ML[[j]][[1]]
    }


    mean_est <-  mean(unlist(dbl_means))
    std_err_means <- sd(unlist(dbl_means))
    dbl_means <- as.data.frame(unlist(dbl_means))
    colnames(dbl_means) <- "Simulation"
    rownames(dbl_means) <- NULL

# Grab individual prediction errors, generate their means and standard errors
    dbl_pred_error <- list()
    for(j in 1:length(dbl_ML)){
      dbl_pred_error[[j]] <- dbl_ML[[j]][[2]]
    }

    Estim_error_avg <- Reduce("+", dbl_pred_error)/length(dbl_pred_error)
    colnames(Estim_error_avg) <- c("Prediction_error_Y","Prediction_error_W")

# Grab the moment condition results and calculate their means, standard errors
    dbl_moment_error <- list()
    for(j in 1:length(dbl_ML)){
      dbl_moment_error[[j]] <- dbl_ML[[j]][[3]]
    }
    Moment_condition <- Reduce("+", dbl_moment_error)/length(dbl_moment_error)
    colnames(Moment_condition) <- c("Moment_condition_Y","Moment_condition_W")

    Moment_condition_all <- Reduce(rbind, dbl_moment_error)
    variance_error_condition <- apply(Moment_condition_all, MARGIN = 2, FUN = stats::sd)

    names(Moment_condition) <- c("Mean_Y_moment_condition","Mean_W_moment_condition")
    names(variance_error_condition) <- c("Std_Err_Moment_Cond_Y", "Std_Err_Moment_Cond_W")
    Moment_condition <- list(Moment_condition, variance_error_condition)
    names(Moment_condition) <- c("Means","Std_errors")

    Moment_condition_behaviour <- list(Moment_condition_all )
    names(Moment_condition_behaviour) <- "All Moment Conditions"


# Grab the heterogenous effects
dbl_heterogenous <- list()
    for(j in 1:length(dbl_ML)){
      dbl_heterogenous[[j]] <- dbl_ML[[j]][[4]]
    }

dbl_heterogenous <- suppressWarnings(Reduce( function(x1, x2){
  merge(x1, x2, all = TRUE,
        by=c("observation_index"))}, dbl_heterogenous, accumulate=F))
colnames(dbl_heterogenous) <- c("Observation_index",
                                 paste("Simulation_", 1:(ncol(dbl_heterogenous)-1),
                                                                                 sep = ""))

Avg_Heterogenous_effect <- rowMeans(dbl_heterogenous[,2:ncol(dbl_heterogenous)])
Std_Err_Heterogenous_effect <- apply(dbl_heterogenous[,2:ncol(dbl_heterogenous)],
                                     MARGIN = 1, stats::sd)
mean_heterogenous <- cbind(Avg_Heterogenous_effect, Std_Err_Heterogenous_effect)
colnames(mean_heterogenous) <- c("Treatment_Effect", "Std_Err")

    final <- list(mean_est, std_err_means, dbl_means,
                  dbl_heterogenous,
                  mean_heterogenous,
                  Estim_error_avg,
                  Moment_condition, Moment_condition_behaviour)
    class(final) <- "ML_Treatment_Effects"
    names(final) <- c( "ATE/APE","Std.Err", "Simulation results",
                       "Heterogenous_Effects",
                       "Average Heterogenous Effect + Std_err",
                       "Average estimation MSE",
                       "Mean Moment Conditions + Std_Err", "Moment Condition (All Estimates)")
  return(final)

}




