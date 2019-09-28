#' Cross-fitting helper
#'
#' @description Implements k-fold cross-fitting with the supplied method, is a helper function that the user should not need.
#' @param Y_def An argument that control the Y input (used for argument passing)
#' @param X_def An argument that control the X input (used for argument passing)
#' @param W_def An argument that control the W input (used for argument passing)
#' @param data.size Parses the length of the dataset (nrow) for splitting.
#' @param fun.call Designates the function to use for cross-fitting.
#' @param k.folds The number of k-folds for daataset splitting, defaults to 3.
#' @param method_used The method to used when applying predict trough a helper function (do not worry about this!).
#' @return A list with two elements: The sorted effects estimated for the data, of same size as Y, and a mean estimated effect \eqn{\theta}
#' @export
#' @details This only implements the k-fold crossfitting, not the n.repeat simulation - if you intend to use this function, it works as a 'naive' double machine learning.
#' @examples
#'
#'
#'   n = 2000; p = 10
#'   X = matrix(rnorm(n*p), n, p)
#'   W = rbinom(n, 1, 0.4 + 0.2 * (X[,1] > 0))
#'   Y = pmax(X[,1], 0) * W + X[,2] + pmin(X[,3], 0) + rnorm(n)
#'
#'
#' fit_cross(Y_def = Y, X_def = X, W_def = W, data.size = 2000, fun.call = glmnet_helper(X,Y,W),
#' k.folds = 3, method_used = "glmnet")
#'
#'
#'




fit_cross <- function(Y_def,
                      X_def,
                      W_def,
                      data.size,
                      fun.call,
                      k.folds,
                      method_used)
{
      samples_to_fit <- list()
      remain_in_sample <- c(1:data.size)
##################################################################################################
#                       Generate samples to fit, using observation indexing.                     #
##################################################################################################
      while( k.folds > 0)
      {
        # You generate a sample of length('size of dataset')/'number of folds'
        # Then sort it (so it is not in a random order)
        current_sample <- sort(sample(remain_in_sample, length(remain_in_sample)/k.folds))
        # Specify that this constitutes a sample and store it for later in a list
        samples_to_fit[[k.folds]] <- current_sample
        # Remove the samples you have drawn from what remains in sample
        remain_in_sample <- setdiff(remain_in_sample, current_sample)

        k.folds <- k.folds - 1

      }
      # generate temporary variables to work better with helpers
      Y_temp <- Y_def
      X_temp <- X_def
      W_temp <- W_def


##################################################################################################
#                       Use a helper function (just to generate an enclosure)                    #
##################################################################################################
enclosure_helper <- function(Y_temp, X_temp, W_temp, fun.call, samples_to_fit){
  model_fit <- list()
  folds_to_estim <- list()
  # You go over the samples you generated previously
  for( i in 1:length(samples_to_fit))
  {
    # Taking the variables from the current sample
    Y <- Y_temp[samples_to_fit[[i]]]
    X <- X_temp[samples_to_fit[[i]],]
    W <- W_temp[samples_to_fit[[i]]]
    # Fitting the model with the variables in the current enviroment scope (hence the enviroment())
    model_fit[[i]] <- eval(fun.call, envir = environment())
    # You store this dataset for later (You will need to predict from them using other iterations of
    # the model_fit )
    sample_ids <- samples_to_fit[[i]]
    folds_to_estim[[i]] <- as.data.frame(cbind(sample_ids, Y, X, W ))
    names( folds_to_estim[[i]] ) <- c("sample_id","Y_t", paste("X_t_", 1:ncol(X), sep = ""), "W_t")
    # Paste names onto them because that helps tremendously with indexing in the cross fitting
  }
  return(list(folds_to_estim, model_fit))
}



enclosured <- enclosure_helper(Y_temp, X_temp, W_temp, fun.call, samples_to_fit)
model_fit <- enclosured[[2]]
folds_to_estim <- enclosured[[1]]


##################################################################################################
#             Call the other helper function iteratively to do k-fold crossfitting               #
##################################################################################################

helper_res <- list()

for(i in 1:length(model_fit)){
    helper_res[[i]] <- cross_fit_helper(model_W = model_fit[[i]][[1]], # grab the model for W, i
                                        model_Y = model_fit[[i]][[2]], # grab the model for Y, i
                                        folds_to_fit = folds_to_estim[-i], # grab all the data but i
                                        use = method_used) # predict has inconsistent formatting

}


# Grab the resulting mean
mean_res <- list()
for( i in 1:length(helper_res))
{
  mean_res[[i]] <- unlist(helper_res[[i]][[1]])
}
  mean_res <- mean(unlist(mean_res))
# Grab the resulting errors in prediction of the models
error_res <- list()
  for( i in 1:length(helper_res))
  {
    error_res[[i]] <- helper_res[[i]][[2]]
  }

# Average the errors of prediction, giving the average MSE
error_res <- lapply(error_res, FUN = function(i){Reduce("+",i)/length(i)})
error_res <- Reduce("+", error_res)/length(error_res)
colnames(error_res) <- c("Avg_MSE_Y","Avg_MSE_W")

# Grab the results of the moment conditions check ( E[V|X] = 0, E[U|X] = 0)
moment_condition_res <- list()
for( i in 1:length(helper_res))
{
  moment_condition_res[[i]] <- helper_res[[i]][[3]]
}

# Average the results of the moment conditions
moment_condition_res <- lapply(moment_condition_res, FUN = function(i){Reduce("+",i)/length(i)})
moment_condition_res <- Reduce("+", moment_condition_res)/length(moment_condition_res)
colnames(moment_condition_res) <- c("Avg_Resid_Y","Avg_Resid_W")

# Grab the heterogenous treatment effects
heterogenous_effects <- list()
for( i in 1:length(helper_res))
{
  heterogenous_effects[[i]] <- helper_res[[i]][[4]]
  heterogenous_effects[[i]] <- Reduce( rbind, heterogenous_effects[[i]] )
}


heterogenous_effects <- suppressWarnings(Reduce( function(x1, x2){
  merge(x1, x2, all = TRUE,
        by=c("observation_index"))}, heterogenous_effects, accumulate=F))
observation_index <- heterogenous_effects[,"observation_index"]

heterogenous_effect <- rowMeans(
  heterogenous_effects[,!names(heterogenous_effects)
                       %in% c("observation_index")],
                                                     na.rm = TRUE)
heterogenous_effects <- cbind( observation_index, heterogenous_effect )

return(list( mean_res, error_res, moment_condition_res, heterogenous_effects ))
}




