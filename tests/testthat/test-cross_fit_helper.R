set.seed(1071)
n = 2000; p = 10
X = matrix(rnorm(n*p), n, p)
W = rbinom(n, 1, 0.4 + 0.2 * (X[,1] > 0))
Y = pmax(X[,1], 0) * W + X[,2] + pmin(X[,3], 0) + rnorm(n)


test_that("Cross fitting works with glmnet", {

  cross_glmnet <- fit_cross(Y_def = Y,
                           X_def = X,
                           W_def = W,
                           data.size = 2000,
                    fun.call = substitute(glmnet_helper(X, Y, W, parallelize = FALSE)),
                    k.folds = 3,
                    method_used = "glmnet")

  expect_lte( cross_glmnet[[1]], 0.65 )
  expect_gte( cross_glmnet[[1]], 0.15 )
  expect_true(is.numeric(cross_glmnet[[2]]))
  expect_lte(cross_glmnet[[2]][1],2)
  expect_lte(cross_glmnet[[2]][2],2)
  expect_true(is.numeric(cross_glmnet[[3]]))
  expect_equal(colnames(cross_glmnet[[3]]), c("Avg_Resid_Y","Avg_Resid_W"))


})


test_that("Cross fitting works with ols", {

  cross_ols <- fit_cross(Y_def = Y,
                        X_def = X,
                        W_def = W,
                        data.size = 2000,
                        fun.call = substitute(ols_helper(X, Y, W)),
                        k.folds = 3,
                        method_used = "ols")

  expect_lte( cross_ols[[1]], 0.65 )
  expect_gte( cross_ols[[1]], 0.15 )
  expect_true(is.numeric(cross_ols[[2]]))
  expect_lte(cross_ols[[2]][1],2)
  expect_lte(cross_ols[[2]][2],2)
  expect_true(is.numeric(cross_ols[[3]]))
  expect_equal(colnames(cross_ols[[3]]), c("Avg_Resid_Y","Avg_Resid_W"))

})

test_that("Cross fitting works with custom methods", {

  W_mod <- expression( glm( W~.,
                            family = "binomial",
                            data = as.data.frame(cbind(X,W))))

  Y_mod <- expression( glm( Y~.,
                            family = "gaussian",
                            data = as.data.frame(cbind(X,Y))))

  cross_custom <- fit_cross(Y_def = Y,
                         X_def = X,
                         W_def = W,
                         data.size = 2000,
                         fun.call = substitute(custom_helper( X = X,
                                                              Y = Y,
                                                              W = W,
                                                              Y.hat.model = Y_mod,
                                                              W.hat.model = W_mod)),
                         k.folds = 3,
                         method_used = "custom")

  expect_lte( cross_custom[[1]], 0.65 )
  expect_gte( cross_custom[[1]], 0.15 )
  expect_true(is.numeric(cross_custom[[2]]))
  expect_lte(cross_custom[[2]][1],2)
  expect_lte(cross_custom[[2]][2],2)
  expect_true(is.numeric(cross_custom[[3]]))
  expect_equal(colnames(cross_custom[[3]]), c("Avg_Resid_Y","Avg_Resid_W"))

})



test_that("Cross fitting works with rf", {

  skip_on_cran()
  skip_on_travis()
  cross_rf <- fit_cross(Y_def = Y,
                        X_def = X,
                        W_def = W,
                        data.size = 2000,
                        fun.call = substitute(rf_helper(X, Y, W)),
                        k.folds = 3,
                        method_used = "randomforest")


  expect_lte( cross_rf[[1]], 0.65 )
  expect_gte( cross_rf[[1]], 0.15 )
  expect_true(is.numeric(cross_rf[[2]]))
  expect_lte(cross_rf[[2]][1],2)
  expect_lte(cross_rf[[2]][2],2)
  expect_true(is.numeric(cross_rf[[3]]))
  expect_equal(colnames(cross_rf[[3]]), c("Avg_Resid_Y","Avg_Resid_W"))


})


