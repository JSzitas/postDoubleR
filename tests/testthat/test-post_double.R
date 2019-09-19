set.seed(1071)
n = 2000; p = 10
X = matrix(rnorm(n*p), n, p)
W = rbinom(n, 1, 0.4 + 0.2 * (X[,1] > 0))
Y = pmax(X[,1], 0) * W + X[,2] + pmin(X[,3], 0) + rnorm(n)

test_that(" Double ML selection works with glmnet", {

  double_glmnet <- double_ML(X, Y, W, method = c("glmnet"),
                              k.fld = 4,
                              simulations = 50,
                              show.progress = FALSE,
                              lambda.set.Y = 1,
                              lambda.set.W = 1,
                              Z.trans = TRUE,
                              parallelize = FALSE)




  expect_lte(double_glmnet[[1]], 0.55)
  expect_gte(double_glmnet[[1]], 0.35)
  expect_true(is.list(double_glmnet))
  expect_true(is.data.frame(double_glmnet[[3]]))
})


test_that(" Double ML selection works with ols", {

    double_ols <- double_ML(X, Y, W, method = c("ols"),
                                show.progress = FALSE,
                                k.fld = 4,
                                simulations = 50)




  expect_lte(double_ols[[1]], 0.55)
  expect_gte(double_ols[[1]], 0.35)
  expect_true(is.list(double_ols))
  expect_true(is.data.frame(double_ols[[3]]))

})




test_that(" Double ML selection works with random forests", {
  skip_on_cran()
  skip_on_travis()

  double_rf <- double_ML(X, Y, W, method = c("randomforest"),
                             k.fld = 2, simulations = 10,
                             tree.n = 200,
                             show.progress = FALSE,
                             tune = FALSE)
  double_rf_tune <- double_ML(X, Y, W, method = c("randomforest"),
                                  k.fld = 2, simulations = 10,
                                  tree.n = 200,
                                  show.progress = FALSE,
                                  tune = TRUE)




  expect_lte(double_rf[[1]], 0.55)
  expect_gte(double_rf[[1]], 0.20)
  expect_true(is.list(double_rf))
  expect_true(is.data.frame(double_rf[[3]]))




  expect_lte(double_rf_tune[[1]], 0.55)
  expect_gte(double_rf_tune[[1]], 0.20)
  expect_true(is.list(double_rf_tune))
  expect_true(is.data.frame(double_rf_tune[[3]]))

})
