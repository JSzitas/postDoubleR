n = 2000; p = 10
X = matrix(rnorm(n*p), n, p)
W = rbinom(n, 1, 0.4 + 0.2 * (X[,1] > 0))
Y = pmax(X[,1], 0) * W + X[,2] + pmin(X[,3], 0) + rnorm(n)


test_that("Post double selection works with glmnet in parallel", {

  skip_on_cran()
  skip_on_travis()

  double_lasso <- double_ML(X, Y, W, method = c("glmnet"),
                                k.fld = 4, simulations = 50,
                                lambda.set.Y = 1,
                                lambda.set.W = 1,
                                show.progress = FALSE,
                                Z.trans = TRUE,
                                parallelize = TRUE )




  expect_lte(double_lasso[[1]], 0.55)
  expect_gte(double_lasso[[1]], 0.35)
  expect_true(is.list(double_lasso))
})


test_that("Post double selection works with glmnet in parallel(with specified core.n", {

  skip_on_cran()
  skip_on_travis()

  double_lasso <- double_ML(X, Y, W, method = c("glmnet"),
                                k.fld = 4, simulations = 50,
                                lambda.set.Y = 1,
                                lambda.set.W = 1,
                                show.progress = FALSE,
                                Z.trans = TRUE,
                                parallelize = TRUE,
                                cores.to.use = 4)




  expect_lte(double_lasso[[1]], 0.55)
  expect_gte(double_lasso[[1]], 0.35)
  expect_true(is.list(double_lasso))
})

test_that("Post double selection works with glmnet in parallel(with specified core.n", {

  skip_on_cran()
  skip_on_travis()

  double_lasso <- double_ML(X, Y, W, method = c("glmnet"),
                                k.fld = 4, simulations = 10,
                                cv.steps = 10,
                                show.progress = FALSE,
                                Z.trans = TRUE,
                                parallelize = TRUE,
                                cores.to.use = 4)




  expect_lte(double_lasso[[1]], 0.55)
  expect_gte(double_lasso[[1]], 0.35)
  expect_true(is.list(double_lasso))
})

