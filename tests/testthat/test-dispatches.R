n = 2000; p = 10
X = matrix(rnorm(n*p), n, p)
W = rbinom(n, 1, 0.4 + 0.2 * (X[,1] > 0))
Y = pmax(X[,1], 0) * W + X[,2] + pmin(X[,3], 0) + rnorm(n)

test_that("Glmnet dispatch works", {

  glmnet_non_parall <- glmnet_helper( X,
                                      Y,
                                      W,
                                      Z.trans = TRUE,
                                      cv.steps = 100 )

   glmnet_lambda_W <- glmnet_helper( X,
                                     Y,
                                     W,
                                     lambda.set.W = 1)

   glmnet_lambda_Y <- glmnet_helper( X,
                                     Y,
                                     W,
                                     lambda.set.Y = 1)

   glmnet_lambda_YW <- glmnet_helper( X,
                                      Y,
                                      W,
                                      Z.trans = TRUE,
                                      lambda.set.Y = 1,
                                      lambda.set.W = 1)

   glmnet_parall <- glmnet_helper( X,
                                   Y,
                                   W,
                                   Z.trans = TRUE,
                                   cv.steps = 100,
                                   parallelize = TRUE,
                                   cores.to.use = 1)


   expect_equal(class(glmnet_non_parall[[1]]), "cv.glmnet")
   expect_equal(class(glmnet_non_parall[[2]]), "cv.glmnet")

   expect_equal(class(glmnet_lambda_W[[1]]), c("elnet", "glmnet"))
   expect_equal(class(glmnet_lambda_W[[2]]), "cv.glmnet")

   expect_equal(class(glmnet_lambda_Y[[1]]), "cv.glmnet")
   expect_equal(class(glmnet_lambda_Y[[2]]), c("elnet", "glmnet"))

   expect_equal(class(glmnet_lambda_YW[[1]]), c("elnet", "glmnet"))
   expect_equal(class(glmnet_lambda_YW[[2]]), c("elnet", "glmnet"))

   expect_equal(class(glmnet_parall[[1]]), "cv.glmnet")
   expect_equal(class(glmnet_parall[[2]]), "cv.glmnet")

})



test_that("Ols dispatch works",{
    ols_disp <- ols_helper(X,Y,W)

    expect_equal(class(ols_disp[[1]]), "lm")
    expect_equal(class(ols_disp[[2]]), "lm")

})


test_that("RF dispatch works",{

  rf_disp_base <- rf_helper(X,Y,W,
                       orthog.boost = FALSE,
                       tree.n = 500,
                       tune = FALSE )

  rf_disp_ort <- rf_helper(X,Y,W,
                            orthog.boost = FALSE,
                            tree.n = 500,
                            tune = FALSE )

  rf_disp_base_tune <- rf_helper(X,Y,W,
                            orthog.boost = FALSE,
                            tree.n = 500,
                            tune = TRUE )

  rf_disp_ort_tune <- rf_helper(X,Y,W,
                            orthog.boost = TRUE,
                            tree.n = 500,
                            tune = TRUE )

  expect_equal(class(rf_disp_base[[1]]), c("regression_forest", "grf"))
  expect_equal(class(rf_disp_base[[2]]), c("regression_forest", "grf"))

  expect_equal(class(rf_disp_ort[[1]]), c("regression_forest", "grf"))
  expect_equal(class(rf_disp_ort[[2]]), c("regression_forest", "grf"))

  expect_equal(class(rf_disp_base_tune[[1]]), c("regression_forest", "grf"))
  expect_equal(class(rf_disp_base_tune[[2]]), c("regression_forest", "grf"))

  expect_equal(class(rf_disp_ort_tune[[1]]), "boosted_regression_forest")
  expect_equal(class(rf_disp_ort_tune[[2]]), "boosted_regression_forest")


})


test_that("Custom dispatch works",{

glm(Y~., data = as.data.frame(cbind(X,Y)))
glm(W~., data = as.data.frame(cbind(X,W)))

   custom_disp <- custom_helper( X,Y,W,
                                 Y.hat.model =
                                    glm(Y~., data = as.data.frame(cbind(X,Y)))
                                                             ,
                                 W.hat.model =
                                    glm(W~., data = as.data.frame(cbind(X,W)))
                                                              )

   expect_equal(class(custom_disp[[1]]), c("glm", "lm") )
   expect_equal(class(custom_disp[[2]]), c("glm", "lm"))

})

