test_that("Test that input checker works as intended", {

  n = 2000; p = 10
  X_test = matrix(rnorm(n*p), n, p)
  W_test = rbinom(n, 1, 0.4 + 0.2 * (X_test[,1] > 0))
  Y_test = pmax(X_test[,1], 0) * W_test + X_test[,2] + pmin(X_test[,3], 0) + rnorm(n)

  X_bad <- rep("notanumber",nrow(X_test))

  X_awful <- cbind(X_test,X_bad)


  expect_message( input_checker(X_test, Y_test, W_test,
                                test.inputs = T, specify.custom = NULL),
                  "Input is correct")
  expect_warning( input_checker(X_test, Y_test, W_test,
                                test.inputs = F, specify.custom = "something"),
                  "Your inputs will not be checked for compliance with your method,
      please check your types are correct manually!")
  expect_warning( input_checker(X_test, Y_test, W_test,
                                test.inputs = F, specify.custom = "something"),
                  "Your inputs will not be checked for compliance with your method,
      please check your types are correct manually!")

  expect_warning( input_checker(X_test, Y_test, W_test,
                                test.inputs = F, specify.custom = NULL),
                  "Your inputs will not be checked for compliance with your method,
      please check your types are correct manually!")


  expect_error( input_checker(X_awful, Y_test, W_test,
                              test.inputs = T, specify.custom = NULL),
                "Your covariates are not all numeric. Disable input checking if this is not a problem."
                )
  expect_error( input_checker(data.frame(X_awful), Y_test, W_test,
                              test.inputs = T, specify.custom = NULL),
                "Your X is not a matrix. Disable input checking if this is not a problem.")



  })
