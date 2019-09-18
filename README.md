
<!-- README.md is generated from README.Rmd. Please edit that file -->

# postDoubleR

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![CRAN
status](https://www.r-pkg.org/badges/version/postDoubleR)](https://CRAN.R-project.org/package=postDoubleR)
<!-- badges: end -->

**postDoubleR** implements post-double selection, also known as double
machine learning/debiasing, due Chernozukov et al.(2017a)
<https://doi.org/10.1111/ectj.12097>.

## Installation

You can install the stable version of postDoubleR from
[CRAN](https://CRAN.R-project.org) with

``` r
install.packages("postDoubleR")
```

The latest version is also available from
[github](https://github.com/JSzitas/postDoubleR) using the *devtools*
package

``` r
devtools::install_github("JSzitas/postDoubleR")
```

## Introduction

The goal of this package is to facilitate the estimation of treatment
effects through double machine learning. To that end, several backends
have been implemented in the package, including random forests (through
package **grf**), lasso (through **glmnet**) and a custom method for
declaring any methods to use.

## Example

Here we use 5 fold crossfitting, using 100 simulations and lasso
(through glmnet) to predict the treatment effects.

``` r
library(postDoubleR)

# random data generating process, shamelessly taken from the examples on the grf github page,
# due Tibshirani, Wager, Athey
 n = 2000
 p = 10
 X = matrix(rnorm(n*p), n, p)
 W = rbinom(n, 1, 0.4 + 0.2 * (X[,1] > 0))
 Y = pmax(X[,1], 0) * W + X[,2] + pmin(X[,3], 0) + rnorm(n)


double_ML(X = X,
          Y = Y,
          W = W,
          method = "glmnet",
          lambda.set.Y = 1,
          lambda.set.W = 1,
          k.fld = 5,
          simulations = 100,
          validate.inputs = TRUE,
          seed.use = 1071)

  
 
```

Here is a simple way to extend the package functionality with custom
methods, using the **custom\_helper** function. (glm is an example as
this requires no addtional packages.)

``` r
# first specify an expression to evaluate for both W and Y
 W_mod <- expression( glm( W~.,
                            family = "binomial",
                            data = as.data.frame(cbind(X,W))))

  Y_mod <- expression( glm( Y~.,
                            family = "gaussian",
                            data = as.data.frame(cbind(X,Y))))
 # then just pass these as custom_helper arguments 
double_ML(X = X,
          Y = Y,
          W = W,
          method = "custom",
          specify.own = custom_helper( X = X,
                                       Y = Y,
                                       W = W,
                                       Y.hat.model = Y_mod,
                                       W.hat.model = W_mod),
          k.fld = 5,
          simulations = 100,
          validate.inputs = TRUE,
          seed.use = 1071)
```

Note that you can compose more complicated functions, and they should
work if they are loaded in your environment.

``` r
  composed_Y <- function(X,Y){
    X <- as.data.frame(X)
    return(glm( Y~.,
                family = "gaussian",
                data = as.data.frame(cbind(X,Y))))
  }

  composed_W <- function(X,Y){
    X <- as.data.frame(X)
    return(glm( Y~.,
                family = "gaussian",
                data = as.data.frame(cbind(X,Y))))
  }

  W_mod <- expression( composed_W(X,W) )

  Y_mod <- expression( composed_Y(X,Y) )

  
double_ML(X = X,
          Y = Y,
          W = W,
          method = "custom",
          specify.own = custom_helper( X = X,
                                       Y = Y,
                                       W = W,
                                       Y.hat.model = Y_mod,
                                       W.hat.model = W_mod),
          k.fld = 5,
          simulations = 100,
          validate.inputs = TRUE,
          seed.use = 1071)
  
```

## Contributing

If you would like to contribute a pull request, please do contribute\!
All contributions will be considered for acceptance, provided they are
justifiable and the code is reasonable, regardless of anything related
to the person submitting the pull request. Please keep things civil -
there is no need for negativity. Also, please do refrain from adding
unnecessary dependencies (*Ex:* pipe) to the package (such pull requests
as would add an unnecessary dependency will be denied/ suspended until
the code can be made dependency free).
