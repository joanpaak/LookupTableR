# Functions 1-20 were suggested by GPT-5.6 Luna
#
# 0. One-dimensional function
# f(x) = x^2
#
# 1. Constant
# f(x, y) = 0
#
# 2. Single-input dependence
# f(x, y) = x
#
# 3. Linear combination
# f(x, y) = 2x - 3y + 5
#
# 4. Multiplicative interaction
# f(x, y) = xy
#
# 5. Polynomial with multiple terms
# f(x, y) = x^2 + 3xy - 2y^2 + x - y
#
# 6. Smooth nonlinear function
# f(x, y) = sin(x) cos(y)
#
# 7. Exponential
# f(x, y) = exp(x - y)
#
# 8. Gaussian peak
# f(x, y) = exp(-(x² + y²))
#
# 9. Distance from a point
# f(x, y) = sqrt((x - 1)² + (y + 2)²)
#
# 10. Absolute value
# f(x, y) = |x - y|
#  
# 11. Piecewise function
# f(x, y) =
#  x + y,  if x >= 0
# x - y,  if x < 0
#
# 12. Discontinuous function
# f(x, y) =
#  0, if x + y < 1
# 1, otherwise
#
# 13. Rational function
# f(x, y) = 1 / (1 + x² + y²)
#
# 14. Function with a singularity
# f(x, y) = 1 / (x - y)
#
# 15. Symmetric function
# f(x, y) = x^2 + y^2 (Should satisfy f(x, y) = f(y, x))
# 
# 16. Antisymmetric function
# f(x, y) = x - y (Should satisfy f(x, y) = -f(y, x))
#
# 17. Periodic function
# f(x, y) = sin(2πx) + cos(2πy)
#
# 18. Boolean or threshold function
# f(x, y) = (x > y)
#
# 19. Three-input interaction
# f(x, y, z) = xyz + x + 2y - z
#
# 20. Higher-order interaction
# f(x, y, z) = sin(xy) + exp(-z²)
#
# Problematic cases:
#
# Test 7
# Test 8
# Test 9
# Test 10
# Test 14
# Test 17
# Test 18 - Boolean function, here the gradient correction fails completely


setwd("~/Desktop/LookUpTable_R/Repo_w_deriv/")

source("lookUpTable_v_0.01.R")

test_random_points = function(min, max, n_steps, f, n_sim){
  lut = LUT$new(
    min = min, 
    max = max, 
    n_steps = n_steps, 
    f = f
  )
  
  x = matrix(NaN, ncol = length(min), nrow = n_sim)
  res = matrix(NaN, ncol = 3, nrow = n_sim)
  colnames(res) = c("True", "Raw", "Gradient")
  
  for(i in 1:n_sim){
    x[i,] = runif(length(min), min, max)
    res[i,1] = f(x[i,])
    res[i,2] = lut$value(x[i,])
    res[i,3] = lut$value_g(x[i,])
  }
  
  return(list(
    res = data.frame(
      f = rep(c("True", "LUT", "LUT/Grad"), each = n_sim),
      x = c(res[,1], res[,2], res[,3])),
    error = data.frame(
      Method = rep(c("LUT", "LUT/Grad"), each = n_sim),
      Error = c(res[,1] - res[,2], res[,1] - res[,3])
    ), x = x))
}

test_0 = test_random_points(
  c(-2),
  c(2),
  c(15),
  function(x) return(x^2),
  1000
)
ggplot2::ggplot(test_0$error, 
                ggplot2::aes(
                  x = Method, y = Error)) + 
  ggplot2::geom_violin(scale = "width")


test_1 = test_random_points(
  c(-2, -2), 
  c(2, 2), 
  c(10, 10), 
  function(x) return(0), 
  1000)
ggplot2::ggplot(test_1$error, 
                ggplot2::aes(
                  x = Method, y = Error)) + 
  ggplot2::geom_violin(scale = "width")

test_2 = test_random_points(
  c(-2, -2), 
  c(2, 2), 
  c(10, 10), 
  function(x) return(x[1]), 
  1000)
ggplot2::ggplot(test_2$error, 
                ggplot2::aes(
                  x = Method, y = Error)) + 
  ggplot2::geom_violin(scale = "width")

test_3 = test_random_points(
  c(-2, -2), 
  c(2, 2), 
  c(10, 10), 
  function(x) return(2 * x[1] - 3 * x[2] + 5), 
  1000)
ggplot2::ggplot(test_3$error, 
                ggplot2::aes(
                  x = Method, y = Error)) + 
  ggplot2::geom_violin(scale = "width")

test_4 = test_random_points(
  c(-2, -2), 
  c(2, 2), 
  c(10, 10), 
  function(x) return(x[1] * x[2]), 
  1000)
ggplot2::ggplot(test_4$error, 
                ggplot2::aes(
                  x = Method, y = Error)) + 
  ggplot2::geom_violin(scale = "width")

test_5 = test_random_points(
  c(-2, -2), 
  c(2, 2), 
  c(10, 10), 
  function(x) return(x[1]^2 + 3 * prod(x) - 2 * x[2]^2 + x[1] - x[2]), 
  1000)
ggplot2::ggplot(test_5$error, 
                ggplot2::aes(
                  x = Method, y = Error)) + 
  ggplot2::geom_violin(scale = "width")

test_6 = test_random_points(
  c(-2, -2), 
  c(2, 2), 
  c(10, 10), 
  function(x) sin(x[1]) * cos(x[2]), 
  1000)
ggplot2::ggplot(test_6$error, 
                ggplot2::aes(x = Method, y = Error)) + 
  ggplot2::geom_violin(scale = "width")

test_7 = test_random_points(
  c(-4, -4), 
  c(4, 4), 
  c(10, 10), 
  function(x) exp(x[1] - x[2]), 
  1000)
ggplot2::ggplot(test_7$error, 
                ggplot2::aes(x = Method, y = Error)) + 
  ggplot2::geom_violin(scale = "width")

test_8 = test_random_points(
  c(-2, -2), 
  c(2, 2), 
  c(10, 10), 
  function(x) exp(x[1]^2 - x[2]^2), 
  1000)
ggplot2::ggplot(test_8$error, 
                ggplot2::aes(x = Method, y = Error)) + 
  ggplot2::geom_violin(scale = "width")

test_9 = test_random_points(
  c(-2, -2), 
  c(2, 2), 
  c(10, 10), 
  function(x) sqrt((x[1] - 1)^2 + (x[2] - 1)^2), 
  1000)
ggplot2::ggplot(test_9$error, 
                ggplot2::aes(x = Method, y = Error)) + 
  ggplot2::geom_violin(scale = "width")


test_10 = test_random_points(
  c(-2, -2), 
  c(2, 2), 
  c(10, 10), 
  function(x) abs(x[1] - x[2]), 1000)
ggplot2::ggplot(test_10$error, 
                ggplot2::aes(
                  x = Method, y = Error)) + 
  ggplot2::geom_violin(scale = "width")

test_11 = test_random_points(
  c(-2, -2), 
  c(2, 2), 
  c(10, 10), 
  function(x) {
    if(x[1] >= 0) return(x[1] + x[2])
    return(x[1] - x[2])
  }, 
  1000)
ggplot2::ggplot(test_11$error, 
                ggplot2::aes(x = Method, y = Error)) + 
  ggplot2::geom_violin(scale = "width")

test_12 = test_random_points(
  c(-2, -2), 
  c(2, 2), 
  c(10, 10), 
  function(x) {
    if(sum(x) < 1) return(0)
    return(1)
  }, 
  1000)
ggplot2::ggplot(test_12$error, 
                ggplot2::aes(x = Method, y = Error)) + 
  ggplot2::geom_violin(scale = "width")

test_13 = test_random_points(
  c(-2, -2), 
  c(2, 2), 
  c(10, 10), 
  function(x) 1.0 / (1 + x[1]^2 + x[2]^2), 
  1000)
ggplot2::ggplot(test_13$error, 
                ggplot2::aes(x = Method, y = Error)) + 
  ggplot2::geom_violin(scale = "width")


test_14 = test_random_points(
  c(-2, -2), 
  c(2, 2), 
  c(10, 10), 
  function(x) 1.0 / (x[1] - x[2]), 
  1000)
ggplot2::ggplot(test_14$error, 
                ggplot2::aes(x = Method, y = Error)) + 
  ggplot2::geom_violin(scale = "width")


test_15 = test_random_points(
  c(-2, -2), 
  c(2, 2), 
  c(10, 10), 
  function(x) x[1]^2 - x[2]^2, 
  1000)
ggplot2::ggplot(test_15$error, 
                ggplot2::aes(x = Method, y = Error)) + 
  ggplot2::geom_violin(scale = "width")

test_16 = test_random_points(
  c(-2, -2), 
  c(2, 2), 
  c(10, 10), 
  function(x) x[1] - x[2], 
  1000)
ggplot2::ggplot(test_16$error, 
                ggplot2::aes(x = Method, y = Error)) + 
  ggplot2::geom_violin(scale = "width")


test_17 = test_random_points(
  c(-2, -2), 
  c(2, 2), 
  c(10, 10), 
  function(x) sin(2 * pi * x[1]) + cos(2 * pi * x[2]), 
  1000)
ggplot2::ggplot(test_17$error, 
                ggplot2::aes(x = Method, y = Error)) + 
  ggplot2::geom_violin(scale = "width")

test_18 = test_random_points(
  c(-2, -2), 
  c(2, 2), 
  c(10, 10), 
  function(x) x[1] > x[2], 
  1000)
ggplot2::ggplot(test_18$error, 
                ggplot2::aes(x = Method, y = Error)) + 
  ggplot2::geom_violin(scale = "width")

test_19 = test_random_points(
  c(-2, -2, -2), 
  c(2, 2, 2), 
  c(10, 10, 10), 
  function(x) prod(x) + x[1] + 2 * x[2] -x[3], 
  1000)
ggplot2::ggplot(test_19$error, 
                ggplot2::aes(x = Method, y = Error)) + 
  ggplot2::geom_violin(scale = "width")

test_20 = test_random_points(
  c(-2, -2, -2), 
  c(2, 2, 2), 
  c(10, 10, 10), 
  function(x) sin(x[1] * x[2]) + exp(-x[3]^2), 
  1000)
ggplot2::ggplot(test_20$error, 
                ggplot2::aes(x = Method, y = Error)) + 
  ggplot2::geom_violin(scale = "width")

