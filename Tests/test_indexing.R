# This defines a 3-dimensional look up table. Where
# on every dimension there are only integer steps.
#
# The function that is used for calculating the 
# outputs actually just changes the input into a
# a string, i.e. if the values 1 2 3 are given 
# to the function, it will return the value "1 2 3".
# 
# These return values are compared against a matrix
# of the true values in the matrix x. If all comparisons
# return 1 the test passess.

setwd("~/Desktop/LookUpTable_R/Repo_w_deriv/")
source("lookUpTable_v_0.01.R")

lut = LUT$new(
  min = c(1, 1, 1),
  max = c(3, 6, 7),
  n_steps = c(3, 6, 7),
  function(arg) return(toString(arg)),
  calculate_gradients = FALSE
)

x = expand.grid(1:3, 1:6, 1:7)
res = rep(NaN, nrow(x))

for(i in 1:nrow(x)){
  res[i] = toString(as.numeric(x[i,])) == lut$value(as.numeric(x[i,]))
}

if(!any(res == 0)){
  cat("Test passed\n")
} else {
  cat("Test failed\n")
}
