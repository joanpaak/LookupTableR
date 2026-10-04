# Two-dimensional normal distribution (rho = 0).
# NOTE: The values of the grid, no surprise there, have to 
# be adjusted to match the range of the other dimension
# where mu = 2. Try that with a mismatch and you'll see that
# the error gets out of hand pretty quickly.

setwd("~/Desktop/LookUpTable_R/Repo_w_deriv/")
source("lookUpTable_v_0.01.R")

f = function(arg) return(dnorm(arg[1]) * dnorm(arg[2], 2))

lut = LUT$new(
  min = c(-2, 0),
  max = c(2, 4),
  n_steps = c(40, 40),
  f
)

n_sim = 100
true = rep(NaN, n_sim)
est  = rep(NaN, n_sim)
x = matrix(NaN, ncol = 2, nrow = n_sim)

for(i in 1:n_sim){
  x[i,] = runif(2, c(-2, 0), c(2, 4))
  est[i] = lut$value(x[i,])
  true[i] = f(x[i,])
}

plot(true, est); abline(0, 1, col = "red")

