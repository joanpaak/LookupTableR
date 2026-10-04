# Lookup table of lookup tables. In this example we create a lookup table
# of Gamma distributions. We can search the "gamma_lut" lookup table with the
# parameters (shape and rate) to get a lookup table of a Gamma distribution
# with the specified values for its parameters. 

setwd("~/Desktop/LookUpTable_R/Repo_w_deriv/")
source("lookUpTable_v_0.01.R")

gamma_lut = LUT$new(
  min = c(1, 1),
  max = c(3, 3),
  n_steps = c(10, 10),
  f = function(arg){
    lut = LUT$new(
      min = 0.1,
      max = 10,
      n_steps = 100,
      f = function(x) return(dgamma(x[1], arg[1], arg[2]))
    )
    
    return(lut)
  },
  calculate_gradients = FALSE
)

n_sim = 1000
true_value = rep(NaN, n_sim)
est_value = rep(NaN, n_sim)
x = cbind(runif(n_sim, 1, 3), runif(n_sim, 1, 3), runif(n_sim, 0, 10))

for(i in 1:n_sim){
  true_value[i] = dgamma(x[i,3], x[i,1], x[i,2])
  est_value[i] = gamma_lut$value(x[i,c(1,2)])$value_g(x[i,3])
}

plot(true_value, est_value); abline(0, 1, col = "red")

