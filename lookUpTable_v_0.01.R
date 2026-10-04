#
# Look-up table, v 0.01
# TODO: Add structure to initializer
# TODO: Linear interpolation style thing via an option to approximate
#       gradients based on the evaluated grid points.

LUT <- 
  R6::R6Class("LUT", 
              public = list(
                n = NULL,
                g = NULL,
                x = NULL,
                val = NULL,
                min = NULL,
                max = NULL,
                n_dim = NULL,
                n_steps = NULL,
                # The implementation of these methods depends on if 1) 
                # gradients are saved and 2) how many dimensional the 
                # function is so they are created during initialization.
                value_g = NULL,
                get_index = NULL,
                
                # INPUT
                #   min : vector of minimum values 
                #   max : vector of maximum values
                #   n_steps : vector of steps
                #   f : function used for calculating output
                initialize = function(min, max, n_steps, f, calculate_gradients = TRUE){
                  self$n_dim  <- length(n_steps)
                  self$n    <- prod(n_steps)
                  self$x    <- matrix(NaN, ncol = self$n_dim, nrow = self$n)
                  self$val  <- vector(mode = "list", length = self$n)
                  self$max  <- max
                  self$min  <- min
                  self$n_steps <- n_steps
                  
                  if(calculate_gradients){
                    self$g <- matrix(NaN, ncol = self$n_dim, nrow = self$n) # Gradient
                    
                    self$value_g <- function(x){
                      index = self$get_index(self$val_to_coord(x))
                      uncorrected = self$val[[index]]
                      difference = x - self$x[index,]
                      corrected = uncorrected + sum(difference * self$g[index,])
                      
                      return(corrected)
                    }
                  }
                
                  if(self$n_dim == 1){
                    self$get_index = function(coordinate){
                      return(coordinate[1])
                    }
                  } else {
                    # Transforms n-dimensional coordinate into index of 
                    # the look-up table (in long format)
                    # INPUT:
                    # coordinate : a vector of coordinate values
                    self$get_index <- function(coordinate){
                      # TODO: WHAT THE FUCK!!!!
                      ind = coordinate[1] - 1 + 1
                      
                      for(i in 2:self$n_dim){
                        ind = ind + (coordinate[i] - 1) * prod(self$n_steps[1:(i - 1)])
                      }
                      
                      return(ind)
                    }
                  }
                  
                  #### This part initializes the search grid ####
                  
                  skip_factor = rep(1, self$n_dim)
                  
                  if(self$n_dim > 1){
                    for(i in 2:self$n_dim){
                      skip_factor[i] = skip_factor[i - 1] * n_steps[i - 1]
                    }
                  }
                  
                  counter = rep(1, self$n_dim)
                  arg = rep(NaN, length(n_steps))
                  
                  for(i in 1:self$n){
                    for(j in 1:self$n_dim){
                      arg[j] = min[j] + (max[j] - min[j]) / (n_steps[j] - 1) * 
                        (counter[j] - 1)
                    }
                    
                    self$x[i,] = arg
                    self$val[[i]] = f(arg)
                    
                    if(calculate_gradients){
                      self$g[i,] = numDeriv::grad(f, arg)
                    }
                    
                    # Increment counter vector
                    counter[1] = counter[1] + 1
                    
                    if(self$n_dim > 1){
                      for(j in 2:self$n_dim){
                        if(i %% skip_factor[j] == 0){
                          counter[j] = counter[j] + 1 
                        }
                      }
                    }
                    
                    for(j in 1:self$n_dim){
                      if(counter[j] > n_steps[j]){
                        counter[j] = 1
                      }
                    }
                  }
                },
                
                # Transforms raw input into a n-dimensional coordinate
                # INPUT
                # val : a vector of input values
                val_to_coord = function(val){
                  coordinate = rep(NaN, self$n_dim)
                  # (x - min_x) / ((max_x - min_x) / (n_steps_x - 1)) + 1
                  for(i in 1:self$n_dim){
                    coordinate[i] = round((val[i] - self$min[i]) / 
                                            ((self$max[i] - self$min[i]) /  (self$n_steps[i] - 1)) + 1)
                    
                    if(coordinate[i] < 1) coordinate[i] = 1
                    if(coordinate[i] > self$n_steps[i]) coordinate[i] = self$n_steps[i]
                    
                  }
                  
                  return(coordinate)
                },
                
                # Returns the value of the discretized function for the 
                # input x
                # INPUT:
                # x : n-dimensional vector of input values
                value = function(x){
                  index = self$get_index(self$val_to_coord(x))
                  uncorrected = self$val[[index]]
                  
                  return(uncorrected)
                }
              )
  )


