# An R6 class for a simple lookup table in R

The title tells it all. Create lookup tables in R with this class. 

The class also saves the gradients of the function, which can improve accuracy for some functions, but you should be advised that using gradients can in some cases cause worse performance than not using them. Run the file `Test/test_suite.R`, look at the ggplot plots and see where failure happens.

The output values of the function are stored as a list so this means that you can have all sorts of lookup tables. Also lookup tables of other lookup tables, which can be useful e.g. computationally expensive likelihood functions, see the file `Test/lut_of_luts.R` for a simple yet inspiring example. 

## Example

```
source("lookUpTable_v_0.01.R")

lut = LUT$new(
  min = c(-10, -10),
  max = c(10, 10),
  n_steps = c(10, 10),
  f = function(arg) return(arg[1] + arg[2]^2),
  calculate_gradients = TRUE
)

# Evaluates the original function 
lut$f(c(0, 2))
# Gets the closest value in the lookup table
lut$value(c(0, 2))
# Attempts to correct the value using gradient information
lut$value_g(c(0, 2))

```



