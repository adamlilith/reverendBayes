
library(nimble)

dwpois <- nimbleFunction(
	run = function(
		x = double(0),			# declaring data types for each argument
		lambda = double(0),
		weight = double(0),
		N = double(0),
		log = integer(0)) {
		
		returnType(double(0))	# declaring data type for output
		
		# likelihood
		out <- log(weight) + log(dpois(x = x, lambda = lambda, log = FALSE))
		return(out)
	}
)


rwpois <- nimbleFunction(
	run = function(
		n = integer(0),
		lambda = double(0),
		weight = double(0),
		N = double(0)) {
  		
		returnType(double(0))
		return(rpois(N, lambda = lambda * weight))

})


n <- 1000 # number of points to simulate
beta0 <- 2.5 # intercept
beta1 <- 1.8 # slope
beta2 <- -3.3 # quadratic

x <- runif(n, -1, 1) # independent variable, NB we approximatley center the variable!
y <- rpois(n, exp(beta0 + beta1 * x + beta2 * x^2)) # dependent variable
weights <- runif(n, 0, 20) * (y / (max(y) - min(y)))^2

# setup for models
data <- list(y = y)

constants <- list(
	x = x,
	n = n,
	weights = weights
)

inits <- list(
	beta0_hat = 0,
	beta1_hat = 0,
	beta2_hat = 0
)

# weighted model
code_weighted <- nimbleCode({

	# priors
	beta0_hat ~ dnorm(0, sd = 10)
	beta1_hat ~ dnorm(0, sd = 10)
	beta2_hat ~ dnorm(0, sd = 10)

	# likelihood... NB: note the use of "weights" with dpois()
	for (i in 1:n) {
		log(lambda[i]) <- beta0_hat + beta1_hat * x[i] + beta2_hat * x[i]^2
		y[i] ~ dwpois(lambda[i], weights[i])
	}

})

model_weighted <- nimbleModel(
	code = code_weighted, # our model
	constants = constants, # constants
	data = data, # data
	inits = inits, # initialization values
	check = TRUE, # any errors?
	calculate = FALSE
)
