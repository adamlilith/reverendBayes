dZIP <- nimbleFunction(
	run = function(
	x = integer(),
	theta = double(0),
	zeroProb = double(),
	log = logical(0, default = 0)
) {

	returnType(double())

	## First handle non-zero data
	if (x != 0) {

		## return the log probability if log = TRUE
		if (log) return(dpois(x, theta, log = TRUE) + log(1 - zeroProb))

		## or the probability if log = FALSE
		else return((1 - zeroProb) * dpois(x, theta, log = FALSE))
		
	}

	## From here down we know x is 0
	totalProbZero <- zeroProb + (1 - zeroProb) * dpois(0, theta, log = FALSE)
	if (log) return(log(totalProbZero))
	return(totalProbZero)

})
