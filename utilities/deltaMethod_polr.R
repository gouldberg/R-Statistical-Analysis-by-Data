# ------------------------------------------------------------------------------
# deltaMeghod.polr2
#
# calculate pi-hat and Var(pi-hat) for proportional odds model
# ------------------------------------------------------------------------------

deltaMethod.polr2 <- function(object, g){
  require(car)
  
  # All beta-hat's where the slope estimates are adjusted
  beta.hat <- c(-object$coefficients, object$zeta)
  
  # Count the number of slopes and intercepts
  numb.slope <- length(object$coefficients)
  numb.int <- length(object$zeta)
  
  # Name the corresponding parameters
  names(beta.hat) <- c(paste("b", 1:numb.slope, sep=""), paste("b", 1:numb.int, "0", sep=""))
  
  # Fix covariance matrix - All covariances between slopes and intercepts need to be multiplied by -1
  cov.mat <- vcov(object)
  
  # Above diagonal
  cov.mat[1:numb.slope, (numb.slope + 1):(numb.slope + numb.int)] <- -cov.mat[1:numb.slope, (numb.slope + 1):(numb.slope + numb.int)]
  
  # Below diagonal
  cov.mat[(numb.slope + 1):(numb.slope + numb.int), 1:numb.slope] <- -cov.mat[(numb.slope + 1):(numb.slope + numb.int), 1:numb.slope]

  # deltaMethod.default() method function completes calculations
  deltaMethod(object = beta.hat, g = g, vcov. = cov.mat)
    
  }


