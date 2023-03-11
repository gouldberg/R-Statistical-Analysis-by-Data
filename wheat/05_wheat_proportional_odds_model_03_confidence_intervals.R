setwd("//media//kswada//MyFiles//R//wheat")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wheat
# ------------------------------------------------------------------------------

wheat <- read.csv(file = "//media//kswada//MyFiles//references//AnalysisOfCategoricalDataWithR//Chapter3//wheat.csv")

str(wheat)

car::some(wheat)



# ------------------------------------------------------------------------------
# Proportional odds model
# ------------------------------------------------------------------------------

# Order properly the type factor
levels(wheat$type)

wheat$type.order <- factor(wheat$type, levels = c("Scab", "Sprout", "Healthy"))

levels(wheat$type.order)



# ----------
library(MASS)

# The method = "logistic" argument value instructs to use the logit transformation on the cumulative probabilities
mod.fit.ord <- polr(formula = type.order ~ class + density + hardness + size + weight + moisture, data = wheat, method = "logistic")
# mod.fit.ord2 <- polr(formula = type.order ~ class + density + hardness + size + weight + moisture, data = wheat)

summary(mod.fit.ord)
# summary(mod.fit.ord2)



# ------------------------------------------------------------------------------
# Confidence intervals for pi_j by default deltaMethod
# ------------------------------------------------------------------------------

x1 <- 0
x2 <- wheat[1,2]
x3 <- wheat[1,3]
x4 <- wheat[1,4]
x5 <- wheat[1,5]
x6 <- wheat[1,6]


# ----------
# object: beta hat
beta.hat <- c(-mod.fit.ord$coefficients, mod.fit.ord$zeta)
names(beta.hat) <- c("b1", "b2", "b3", "b4", "b5", "b6", "b10", "b20")

beta.hat



# ----------
# vcov.
( cov.mat <- vcov(mod.fit.ord) )

numb.int <- length(mod.fit.ord$zeta)
numb.slope <- length(mod.fit.ord$coefficients)

cov.mat[1:numb.slope, (numb.slope + 1):(numb.slope + numb.int)] <- -cov.mat[1:numb.slope, (numb.slope + 1):(numb.slope + numb.int)]
cov.mat[(numb.slope + 1):(numb.slope + numb.int), 1:numb.slope] <- -cov.mat[(numb.slope + 1):(numb.slope + numb.int), 1:numb.slope]

cov.mat


# ----------
# g
g.scab <- "exp(b10 + b1*x1 + b2*x2 + b3*x3 + b4*x4 + b5*x5 + b6*x6)/(1 + exp(b10 + b1*x1 + b2*x2 + b3*x3 + b4*x4 + b5*x5 + b6*x6))"


# Uses deltaMethod.default()
deltaMethod(object = beta.hat, g = g.scab, vcov. = cov.mat)



# ------------------------------------------------------------------------------
# Confidence intervals for pi_j by customeized function
#
# IMPORTANT !!!!
# The polr() function estimates the model:  logit(P(Y <= j)) = beta - beta1 * x1 - beta2 * x2 ...
# NOT: logit(P(Y <= j)) = beta + beta1 * x1 + beta2 * x2 ...

# -->
# As with model fit objects from multinom(), the predict() function does not provide the estimated variances neede to form Wald confidence intervals.
# In addition, the deltaMethod.polr() method function does not allow for any mathematical functions of the intercept terms, and further problems arise
# because polr() estimates a model as described above.
# ------------------------------------------------------------------------------

# Replacement function for deltaMethod.polr()
deltaMethod.polr2 <- function(object, g)  {
  # All beta^'s where the slope estimates are adjusted
  beta.hat <- c(-object$coefficients, object$zeta)
  
  # Count the number of slopes and intercepts
  numb.slope <- length(object$coefficients)
  numb.int <- length(object$zeta)
  
  # Name corresponding parameters
  names(beta.hat) <- c(paste("b", 1:numb.slope, sep=""), paste("b", 1:numb.int, "0", sep=""))
  
  # Fix covariance matrix - All covariances between slopes and intercepts
  #  need to be multiplied by -1
  cov.mat <- vcov(object)
  # Above diagonal
  cov.mat[1:numb.slope, (numb.slope + 1):(numb.slope + numb.int)] <- -cov.mat[1:numb.slope, (numb.slope + 1):(numb.slope + numb.int)]
  # Below diagonal
  cov.mat[(numb.slope + 1):(numb.slope + numb.int), 1:numb.slope] <- -cov.mat[(numb.slope + 1):(numb.slope + numb.int), 1:numb.slope]
  
  # deltaMethod.default() method function completes calculations
  deltaMethod(object = beta.hat, g = g, vcov. = cov.mat)
}

alpha <- 0.05



# -----------
# Parts of character string
scab <- "exp(b10 + b1*x1 + b2*x2 + b3*x3 + b4*x4 + b5*x5 + b6*x6)"
sprout <- "exp(b20 + b1*x1 + b2*x2 + b3*x3 + b4*x4 + b5*x5 + b6*x6)"

# pi^_Scab
g.scab <- paste(scab, "/ (1 + ", scab, ")")
g.scab
calc.scab <- deltaMethod.polr2(object = mod.fit.ord, g = g.scab)
calc.scab$Estimate + qnorm(p = c(alpha/2, 1-alpha/2)) * calc.scab$SE

# pi^_Sprout
g.sprout <- paste(sprout, "/ (1 + ", sprout, ")", " - ", scab, "/ (1 + ", scab, ")")
g.sprout
calc.sprout <- deltaMethod.polr2(object = mod.fit.ord, g = g.sprout)
calc.sprout$Estimate + qnorm(p = c(alpha/2, 1-alpha/2)) * calc.sprout$SE

# pi^_Healthy
g.healthy <- paste("1 - ", sprout, "/ (1 + ", sprout, ")")
g.healthy
# Alternatively, directly enter the string:
g.healthy<-"1 - exp(b20 + b1*x1 + b2*x2 + b3*x3 + b4*x4 + b5*x5 + b6*x6) /
      (1 + exp(b20 + b1*x1 + b2*x2 + b3*x3 + b4*x4 + b5*x5 + b6*x6))"
calc.healthy <- deltaMethod.polr2(object = mod.fit.ord, g = g.healthy)
calc.healthy$Estimate  # pi^_Healthy
calc.healthy$SE  # sqrt(Var^(pi^_Healthy))
calc.healthy$Estimate + qnorm(p = c(alpha/2, 1-alpha/2)) * calc.healthy$SE

