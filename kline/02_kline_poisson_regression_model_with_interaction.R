setwd("//media//kswada//MyFiles//R//kline")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Kline
# ------------------------------------------------------------------------------
data("Kline", package = "rethinking")

d <- Kline

dim(d)

str(d)


# ----------
# Theory says that it is the order of magnitude of the population that matters, not the absolute size of it.
d$log_pop <- log(d$population)
d$contact_high <- ifelse(d$contact == "high", 1, 0)



# ------------------------------------------------------------------------------
# poisson regression
# ------------------------------------------------------------------------------
# Need to use more strongly regularizing priors on the beta parameters, because the sample is small.
# Here Normal(0,1) priors are probably not conservative enough.

mod1 <- map(
  alist(
    total_tools ~ dpois(lambda),
    log(lambda) <- a + bp * log_pop + bc * contact_high + bpc * contact_high * log_pop,
    a ~ dnorm(0, 100),
    c(bp, bc, bpc) ~ dnorm(0, 1)
  ),
  data = d
)



# ----------
precis(mod1, digits=3, corr=TRUE)
plot(precis(mod1))


# -->
# First notice that the main effect of log-population, bp, is positive and precise, but that both bc and bpc overlap zero substaintially.

# The correlation between bc and bpc is strongly negative, so when bc is small, bpc is large.
# As a result, you can't just inspect the margianl uncertainty in each parameter


pairs(mod1)


# -->
# Remember, in principle the posterior distribution for a GLM may not be multivariate Gaussian, even if all your priors are Gaussian.



# ------------------------------------------------------------------------------
# model investigation
#  - To prove that contact rate is having an important effect on prediction in this model
# ------------------------------------------------------------------------------

# Consider two islands:
#  - both with log-population of 8
#  - but one with high contac and the other with low

post <- extract.samples(mod1)

# inverting the link means exponentiating with exp.
lambda_high <- exp(post$a + post$bc + (post$bp + post$bpc) * 8)
lambda_low <- exp(post$a + post$bp * 8)


# ----------
diff <- lambda_high - lambda_low
sum(diff > 0) / length(diff)

dens(diff)


# -->
# That's a 95% plausibility that the high-contact island has more tools than the low-contact islands.



