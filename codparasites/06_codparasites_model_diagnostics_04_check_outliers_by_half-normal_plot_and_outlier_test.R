setwd("//media//kswada//MyFiles//R//codparasites")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Cod Parasites
# ------------------------------------------------------------------------------

data("CodParasites", package = "countreg")


dim(CodParasites)

str(CodParasites)


car::some(CodParasites)



# ----------
CodParasites$prevalence <- ifelse(CodParasites$intensity == 0, "no", "yes")



# ----------
modp <- glm(intensity ~ length + area * year, data = CodParasites, family = poisson(link = "log"))



# ----------
library(MASS)
( nbin <- glm.nb(intensity ~ length + area * year, data = CodParasites) )
( theta <- nbin$theta )
mod.nbin <- glm(intensity ~ length + area * year, CodParasites, family = negative.binomial(theta))

# modp <- mod.nbin



# ------------------------------------------------------------------------------
# Normal Quantile-quantile Plot:  Studentized residuals vs. Normal Quantiles
# ------------------------------------------------------------------------------

car::qqPlot(rstudent(modp), xlab = "Normal Quantiles", ylab = "Studentized residuals")


# -->
# There are many outliers outside the envelope:  especially 1254 and 1085



# ----------
# by group
car::qqPlot(rstudent(modp), group = CodParasites$area, xlab = "Normal Quantiles", ylab = "Studentized residuals")




# -->
# For GLMs with discrete responses, such plots are often disappointing, even with a reasonably good-fitting model, because
# (a) possible outlierrs can appear at both the lower and upper ends of the distribution of residuals
# (b) the theoretical normal distribution used to derive the envelope may not be well approximated in a given model




# ------------------------------------------------------------------------------
# Half-normal Plot with simulated confidence envelopes
#   - Atkinson (1981, 1987) suggested a more robust and usefule version of QQ plots
# ------------------------------------------------------------------------------

observed <- sort(abs(rstudent(modp)))

n <- length(observed)

expected <- qnorm((1:n + n - 1/8) / (2 * n + 1/2))



# ----------
S <- 100

sims <- simulate(modp, nsim = S)

simdat <- cbind(na.omit(CodParasites), sims)



# ----------
# calculate residuals for one simulated data set

resids <- function(y){

  # Poisson model
  rstudent(glm(y ~ length + area * year, data = simdat, start = coef(modp)))

  # Negative Binomial Model
  # rstudent(glm.nb(y ~ length + area * year, data = simdat, start = coef(modp)))
}


simres <- matrix(0, nrow(simdat), S)


for(i in 1:S){
  simres[,i] <- sort(abs(resids(simdat[,paste("sim", i, sep = "_")])))
}



# ----------
envelope <- 0.95

mean <- apply(abs(simres), 1, mean)

lower <- apply(abs(simres), 1, quantile, prob = (1 - envelope) / 2)

upper <- apply(abs(simres), 1, quantile, prob = (1 + envelope) / 2)



# ----------
par(mfrow = c(1,1))

plot(expected, observed, xlab = "Expected value of half-normal order statistic", ylab = "Absolute value of studentized residuals")

lines(expected, mean, lty = 1, lwd = 2, col = "blue")

lines(expected, lower, lty = 2, lwd = 2, col = "red")

lines(expected, upper, lty = 2, lwd = 2, col = "red")


identify(expected, observed, labels = names(observed), n = 3)




# ------------------------------------------------------------------------------
# Half-normal Plot by faraway::halfnorm
# ------------------------------------------------------------------------------

faraway::halfnorm(rstudent(modp))



# ------------------------------------------------------------------------------
# car::OutlierTest()
#  - give a formal test of significance of the largest absolute studentized residuals, witha Bonferroni-adjusted p-value accounting
#    for choosing the largest values among n such tests.
# ------------------------------------------------------------------------------

car::outlierTest(modp)




# ------------------------------------------------------------------------------
# Examine distribution of residuals
# ------------------------------------------------------------------------------

res <- rstudent(modp)



# ----------
plot(density(res), lwd = 2, col = "blue")

rug(res)



# ----------
# Why the bimodality ??
plot(jitter(log(na.omit(CodParasites)$intensity + 1), factor = 1.5), res, xlab = "log(intensity + 1)", ylab = "Studentized residual")



# -->
# The lower mode corresponds to intensity == 0  --> excess zero counts !!
