setwd("//media//kswada//MyFiles//R//mammalsleep")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  mammalsleep
# ------------------------------------------------------------------------------

data("mammalsleep", package = "faraway")


str(mammalsleep)


mammalsleep$pdr <- with(mammalsleep, dream / sleep)


head(mammalsleep)



# ----------
# mod_obj <- qlmod
mod_obj <- qlmod2b



# ------------------------------------------------------------------------------
# Studentized (deletion) residuals by index  (with Standardized Pearson Residuals)
# ------------------------------------------------------------------------------

# studentized Residuals
stud.resid <- rstudent(mod_obj)


# standardized Pearson residuals
stand.resid <- rstandard(mod_obj, type = "pearson")



# ----------
par(mfrow=c(2,1))
plot(stud.resid, ylim = c(min(-3, stud.resid), max(3, stud.resid)), main = "Studentized residuals", type = "l")
abline(h = c(3,2,0,-2,-3), lty = "dotted", col = c("red", "blue", "gray", "blue", "red"))

plot(stand.resid, ylim = c(min(-3, stand.resid), max(3, stand.resid)), main = "Standardized Pearson residuals", type = "l")
abline(h = c(3,2,0,-2,-3), lty = "dotted", col = c("red", "blue", "gray", "blue", "red"))




# ------------------------------------------------------------------------------
# Studentized Residuals
#  - id.n argument here works to label that number of the most extreme observations individually for each measure plotted.
# ------------------------------------------------------------------------------

car::influenceIndexPlot(mod_obj, vars = c("studentized"), id.n = 4)



# ------------------------------------------------------------------------------
# Half-normal Plot by faraway::halfnorm
# ------------------------------------------------------------------------------

par(mfrow=c(1,1))

faraway::halfnorm(rstudent(mod_obj))


mammalsleep[c(11,2),]



# ----------
qlmod <- glm(pdr ~ log(body) + log(lifespan) + danger, family = quasibinomial, data = mammalsleep)

mod_obj <- qlmod



# ------------------------------------------------------------------------------
# car::OutlierTest()
#  - give a formal test of significance of the largest absolute studentized residuals, witha Bonferroni-adjusted p-value accounting
#    for choosing the largest values among n such tests.
# ------------------------------------------------------------------------------

car::outlierTest(mod_obj)




# ------------------------------------------------------------------------------
# Examine distribution of residuals
# ------------------------------------------------------------------------------

res <- rstudent(mod_obj)


# ----------
plot(density(res), lwd = 2, col = "blue")

rug(res)



# ----------
# Why the bimodality ??  --> here no modality
plot(jitter(mammalsleep$pdr, factor = 1.5), res, xlab = "mammalsleep", ylab = "Studentized residual")




# ------------------------------------------------------------------------------
# Half-normal Plot with simulated confidence envelopes
#   - Atkinson (1981, 1987) suggested a more robust and usefule version of QQ plots
# ------------------------------------------------------------------------------

###############################################################
# This section does NOT apply to quasibinomial model !!!
###############################################################

observed <- sort(abs(rstudent(mod_obj)))

n <- length(observed)

expected <- qnorm((1:n + n - 1/8) / (2 * n + 1/2))



# ----------
S <- 100

sims <- simulate(mod_obj, nsim = S)

simdat <- cbind(mammalsleep, sims)



# ----------
# calculate residuals for one simulated data set

resids <- function(y){
  rstudent(glm(y ~ poly(log(body),3) + log(lifespan) + danger, data = simdat, start = coef(mod_obj)))
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


