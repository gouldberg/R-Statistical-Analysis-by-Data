setwd("//media//kswada//MyFiles//R//econ5")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  econ5
# ------------------------------------------------------------------------------

data(econ5, package = "astsa")


str(econ5)


car::some(econ5)




# ------------------------------------------------------------------------------
# Data transformation
# ------------------------------------------------------------------------------

# First logging and then removing the linear trend

U <- resid(lm(log(econ5$unemp) ~ time(log(econ5$unemp))))

G <- resid(lm(log(econ5$gnp) ~ time(log(econ5$gnp))))

C <- resid(lm(log(econ5$consum) ~ time(log(econ5$consum))))


x <- cbind(G, U, C)


MTSplot(x)




# ----------
Ud <- diff(econ5$unemp)

Gd <- diff(econ5$govinv)

Cd <- diff(econ5$consum)



dat <- cbind(Gd, Cd, Ud)



# scaled growth rate
gr_s <- scale(apply(log(econ5), 2, FUN = diff))



# ------------------------------------------------------------------------------
# compute correlation matrix
# ------------------------------------------------------------------------------

polcor <- cor(gr_s)


polcor



# ------------------------------------------------------------------------------
# Factor Rotation:  Orthogonal rotation  (factors are independent from each other)
#
#   - for orthogonal rotation, we impose restriction on rotaion matrix T:  T T' = I
#     This keeps the orthogonal factor structure intact, the loadings are altered, but fit remains unchanged.
#
#   - "varimax" rotation:  most popular orthogonal rotation
# ------------------------------------------------------------------------------


resFA2 <- fa(polcor, nfactors = 3, rotate = "varimax", fm = "ml")




# ------------------------------------------------------------------------------
# Check factor loadings
# ------------------------------------------------------------------------------

print(resFA$loadings, cutoff = 0.2)


print(resFA2$loadings, cutoff = 0.2)



# ----------
# for comparison
plot(prin)




# ------------------------------------------------------------------------------
# Communalities
#   - Portion of variances (sum pf squared loadings) of the manifest variables accounted by the common factor effects
#   - squared multiple correlations explained by the common factors
# ------------------------------------------------------------------------------

# note that communality (fit) remain unchanged

round(resFA$communality, 4)

round(resFA2$communality, 4)


