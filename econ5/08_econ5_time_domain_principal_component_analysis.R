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
# Principal component analysis
# ------------------------------------------------------------------------------


m1 <- princomp(gr_s)


names(m1)


summary(m1)




# ----------
par(mfrow = c(1,1))

screeplot(m1)



# -->
# we may select PCs = 1 or 2
# the first 3 components exlain about 86.3% of the total variability of scaled growth rate



# ------------------------------------------------------------------------------
# Compute loading matrix
# ------------------------------------------------------------------------------

# square root of eigenvalues

sdev <- m1$sdev



# number of PCs

pc_n <- 3


SD <- diag(sdev[1:pc_n])


M <- m1$loadings


L <- M[,1:pc_n] %*% SD


print(round(L, 3))



# -->
# 1st factor:  unemp, gnp and prinv
# 2st factor:  govinv
# 3rd factor:  consum




# ------------------------------------------------------------------------------
# Covariance matrix of errors
# ------------------------------------------------------------------------------

LLt <- L %*% t(L)


diag(LLt)


sigE <- 1 - diag(LLt)


summary(sigE)



# -->
# the variances of the noise components are between 0.8% to 37.3% of each standardized variable
# indicating that marked variablity remains in unemployment



# ------------------------------------------------------------------------------
# Estimated commmon factors
# ------------------------------------------------------------------------------


head(m1$scores)


MTSplot(m1$scores[,1:3])





