setwd("//media//kswada//MyFiles//R//mroz")

packages <- c("dplyr", "lmtest", "stargazer", "broom", "knitr", "POE5Rdata")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  mroz
# ------------------------------------------------------------------------------

data(mroz, package = "POE5Rdata")


dim(mroz)

str(mroz)


# ----------
# restrict to wome in the labour force
mroz_lfp1 <- subset(mroz, lfp == 1)



# ------------------------------------------------------------------------------
# Cragg-Donald F-test for Weak instruments
#   - the test for weak instruments might be unreliable with more than one endogenous regressor,
#     because there is indeed one F-statistic for each endogenous regressor
# ------------------------------------------------------------------------------

# Let us look at the hours equation with 2 endogenous variabls, mtr and educ, and 2 external instruments, mothereduc and fathereduc.
# One of 2 exogenous regressors nwifeinc is the family income net of the wife's income
# The other exogenous regressor, mtr is the wife's marginal tax rate

data_1 <- subset(mroz_lfp1, wage > 0)

nwifeinc <- (data_1$faminc - data_1$wage * data_1$hours) / 1000

L <- 2  # number of external instruments
N <- nrow(data_1)  # number of observations

x1 <- resid(lm(mtr ~ kidsl6 + nwifeinc, data = data_1))

x2 <- resid(lm(educ ~ kidsl6 + nwifeinc, data = data_1))

z1 <- resid(lm(mothereduc ~ kidsl6 + nwifeinc, data = data_1))

z2 <- resid(lm(fathereduc ~ kidsl6 + nwifeinc, data = data_1))

X <- cbind(x1, x2)

Y <- cbind(z1, z2)

rB <- min(cancor(X, Y)$cor)  # lowest canonical correlation, a measure of the correlation between the endogenous and the exogenous variables
( CraggDonaldF <- ((N - L) / L) * (rB^2 / ( 1- rB^2)) )



# -->
# Cragg-Donald F = 0.1013 is smaller than the critical value of 4.58 
# The test does NOT reject the H0 of weak instruments, contradicting the previous results

