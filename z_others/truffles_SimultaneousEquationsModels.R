# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2", "AER", "foreign", "car", "lmtest", "stargazer", "broom", "knitr", "plm", "systemfit")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  truffles
#
# q:  quantity of truffles traded
# p:  market price
# ps:  the price of a substitute
# di:  income
# pf:  measure of costs of production
#
# Simultaneous Equations Models:  quantity and price are endogenous and all the other variables are considered exogenous
# ------------------------------------------------------------------------------
data("truffles", packages = "POE5Rdata")
data <- truffles
dim(data)
str(data)
describe(data)


# Quantity and Price
par(mfrow=c(1,1))
plot(data$p, data$q)


# necessary condition for identification requires that for the problem to have a solution
# each equation in the structural form of the system should miss at least an exogenous variable that is present in other equations
D <- q ~ p + ps + di
S <- q ~ p + pf
sys <- list(D, S)
instr <- ~ ps + di + pf

truff.sys <- systemfit(sys, inst = instr, method = "2SLS", data = data)
summary(truff.sys)


# Estimating the reduced forms, giving only equilibrium point
# Showing that all the exogenous variables have significant effects on the equilibrium quantity and price and have the expected signs
Q.red <- lm(q ~ ps + di + pf, data = data)
P.red <- lm(p ~ ps + di + pf, data = data)

kable(tidy(Q.red), digits = 4, caption = "Reduced Form for Quantity of Truffles")
kable(tidy(P.red), digits = 4, caption = "Reduced Form for Price of Truffles")

