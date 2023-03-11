setwd("//media//kswada//MyFiles//R//eustockmarkets")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  EuStockMarkets
# ------------------------------------------------------------------------------
data("EuStockMarkets", package = "datasets")


str(EuStockMarkets)

head(EuStockMarkets)



# ----------
dax <- EuStockMarkets[, "DAX"]

Rdax <- diff(log(dax))




# ------------------------------------------------------------------------------
# fitDist() can be applied only to fitting distributions when no explanatory variables are in the model.
# The function chooseDist() can be used for any GAMLSS model and provides results for more than one penalty k, simultaneously.
# ------------------------------------------------------------------------------

# Normal distribution model is fitted first to start the process

m1 <- gamlssML(Rdax, family = NO)


t1 <- chooseDist(m1, type = "realline", trace = TRUE)


t1



# -->
# In this case the distributions fitted are all the gamlss.family distributions on the real line, i.e. with range (-Inf, Inf)
# The different GAICs are determined by the penalty argument k, with default values 2, 3.84, and log(n), i.e. AIC, X^2(1, 0.05), and BIC/SBC, respectively



# ----------
# ordering of a specified column according to the relevant GAIC

getOrder(t1, 1)[1:10]

getOrder(t1, 2)[1:10]

getOrder(t1, 3)[1:10]




# ----------
# plot the best GT model

fh <- histDist(Rdax, family = "GT", nbins = 30, line.wd = 2.5)




# ----------
# No final fitted model is provided by chooseDist(), unlike fitDist().
# The final model can be refitted using the update() function

mf <- update(m1, family = "GT")

summary(mf)




# -->
# But note that since the parameters of the GT distribution were fitted using default link function (identity, log, log, log, respectively).
# Also t tests here are asymptotic Wald tests.
# While t tests are useful in normal distribution regression models (with constant variance sigma) when by testing the significance of a linear coefficient
# in the mean model one checkes whether the explanatory variable affects the parameter mu (the mean) of the response,
# their usefulness is dubious in the current situation.
# In general the generalized likelihood ratio test provides a more reliable test for parameter values.



# ----------
# The value of the fitted parameters (after link function is applied)
# can also be obtained

fitted(mf, "mu")[1]

fitted(mf, "sigma")[1]

fitted(mf, "nu")[1]

fitted(mf, "tau")[1]


