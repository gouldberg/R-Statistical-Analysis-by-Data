setwd("//media//kswada//MyFiles//R//ricefarms")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  RiceFarms
# ------------------------------------------------------------------------------

data("RiceFarms", package = "splm")


str(RiceFarms)


dim(RiceFarms)



# ------------------------------------------------------------------------------
# Mundlak approach for test for correlated effects
#   - The regular Hausman test is based on homoskedasticity assumptions and can yield negative test statistics.
#     These inconveniences can be overcome by the Mundlak version of the test, which is based on a regression model of the form:
#     y(it) = delta1 + beta2 * x2(it) + alpha1 * omega(1i) + gamma2 * mean(x2(i)) + ( c(i) + e(it) )
#     where mean(x2(i)) is the time average of the independent variable
# ------------------------------------------------------------------------------

Rice <- pdata.frame(RiceFarms, index = "id")


# ----------
pdim(Rice)


index(Rice)



# ----------
rice.eq <- log(goutput) ~ log(seed) + log(totlabor) + log(size)



# ----------
rice.w <- plm(rice.eq, data = Rice, model = "within")


rice.b <- update(rice.w, model = "between")



# ----------
cp <- intersect(names(coef(rice.b)), names(coef(rice.w)))

dcoef <- coef(rice.w)[cp] - coef(rice.b)[cp]

V <- vcov(rice.w)[cp, cp] + vcov(rice.b)[cp, cp]


# Chi-square test statistics
( test.stats <- as.numeric(t(dcoef) %*% solve(V) %*% dcoef) )


# p-values
1 - pchisq(test.stats, df = 3)


# -->
# almost close to Hausman test.
# the null hypothesis of no correlation is NOT rejected



# ----------
# correlation coefficient between the individual effects (estiamted by the fixed effects of the within model)
# and the individual means of the explanatory variables obtained by applying the between function to the series

cor(fixef(rice.w), between(log(Rice$goutput)))



# -->
# The correlation is positive but moderate


