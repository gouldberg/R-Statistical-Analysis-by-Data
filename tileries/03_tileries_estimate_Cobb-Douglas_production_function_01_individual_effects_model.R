setwd("//media//kswada//MyFiles//R//tileries")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Tileries
# ------------------------------------------------------------------------------

data("Tileries", package = "pder")


str(Tileries)


dim(Tileries)


car::some(Tileries)



# ----------
Tl.p <- pdata.frame(Tileries)




# ------------------------------------------------------------------------------
# Within model  (Oneway (individual) effect Within Model)
# ------------------------------------------------------------------------------


plm.within <- plm(log(output) ~ log(labor) + log(machine), Tl.p)


summary(plm.within)




# ------------------------------------------------------------------------------
# Within model  (Oneway (individual) effect Within Model) by applying OLS on the within transformed variables or using individual dummy variables
# ------------------------------------------------------------------------------

y <- log(Tl.p$output)


x1 <- log(Tl.p$labor)


x2 <- log(Tl.p$machine)


lm.within <- lm(I(y - Between(y)) ~ I(x1 - Between(x1)) + I(x2 - Between(x2)) - 1)


lm.lsdv <- lm(log(output) ~ log(labor) + log(machine) + factor(id), Tl.p)



# ----------
summary(lm.within)


summary(lm.lsdv)



# ----------
car::residualPlot(lm.within)

car::residualPlots(lm.lsdv)



# ----------
car::compareCoefs(lm.within, plm.within)

coef(lm.within)

coef(plm.within)



# -->
# Coefficients are identical.



