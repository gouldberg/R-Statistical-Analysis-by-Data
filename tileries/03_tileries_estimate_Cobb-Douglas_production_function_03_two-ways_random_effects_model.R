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
# Two-ways random effects model
#
#   - y(nt) + alpha + beta * x(nt) + eta(n) + mu(t) + v(nt)
# ------------------------------------------------------------------------------

plm.within2 <- plm(log(output) ~ log(labor) + log(machine), Tl.p, effect = "twoways")


summary(plm.within2)




# ------------------------------------------------------------------------------
# Two-way random effects model by applying OLS on the within transformed variables or using individual dummy variables
# ------------------------------------------------------------------------------


y <- log(Tl.p$output)

y <- y - Between(y, "individual") - Between(y, "time") + mean(y)
  

x1 <- log(Tl.p$labor)

x1 <- x1 - Between(x1, "individual") - Between(x1, "time") + mean(x1)


x2 <- log(Tl.p$machine)

x2 <- x2 - Between(x2, "individual") - Between(x2, "time") + mean(x2)



lm.within2 <- lm(y ~ x1 + x2 - 1)


lm.lsdv2 <- lm(log(output) ~ log(labor) + log(machine) + factor(id) + factor(week), Tl.p)




# ----------
summary(lm.within2)


summary(lm.lsdv2)



# ----------
car::residualPlot(lm.within2)

car::residualPlots(lm.lsdv2)



# ----------
car::compareCoefs(lm.within2, plm.within2)

coef(lm.within2)

coef(plm.within2)



# -->
# Coefficients are not same ...



