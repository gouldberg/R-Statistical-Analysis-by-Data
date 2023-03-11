# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("//media//kswada//MyFiles//R//mroz")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2", "AER", "foreign", "car", "lmtest", "stargazer", "broom", "knitr", "POE5Rdata")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

library(censReg)



# ------------------------------------------------------------------------------
# data:  mroz  (from wooldridge !!!)
# ------------------------------------------------------------------------------
mroz <- read.dta("http://fmwww.bc.edu//ec-p//data//wooldridge//mroz.dta")

str(mroz)

names(mroz)

oursample <- subset(mroz, !is.na(wage))



# ------------------------------------------------------------------------------
# Corner Solution Responses  - The Tobit Model (Censored Regression)
# Estimate labor supply of married, working women by accounting the fact that the hours worked is necessarily non-negative
#   - Censored data include a large number of observations for which the dependent variable takes one, or a limited number of values
#     A censored model is based on the idea of a latent, or unobserved variable that is not censored, and is explained via a probit model
# ------------------------------------------------------------------------------


hist(mroz$hours, breaks = 20, col = "grey", main = "", xlab = "Hours Worked")
box(byd = "o")



# ----------
TobitRes <- censReg(hours ~ nwifeinc + educ + exper + I(exper^2) + age + kidslt6 + kidsge6, data = mroz)

summary(TobitRes)



# ----------
# Partial Effects at the average x
margEff(TobitRes)



# ------------------------------------------------------------------------------
# Corner Solution Responses  - The Tobit Model (Censored Regression)
# Another alternative for estimating Tobit models is survreg() from package survival
# ------------------------------------------------------------------------------
library(survival)

res <- survreg(Surv(hours, hours > 0, type = "left") ~ nwifeinc + educ + exper + I(exper^2) + age + kidslt6 + kidsge6, data = mroz, dist = "gaussian")

summary(res)



# ------------------------------------------------------------------------------
# Corner Solution Responses  - The Tobit Model (Censored Regression)
# AER::tobit()
# ------------------------------------------------------------------------------

TobitRes2 <- tobit(hours ~ nwifeinc + educ + exper + I(exper^2) + age + kidslt6 + kidsge6, data = mroz)

summary(TobitRes2)



#----------
# marginal effect should be calculated manually

summary(mroz$nwifeinc)
summary(mroz$educ)
summary(mroz$exper)
summary(mroz$exper^2)
summary(mroz$age)
summary(mroz$kidslt6)
summary(mroz$kidsge6)

xnwifeinc <- 20.129
xEduc <- 12.29
xExper <- 10.63
xExper2 <- 178
xAge <- 42.54
xKidslt6 <- 0.2377
xKidsge6 <- 1.353

bInt <- coef(TobitRes2)[[1]]
bnwifeinc <- coef(TobitRes2)[[2]]
bEduc <- coef(TobitRes2)[[3]]
bExper <- coef(TobitRes2)[[4]]
bExper2 <- coef(TobitRes2)[[5]]
bAge <- coef(TobitRes2)[[6]]
bKidslt6 <- coef(TobitRes2)[[7]]
bKidsge6 <- coef(TobitRes2)[[8]]

betas <- c(bInt, bnwifeinc, bEduc, bExper, bExper2, bAge, bKidslt6, bKidsge6)

bSigma <- TobitRes2$scale

Phactor <- pnorm( (bInt + bnwifeinc * xnwifeinc + bEduc * xEduc + bExper * xExper + bExper2 * xExper2 + bAge * xAge + bKidslt6 * xKidslt6 + bKidsge6 * xKidsge6) / bSigma )



betas * Phactor


# for comparison:  the same value
margEff(TobitRes)


