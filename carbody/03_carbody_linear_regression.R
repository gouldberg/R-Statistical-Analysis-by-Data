
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\carbody")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  car body coating process data
# ------------------------------------------------------------------------------

dat <- read.csv("carbody_dat.txt", header = TRUE, skip = 2, sep = "\t")


str(dat)


car::some(dat)




# ----------
dat_s <- data.frame(scale(dat))


head(dat_s)




# ------------------------------------------------------------------------------
# simple linear regression
# ------------------------------------------------------------------------------


lmod0 <- lm(tochaku ~ kishaku + nendo + gunspeed + fukituke + airpres + patternwid + hakidashi + 
              toryotemp + temp + moisture, data = dat_s)


summary(lmod0)




# ----------
lmod1 <- lm(tochaku ~ fukituke + patternwid + toryotemp, data = dat_s)



summary(lmod1)



# -->
# note that the coefficient of "fukituke" is -0.59



car::residualPlots(lmod1)



par(mfrow = c(2,2))

plot(lmod1)





# ------------------------------------------------------------------------------
# variable selection by step
# ------------------------------------------------------------------------------


lmod_step <- step(lmod0)



summary(lmod_step)




# -->
# still almost same model with lmod1
# now the coefficient of "fukituke" is -0.64





# ------------------------------------------------------------------------------
# Estimate "fukituke" effect to "tochaku"
# ------------------------------------------------------------------------------


lmod2 <- lm(tochaku ~ fukituke + temp, data = dat_s)



summary(lmod2)



# -->
# the coefficient of "fukituke" is -0.535





# ----------
lmod3 <- lm(tochaku ~ fukituke + toryotemp, data = dat_s)


summary(lmod3)



lmod4 <- lm(tochaku ~ fukituke + toryotemp + temp, data = dat_s)


summary(lmod4)



# -->
# the coefficient of "fukituke" is -0.5019
# --> this is almost close to total effect of fukituke = -0.503
# estimated by linear structral model (non-recursive path model)



lmod5 <- lm(tochaku ~ fukituke + hakidashi, data = dat_s)


summary(lmod5)



lmod6 <- lm(tochaku ~ fukituke + toryotemp + patternwid, data = dat_s)


summary(lmod6)
