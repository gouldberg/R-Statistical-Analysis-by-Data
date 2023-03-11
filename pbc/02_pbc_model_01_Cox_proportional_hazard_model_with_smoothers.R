setwd("//media//kswada//MyFiles//R//pbc")

packages <- c("dplyr", "gamair", "mgcv")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  pbc
# ------------------------------------------------------------------------------

data(pbc, package = "survival")
data(pbcseq, package = "survival")


str(pbc)
str(pbcseq)


car::some(pbc)
car::some(pbcseq)



# ------------------------------------------------------------------------------
# Cox.ph family initial model and dropping insignificant terms sequentially
# ------------------------------------------------------------------------------

pbc$status1 <- as.numeric(pbc$status == 2)


pbc$stage <- factor(pbc$stage)



# ----------
# protime and ast is square rooted to reduce their skew.

b0 <- gam(time ~ trt + sex + stage + s(sqrt(protime)) + s(platelet) +
            s(age) + s(bili) + s(albumin) + s(sqrt(ast)) + s(alk.phos),
          weights = status1, family = cox.ph, data = pbc)


summary(b0)



# -->
# alk.phos effect is estimated to be almost flat with wide confidence intervals, and a high p-value



# ----------
# then drop alk.phos

b1 <- gam(time ~ trt + sex + stage + s(sqrt(protime)) + s(platelet) +
            s(age) + s(bili) + s(albumin) + s(sqrt(ast)),
          weights = status1, family = cox.ph, data = pbc)


summary(b1)


# -->
# ast effect then continues to look marginal and has a p-value of 0.16


# ----------
# then drop ast and stage

b2 <- gam(time ~ trt + sex + s(sqrt(protime)) + s(platelet) +
            s(age) + s(bili) + s(albumin),
          weights = status1, family = cox.ph, data = pbc)


summary(b2)



# ----------
anova(b2)


# -->
# This provides no evidence for a treatment effect.



# ----------
graphics.off()
par(mfrow = c(2,3))
plot(b2)

# residual plot of the deviance residuals against the "risk scores" (simply the linear predictor values)
plot(b2$linear.predictors, residuals(b2))



# -->
# The wedge shaped block of residuals at the lower left of the residual plot is caused by the censored observations
# and is typical of such plots.

# The couple of high points at the top left are individuals who died despite relatively low risk scores,
# and may be slight outliers here.
# Otherwise the plot appears reasonable.

# There seems to be a low upward drift in hazard with age, while the other effects are in line with expectations.

# Hazard increases with blood clotting time and reduced platelet count.
# There is an initial sharp increase with serum bilirubin which then levels off, consistent with a poorly functioning liver failing to clear bilirubin,
# while hazard decreases with serum albumin, presumably as ability to produce albumin increases with liver function.





