setwd("//media//kswada//MyFiles//R//icu")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Death in the ICU
# ------------------------------------------------------------------------------

data("ICU", package = "vcdExtra")

str(ICU)

dim(ICU)


car::some(ICU)



# ----------
# Here we do not apply "rage" and "coma"
# race, coma is represented initially as 3-level factors, but the recoded to binary variables (white, uncons)
# we apply binary variables

ICU2 <- ICU[, -c(4, 20)]

str(ICU2)



# ----------
dd <- datadist(ICU2);  options(datadist = "dd")

modlrm <- lrm(died ~ age + cancer + admit + uncons, data = ICU2)

modlrm <- update(modlrm, x=TRUE, y=TRUE)



# ------------------------------------------------------------------------------
# Check optimistic level
# ------------------------------------------------------------------------------

# check Optimistic level

( s <- modlrm$stats )

( gamma.hat <- (s["Model L.R."] - s["d.f."]) / s["Model L.R."] )

( 1 - gamma.hat )



# ------------------------------------------------------------------------------
# Check model performance optimism level by Bootstrapping
# ------------------------------------------------------------------------------

B = 200

v <- validate(modlrm, B=B, bw=TRUE, pr=TRUE)

v



# ----------
# check the optimism and corrected for each statistics such as Dxy
v[1:11,]



# ----------
# show first X bootstrap resamples
plot(v)



# ------------------------------------------------------------------------------
# Compute bootstrap estimates of the log odds ratio
# ------------------------------------------------------------------------------

B = 10000

b <- bootcov(modlrm, B=B)

summary(b)



# -----------
# distribution of bootstrap coefficient
hist(b$boot.Coef[,"uncons=Yes"], lty = 1, nclass = 100, xlab = "log(OR)")

sd(b$boot.Coef[,"uncons=Yes"])



# bootstrap confidence interval of coefficient
quantile(b$boot.Coef[,"uncons=Yes"], c(0.025, 0.5, 0.975))

