setwd("//media//kswada//MyFiles//R//meta")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  meta
# ------------------------------------------------------------------------------
data("meta", package = "gamlss.data")


str(meta)

car::some(meta)



# ------------------------------------------------------------------------------
# Binomial response with normal random effect model
# ------------------------------------------------------------------------------

me2 <- gamlss(cbind(d, n - d) ~ fac + re(fixed = ~fac, random = ~fac | study), family = BI, data = meta)


me3 <- gamlss(cbind(d, n - d) ~ fac + re(fixed = ~fac, random = list(study = pdSymm(~fac))), family = BI, data = meta)


me4 <- gamlss(cbind(d, n - d) ~ fac + re(fixed = ~fac, random = list(study = pdIdent(~fac))), family = BI, data = meta)


me5 <- gamlss(cbind(d, n - d) ~ fac + re(fixed = ~fac, random = list(study = pdDiag(~fac))), family = BI, data = meta)


me6 <- gamlss(cbind(d, n - d) ~ fac + re(fixed = ~fac, random = list(study = pdDiag(~fac))), family = BB, data = meta)



# ----------
GAIC(me1, me2, me3, me4, me5, me6, k = log(length(meta)))

AIC(me1, me2, me3, me4, me5, me6)


# -->
# me5 or me2 is the best



# ------------------------------------------------------------------------------
# Check the model ..
# ------------------------------------------------------------------------------

plot(me2)

plot(me5)


wp(me2)

wp(me5)
