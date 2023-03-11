setwd("//media//kswada//MyFiles//R//coal_miners")

packages <- c("dplyr", "vcd", "MASS", "datasets")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Coal Miners
# ------------------------------------------------------------------------------

data("CoalMiners", package = "vcd")

data <- CoalMiners



# ----------
# Breathlessness * Wheeze * Age
dim(data)

dimnames(data)


data


# ----------
# Age 20-24 to be omitted from analysis here
dat2 <- data[,,2:9]


structable(. ~ Age, data = dat2)




# ------------------------------------------------------------------------------
# To test for nonlinearily in the prevalence of the symptoms or their odds ratio with age
# we fit using poly() or ns()
# ------------------------------------------------------------------------------

cm.vglm2 <- vglm(cbind(bw, bW, Bw, BW) ~ poly(agec, 2), binom2.or(zero = NULL), data = coalminers)


cm.vglm3 <- vglm(cbind(bw, bW, Bw, BW) ~ ns(agec, 2), binom2.or(zero = NULL), data = coalminers)



summary(cm.vglm2)


summary(cm.vglm3)



# -->
# poly() model has a residual G^2 = 16.963 with 18 df.




# ----------
( LR <- deviance(cm.vglm1) - deviance(cm.vglm2))


1 - pchisq(LR, cm.vglm1@df.residual - cm.vglm2@df.residual)



# -->
# this represents a significant improvement in goodness of fit.



