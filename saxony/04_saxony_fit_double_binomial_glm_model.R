setwd("//media//kswada//MyFiles//R//saxony")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Saxnony
#  - The nmber of male children in families of size 12 in Saxony
# ------------------------------------------------------------------------------
data("Saxony", package = "vcd")

data <- Saxony



# ------------------------------------------------------------------------------
# create data frame
# ------------------------------------------------------------------------------
Males <- as.numeric(names(data))
Families <- as.vector(data)
data_df <- data.frame(Males, Families)



# ------------------------------------------------------------------------------
# Fit binomial (12, p) as glm
# ------------------------------------------------------------------------------
# using log(n, k) = lchoose(12, 0:12) as an offset
mod_bin <- glm(Families ~ Males, offset = lchoose(12, 0:12), family = poisson, data = data_df)



# ----------
# brief model summaries
LRstats(mod_bin)

coef(mod_bin)

exp(coef(mod_bin)["Males"]) / ( 1 + exp(coef(mod_bin)["Males"]) )


# -->
# beta1 = coef(mod_bin)["Males"] is actually estimating the logit of p = log( p / (1-p) )
# so inverse transformation gives phat = exp(beta1) / ( 1 + exp(beta1) )



# ------------------------------------------------------------------------------
# Fit double binomial model (12, p, psi)
#
# The double binomial distribution allows a separate parameter for overdispersion relative to the binomial
# ------------------------------------------------------------------------------
# The term YlogitY calculates k * log(k) + (n - k) * log(n - k), the second sufficient statistic for the double binomial fitted via glm()
data_df$YlogitY <- Males * log(ifelse(Males == 0, 1, Males)) + (12 - Males) * log(ifelse(12 - Males == 0, 1, 12 - Males))

mod_dbin <- glm(Families ~ Males + YlogitY, offset = lchoose(12, 0:12), family = poisson, data = data_df)



# ----------
coef(mod_dbin)



# ------------------------------------------------------------------------------
# Compare glm models
# ------------------------------------------------------------------------------
LRstats(mod_bin, mod_dbin)


# -->
# We can see the double binomial model with one more parameter is significantly bettern than the simple binomial and represents an adequate fit to the data.



# ------------------------------------------------------------------------------
# Compare residuals
# ------------------------------------------------------------------------------
results <- data.frame(data_df, fit.bin = fitted(mod_bin), res.bin = rstandard(mod_bin), fit.dbin = fitted(mod_dbin), res.dbin = rstandard(mod_dbin))
print(results, digits = 2)



# ------------------------------------------------------------------------------
# Show fitting in rootogram
# ------------------------------------------------------------------------------
with(results, vcd::rootogram(Families, fit.dbin, Males, xlab = "Number of males"))



