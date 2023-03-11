setwd("//media//kswada//MyFiles//R//prostate")

packages <- c("dplyr", "Hmisc", "rms", "lattice", "ggplot2")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  prostate
# ------------------------------------------------------------------------------

getHdata(prostate)

str(prostate)

car::some(prostate)


# ----------
# convert an old data format to R format
prostate$sdate <- as.Date(prostate$sdate)
data <- prostate
str(data)


data$cvd <- data$status %in% c("dead - heart or vascular", "dead - cerebrovascular")



# ------------------------------------------------------------------------------
# conversion
# ------------------------------------------------------------------------------

# combining an infrequent category with the next category, and dichotomizing ekg

levels(data$ekg)[levels(data$ekg) %in% c("old MI", "recent MI")] <- "MI"

data$ekg.norm <- 1 * (data$ekg %in% c("normal", "benign"))

levels(data$ekg) <- abbreviate(levels(data$ekg))

data$pfn <- as.numeric(data$pf)

levels(data$pf) <- levels(data$pf)[c(1,2,3,3)]

data$rxn <- as.numeric(data$rx)



# ------------------------------------------------------------------------------
# Cox regression based on principal components from raw data
# ------------------------------------------------------------------------------

library(survival)


# Create a survival object
S <- with(data, Surv(dtime, status != "alive"))



# ----------
# Use PCs scores to explain the times until death using Cox regression.
pcs <- prin.raw$scores

aic <- numeric(16)


for(i in 1:16){
  ps <- pcs[,1:i]
  aic[i] <- AIC(cph(S ~ ps))
}



# ----------
# Full variable model of imputed data
f <- cph(S ~ sz + sg + log(ap) + sbp + dbp + age + wt + hg + ekg + pf + bm + hx, data = imp)


f2 <- cph(S ~ rcs(sz, 5) + rcs(sg, 5) + rcs(log(ap), 5) + rcs(sbp, 5) + rcs(dbp, 5) + rcs(age, 3) + rcs(wt, 5) + rcs(hg, 5) + ekg + pf + bm + hx, data = imp, tol = 1e-14)



# ----------
plot(1:16, aic, xlab = "Number of Components Used", type = "l", ylim = c(3940, 4000))

abline(h = AIC(f), col = "blue")
abline(h = AIC(f2), col = "blue", lty = 2)



# -->
# The first 5 components adequately summarizes all variables, if linearly transformed,
# and the full linear model is NO BETTER than this.

# The model allowing all continuouts predictors to be nonlinear is not worth its added degree of freedom.



# ------------------------------------------------------------------------------
# Cox regression based on clustered principal components from transformed data
# ------------------------------------------------------------------------------

# Function to extract 1st PC from clustered variables
pco <- function(v){
  f <- princomp(data_trans[,v], cor = TRUE)
  vars <- f$sdev^2
  cat("Fraction of variance expalained by PC1:", round(vars[1] / sum(vars), 2), '\n')
  f$scores[,1]
}



# ----------
tumor <- pco(c("sz", "sg", "ap", "bm"))

bp <- pco(c("sbp", "dbp"))

cardiac <- pco(c("hx", "ekg"))

other <- data_trans[,c("hg", "age", "pf", "wt")]



# ----------
var_all <- data.frame(tumor = tumor, bp = bp, cardiac = cardiac)
var_all <- cbind(var_all, other, S)



# ----------
f3 <- cph(S ~ tumor + bp + cardiac + hg + age + pf + wt + hg, data = var_all)

abline(h = AIC(f3), col = "red", lty = 1, lwd = 2)



# -->
# The principal components by clustered variable (transformed data) are good contributor to model
