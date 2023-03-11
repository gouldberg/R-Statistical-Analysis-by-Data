
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\armd")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

# https://rdrr.io/cran/nlmeU/src/inst/scriptsR2.15.0/Ch12.R


# ------------------------------------------------------------------------------
# data:  ARMD (Age-Related Macular Degeneration Trial)
# ------------------------------------------------------------------------------


armd.wide <- read.csv("armd.wide.txt", header = T, sep = "\t")


armd0 <- read.csv("armd0.txt", header = T, sep = "\t")



str(armd.wide)


str(armd0)



# ----------
data("armd", package = "nlmeU")


str(armd)




# ------------------------------------------------------------------------------
# Heteroscedastic Autoregressive Residuals Errors by corAR1
# ------------------------------------------------------------------------------


# There are 8 patietns with a nonmonotone missing data patter.
# We need to use a position variable "tp"
# 1 for week 4, 2 for week 12, 3 for week 24, and 4 for week 52 measurements

fm12.2 <- update(fm9.2, correlation = corAR1(form = ~ tp | subject), data = armd)


summary(fm12.2)



# -->
# Phi = 0.66, higher than 0.57 obtained for the compound-symmetry structure.



# ----------
# 95% CIs for the variance-covariance parameters

intervals(fm12.2, which = "var-cov")




# ----------
# the marginal variance-covariance structure
# request the subject "2" for whom all four post-randomization measurements are available.

fm12.2vcov <- getVarCov(fm12.2, individual = "2")


nms <- c("4wks", "12wks", "24wks", "52wks")


dnms <- list(nms, nms)


dimnames(fm12.2vcov) <- dnms



print(fm12.2vcov)


print(cov2cor(fm12.2vcov), corr = TRUE, stdevs = FALSE)



# -->
# on the other hand, the measurements separated by, e.g., one intermittent observation
# (those at weeks 4 and 24), are correlated with the orrelation coefficient equal to 0.43,
# which is lower than 0.57 (= compound-symmetry case)







# ----------
# Compound-symmetry vs. autoregressive correlation (nonnested models)
# AIC:  fm12.2 is better thatn fm12.1

anova(fm12.1, fm12.2)


