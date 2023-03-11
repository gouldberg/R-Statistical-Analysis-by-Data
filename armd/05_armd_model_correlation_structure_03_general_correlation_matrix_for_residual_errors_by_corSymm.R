
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
# General correlation structure by corSymm
# ------------------------------------------------------------------------------

# corSymm() specifies general (unconstrained) correlations between the visual acuity measurements

fm12.3 <- update(fm12.2, correlation = corSymm(form = ~ tp | subject), data = armd)


summary(fm12.3)




# ----------
# 95% CIs for the variance-covariance parameters

intervals(fm12.3, which = "var-cov")




# ----------
# the marginal variance-covariance structure
# request the subject "2" for whom all four post-randomization measurements are available.

fm12.3vcov <- getVarCov(fm12.3, individual = "2")


nms <- c("4wks", "12wks", "24wks", "52wks")


dnms <- list(nms, nms)


dimnames(fm12.3vcov) <- dnms



print(fm12.3vcov)


print(cov2cor(fm12.3vcov), corr = TRUE, stdevs = FALSE)

print(cov2cor(fm12.1vcov), corr = TRUE, stdevs = FALSE)

print(cov2cor(fm12.2vcov), corr = TRUE, stdevs = FALSE)



# -->
# different correlation structure compared to corCompSymm and corAR1




# ----------
# Autoregressive of order 1 vs. general correlation structure
# better for general correlation structure

anova(fm12.2, fm12.3)


