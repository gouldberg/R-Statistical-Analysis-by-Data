
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
# Compound-symmetry correlation structure
# ------------------------------------------------------------------------------

lm1.form <- formula(visual ~ -1 + visual0 + time.f + treat.f : time.f)



# same correlation coefficient for different observations for each level of the subject factor.
# that is, we allow for a constant correlation of visual acuity measurements made at different timepoints for the same patient.

# default method = "REML" is used.

fm12.1 <- gls(lm1.form, weights = varPower(form = ~time), 
              correlation = corCompSymm(form = ~1 | subject),
              data = armd)



summary(fm12.1)




# ----------
# 95% CIs for the variance-covariance parameters

intervals(fm12.1, which = "var-cov")




# ----------
# the marginal variance-covariance structure
# request the subject "2" for whom all four post-randomization measurements are available.

fm12.1vcov <- getVarCov(fm12.1, individual = "2")


nms <- c("4wks", "12wks", "24wks", "52wks")


dnms <- list(nms, nms)


dimnames(fm12.1vcov) <- dnms



print(fm12.1vcov)


print(cov2cor(fm12.1vcov), corr = TRUE, stdevs = FALSE)





# ----------
# test of independence vs. compound-symmetry correlation structure
# --> SIGNIFICANT

anova(fm9.2, fm12.1)

