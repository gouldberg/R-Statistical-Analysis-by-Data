
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
# compare with arbitrary variance model
# ------------------------------------------------------------------------------

# most general variance function, which allows arbitrary (positive) variances of the visual acuity measurements
# made at different timepoints

fmA.vc <- update(fm12.3, weights = varIdent(form = ~1 | time.f))




# ----------
# power-of-time variance function vs. timepoint-specific variances
anova(fm12.3, fmA.vc)



# -->
# NOT significant
# indicating that as compared to the model with the general variance and correlation structures,
# the simpler model fm12.3 provides an adequate summary of the data ....




summary(fmA.vc)

