
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\armd")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



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
# Empirical semivariograms for Pearson residuals
# ------------------------------------------------------------------------------

# per time difference

( Vg1 <- nlme::Variogram(fm9.2, form = ~ time | subject) )
( Vg1 <- nlme::Variogram(fm9.2, form = ~ time | subject, robust = TRUE) )


plot(Vg1, smooth = FALSE, xlab = "Time difference", pch = 20, cex = 2, ylim = c(0, 0.8))




# ----------
# per time lag

( Vg2 <- nlme::Variogram(fm9.2, form = ~ tp | subject) )
( Vg2 <- nlme::Variogram(fm9.2, form = ~ tp | subject, robust = TRUE) )


plot(Vg2, smooth = FALSE, xlab = "Time Lag", pch = 20, cex = 2, ylim = c(0, 0.8))




# -->
# The correlation decreases with the dime difference / lag.
# Thus a correlation structure like, e.g., a compound symmetry, will most likely not fit the data well.

# A more appropriate structure might be, e.g., an autoregressive process of order 1.


