
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
# Raw residuals of fm9.2
# ------------------------------------------------------------------------------

plot(fm9.2, resid(., type = "response") ~ fitted(.))


# -->
# It dispalyes an asymmetric pattern, with large positive (negative) residuals present
# mainly for small (large) fitted values



lattice::bwplot(resid(fm9.2) ~ time.f, pch = "|", data = armd)



# -->
# clearly show an increasing variance of the residuals



# ------------------------------------------------------------------------------
# Pearson residuals of fm9.2
# ------------------------------------------------------------------------------

plot(fm9.2, resid(., type = "pearson") ~ fitted(.))



# -->
# similarly it dispalyes an asymmetric pattern



lattice::bwplot(resid(fm9.2, type = "pearson") ~ time.f, pch = "|", data = armd)



# -->
# The plots illustrate the effect of scaling:  the variance of the residuals is virtually constant.



# ------------------------------------------------------------------------------
# Scale-location plots
# ------------------------------------------------------------------------------


plot(fm9.2, sqrt(abs(resid(., type = "response"))) ~ fitted(.), type = c("p", "smooth"))


plot(fm9.2, sqrt(abs(resid(., type = "pearson"))) ~ fitted(.), type = c("p", "smooth"))



# -->
# Square-root transformation of the absolute value of the residuals versus fitted values.
# The plots allow for detection of patterns in the residual variance.

# raw resisulas suggests a dependence between the residual variance and the mean value
# BUT THIS MAY BE AN ARTIFACT OF THE HETEROSCEDASTICITY OF THE RAW RESIDUALS.

# It might be better to look at the scale-location plot for the Pearson residuals.
# The plot does not indicate any clear trend in the residual variance


