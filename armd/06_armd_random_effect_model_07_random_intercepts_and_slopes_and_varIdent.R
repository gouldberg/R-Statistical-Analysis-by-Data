
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
# An alternative residual variance function:  varIdent  (more general variance function)
# ------------------------------------------------------------------------------


( fm16.6 <- update(fm16.3, weights = varIdent(form = ~ 1 | time.f)) )



printCoefmat(summary(fm16.6)$tTable, has.Pvalue = TRUE, P.value = TRUE)



# ----------
# we can use since varPower (M16.3) nested in varIdent (M16.6)

anova(fm16.3, fm16.6)


# -->
# the use of more general varIdent variance function gives a better fit




# ----------
summary(fm16.6)


# -->
# but the parameter delta4 (52wk) is extremely small and substantially differs
# from the estimated values of dela2 and delta3.
# This is surprising, because all previous analyses indicated that the variance of
# the last visual acuity measurement (at week 52) was the largest.



# ----------
intervals(fm16.6, which = "var-cov")



# -->
# the estimated 52wks variance function have very wide range ...



# ----------
qqnorm(fm16.6, ~ resid(.) | time.f)



# -->
# note that the residuals for week52 are all equal to zero.

# Given the number of parameters is close to the number of equations,
# collinearity among the parameters may result ..


