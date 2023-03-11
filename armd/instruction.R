
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
# definition and initialization of an object class varIdent
# ------------------------------------------------------------------------------

library(nlme)


val <- c("12wks" = 0.5, "24wks" = 2)


( fix <- c("52wks" = 3) )



frm <- formula(~ 1 | time.f)



# Var. function object defined and initialized
vf0 <- varIdent(value = val, fixed = fix, form = frm)


# initialization is performed by evaluating the vf0 object for the variance covariate,
# i.e., the factor time.f from the armd dataset
vf0i <- Initialize(vf0, armd)


vf0i




# ------------------------------------------------------------------------------
# Extracting and assigning coefficients to an initialized object of class varident
# ------------------------------------------------------------------------------


# all coefs
coef(vf0i, unconstrained = FALSE, allCoef = TRUE)



# varying only
coef(vf0i, unconstrained = FALSE, allCoef = FALSE)



# ----------
# coefficients on the unconstrained scale

coef(vf0i, unconstrained = TRUE, allCoef = TRUE)


# varying only
coef(vf0i, unconstrained = TRUE, allCoef = FALSE)



# ----------
# new coefs assigned

coef(vf0i) <- c(-0.6, 0.7)


coef(vf0i, allCoef = TRUE)




# ------------------------------------------------------------------------------
# Extracting information from a varIdent class object
# ------------------------------------------------------------------------------


summary(vf0i)


formula(vf0i)


getGroups(vf0i)



# reciprocals of the weights for observations for subject 2
# for whom visual acuity measurements at all four timepoints are available
varWeights(vf0i)[3:6]



# sum of the logarithms of the weights, 
# which is contribution of the variance-function structure to the log-likelihood
logLik(vf0i)





