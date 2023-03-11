setwd("//media//kswada//MyFiles//R//gala")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\gala")



# ------------------------------------------------------------------------------
# data:  gala
# ------------------------------------------------------------------------------

data("gala", package = "faraway")

str(gala)

dim(gala)


car::some(gala)



# ----------
# model
lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)




# ------------------------------------------------------------------------------
# Testing one predictor:  coefficients significance
# ------------------------------------------------------------------------------


summary(lmod)



# -->
# in original model, Area is not significant




# ----------
# exclude Area 
lmods <- lm(Species ~ Elevation + Nearest + Scruz + Adjacent, data = gala)

# lmods <- update(lmod, . ~ . - Area)



# ----------
# compare coefficients
coef(lmod)

coef(lmods)



# ----------
# Test Area significance
# Null Hypothesis:  Coefficients of Area is zero

anova(lmods, lmod)



# -->
# p = 0.2963 indicates that the null hypothesis is NOT rejected
# Area is not significant in this model



# ----------
# sum of squares (ss) = 4237.7

sum(lmods$residuals^2) - sum(lmod$residuals^2)



# ----------
# t stat

( tstat <- ( coef(lmod)["Area"] - 0 ) / lmodsum$coef[,"Std. Error"]["Area"] )


df <- 24
2 * pt(tstat, df)



# ----------
# But note that Area is significant by simple model
summary(lm(Species ~ Area, data = gala))




# ----------
# This is same:  p value for Area is 0.2963
car::Anova(lmod)




# ------------------------------------------------------------------------------
# Testing a pair of predictors
# ------------------------------------------------------------------------------

# exclude Area and Adjacent

lmods2 <- lm(Species ~ Elevation + Nearest + Scruz, data = gala)

# lmods2 <- update(lmod, . ~ . - Area - Adjacent)


summary(lmods2)



# ----------
# Null Hypothesis: both coeffs of Area and Adjacent = zero
anova(lmods2, lmod)



# -->
# p < 0.05 indicate that Null is rejected.



# ----------
# sum of squares (ss) = 69060

sum(lmods2$residuals^2) - sum(lmod$residuals^2)




# ------------------------------------------------------------------------------
# Testing a subspace
# ------------------------------------------------------------------------------

# I():  the argument is evaluated (in this case actual addition) rather than interpreted as part of model

head(model.matrix(~ I(Area + Adjacent) + Area + Adjacent + Elevation + Nearest + Scruz, data = gala))



# ----------
# the combined term of Area and Adjacent by same coeffs (= 1) is significant ??

lmod3 <- lm(Species ~ I(1 * Area + 1 * Adjacent) + Elevation + Nearest + Scruz, data = gala)

# lmods3 <- update(lmod, . ~ . - Area - Adjacent + I(1 * Area + 1 * Adjacent))


summary(lmod3)



# ----------
# Null Hypothesis:  both coeffs of Area and Adjacent = 1 ??
anova(lmod3, lmod)



# -->
# rejected:  indicating proposed simplification to a single combined area predictor is NOT justifiable



# ------------------------------------------------------------------------------
# Fixed term
# ------------------------------------------------------------------------------


# offset() set fixed term in the regression

head(model.matrix(~ Area + offset(0.5 * Elevation) + Nearest + Scruz + Adjacent, data = gala))



# ----------
lmod4 <- lm(Species ~ Area + offset(0.5 * Elevation) + Nearest + Scruz + Adjacent, data = gala)


summary(lmod4)



# -->
# Note that Elevation is not included in summary
# DF = 25 = 30 - (4 + 1)
# Elevation is not counted as df




# ----------
# Null Hypothesis:  The coeff of I(0.5 * Elevation) = 1 ??
anova(lmod4, lmod)



# -->
# rejecting:  coeff is NOT 1



