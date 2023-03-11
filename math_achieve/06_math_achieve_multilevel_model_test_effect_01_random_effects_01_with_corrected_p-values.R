setwd("//media//kswada//MyFiles//R//math_achieve")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  MathAchieve
# ------------------------------------------------------------------------------

data("MathAchieve", package = "nlme")

dim(MathAchieve)

str(MathAchieve)

car::some(MathAchieve)



# ----------
data("MathAchSchool", package = "nlme")

dim(MathAchSchool)

str(MathAchSchool)

car::some(MathAchSchool)



# ----------
names(MathAchieve) <- tolower(names(MathAchieve))
names(MathAchSchool) <- tolower(names(MathAchSchool))


Temp <- MathAchieve %>% group_by(school) %>% summarize(mean.ses = mean(ses))
Temp <- merge(MathAchSchool, Temp, by = "school")
car::brief(Temp)


# ----------
HSB <- merge(Temp[, c("school", "sector", "mean.ses")], MathAchieve[, c("school", "ses", "mathach")], by = "school")


# ----------
HSB$cses <- with(HSB, ses - mean.ses)
car::brief(HSB)



# ------------------------------------------------------------------------------
# Test variance and covariance component:  assuming the variance component cses for the slope is zero.
# ------------------------------------------------------------------------------

# When linear mixed models are fit by REML, we must be careful, however, to compare models that are identical in their fixed effects.


# We first fit an alternative model that assumes that the variance component cses for the slopes is zero.
# We omit random effect of cses
# This is equivalent to specifying a constant correlation between pairs of observations in the sample group.

hsb.lme.2 <- update(hsb.lme.1, random = ~ 1 | school)


S(hsb.lme.2)



# ----------
# likelihood ratio test
anova(hsb.lme.1, hsb.lme.2)




# ------------------------------------------------------------------------------
# Test variance and covariance component:  retains random slopes but includes no variance component for the intercepts
# ------------------------------------------------------------------------------


# We also fit a model the retains random slopes but includes no variance component for the intercepts,
# which implies that schools within sectors have the same average adjusted levels of math achievement but that the relationship between
# students' math achievement and their SES can vary from school to school

# We omit random intercept

hsb.lme.3 <- update(hsb.lme.1, random = ~ cses - 1 | school)


S(hsb.lme.3)



# ----------
# likelihood ratio test
anova(hsb.lme.1, hsb.lme.3)




# -->
# Each of these likelihood ratio tests is on two degrees of freedom, because excluding one of the random effects removes ont only its variance from the model
# but also its covariance with the other random effect

# The larger p-value in the first of these tests suggests no evidence against the constant-correlation model,
# thus that a random slope for each school is not required.

# In contrast, the small p-value for the second test suggests that even accounting for differences due to sector and mean school SES,
# average math achievement varies from school to school.



# ------------------------------------------------------------------------------
# Take care of a boundary of the parameter space:  corrected p-values
# ------------------------------------------------------------------------------

# A more careful formulation of these test takes account of the fact that each null hypothesis places a variance (but not the covariance)
# component on a boundary of the parameter space -- that is a variance of zero.

# Consequently, the null distribution of the likelihood-ratio test statistic is not simply chi-square with 2 degrees of freedom but rather a mixture
# of chi-square distributions.

# We compute the corrected p-value

pvalCorrected <- function(chisq, df){
  (pchisq(chisq, df, lower.tail = FALSE) + pchisq(chisq, df - 1, lower.tail = FALSE)) / 2
}


pvalCorrected(1.1241, df = 2)

pvalCorrected(220.56, df = 2)



# -->
# Here the corrected p-values are similar to the uncorrected ones.



# ------------------------------------------------------------------------------
# compare coefficients
# ------------------------------------------------------------------------------

compareCoefs(hsb.lme.1, hsb.lme.2)



# -->
# resulting fixed-effects estimates and their standard errors are nearly identical to those for the initial model

# Estimates of fixed effects are often insensitive to precise specification of the random effects,
# as long as the maximized likelihoods under the alternative specifications are similar.


