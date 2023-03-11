# setwd("//media//kswada//MyFiles//R//lalonde_nswcps")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\algebra_nation")

packages <- c("dplyr", "tidyverse", "MatchIt", "Matching", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data: Algebra Nation
# ------------------------------------------------------------------------------


data <- read.csv("algebra_nation.csv")


str(data)


car::some(data)


names(data)




# ----------
# Standardize continuous covariates

data$numOfStud2014 <- scale(data$numOfStud2014)

data$meanScale2012 <- scale(data$meanScale2012)

data$lev1Perc2012 <- scale(data$lev1Perc2012)

data$lev5Perc2012 <- scale(data$lev5Perc2012)

data$perc.free.lunch <- scale(data$perc.free.lunch)

data$perc.reduced.lunch <- scale(data$perc.reduced.lunch)



# ----------
# convert binary variables to factors

data$SeniorHigh <- factor(data$SeniorHigh)

data$middleHigh <- factor(data$middleHigh)




# ----------
# breakdown locale into size and type variables

data$locationSize <- with(data, ifelse(Locale.=="City: Large" |
                                         Locale.=="Suburb: Large", "Large", ifelse( Locale.=="City: Midsize"|
                                                                                      Locale.=="Suburb: Midsize", "Midsize", "Small")))  

data$locationSize <- factor(data$locationSize)

data$locationRural <- with(data, ifelse(Locale.=="Rural: Distant" |
                                          Locale.=="Rural: Fringe" |
                                          Locale.=="Rural: Remote", "Rural", "Urban"))  

data$locationRural <- factor(data$locationRural)




# ------------------------------------------------------------------------------
# Estimate the average treatment effect of logins per examinee
# ------------------------------------------------------------------------------


# apply IPW weight

designIPW <- svydesign(ids = ~ 1, weights = ~ IPW, data = data)


modelOutcome_IPW <- svyglm(formula = meanScale2014 ~ logLoginsPerExaminee,
                       design = designIPW, family = gaussian)


summary(modelOutcome_IPW)



# -->
# coefficient of "logLoginsPerExaminee" is 0.6249




# ----------
# obtain a standardized coefficient

coef(modelOutcome_IPW)[2] * sqrt(coef(svyvar( ~ logLoginsPerExaminee, designIPW))) / sqrt(coef(svyvar(~ meanScale2014, designIPW)))



# -->
# 0.1007586




# ------------------------------------------------------------------------------
# Estimate the average treatment effect:  nonlinear model
# ------------------------------------------------------------------------------


modelOutcome_IPW_Nl <- svyglm(formula = meanScale2014 ~ logLoginsPerExaminee + 
                           I(logLoginsPerExaminee ^ 2) + I(logLoginsPerExaminee ^ 3),
                         design = designIPW, family = gaussian)


summary(modelOutcome_IPW_Nl)



# -->
# coefficient:  0.94695



# ----------
# compare linear and nonlinear models

anova(modelOutcome_IPW_Nl, modelOutcome_IPW)



# -->
# not different ... (p = 0.71)




# ------------------------------------------------------------------------------
# Estimate the average treatment effect:  using the IPW and controlling for charter school
# ------------------------------------------------------------------------------

# For the current example, because the charter school indicator did not meet the target criterion for
# adequate covariate balance, the outcome model is expanded to include charter school as well as
# the interaction between charter school and treatment does.
# if covariates are included, it is important to center the covaraites and add interactions between the covariates
# and the treatment does, which will ensure that the coefficient of the treatment does variable
# can still be interpreted as the average treatment effect.


data$Charter2 <- scale(as.numeric(data$Charter == "Yes"), scale = F)



# recreate the design because Charter changed

designIPW <- svydesign(ids = ~ 1, weights = ~ IPW, data = data)


modelOutcome_IPW_DR <- svyglm(formula = meanScale2014 ~ logLoginsPerExaminee + 
                           Charter2 + Charter2 : logLoginsPerExaminee,
                         design = designIPW, family = gaussian)


summary(modelOutcome_IPW_DR)



# -->
# coefficient = 0.664
# This is the average treatment effect of log logins per examinee on mean Algebra EOC scores

# an increase of 10% in logins per examinee corresponds to an expected change in mean Algebra EOC scores of
# 0.664 * log(1.1 / 1.0) = 0.063

# This is a school-level effect, so no inference can be maded about individual student scores.




# ----------
# obtain a standardized coefficient

coef(modelOutcome_IPW_DR)[2] * sqrt(coef(svyvar( ~ logLoginsPerExaminee, designIPW))) / sqrt(coef(svyvar(~ meanScale2014, designIPW)))



# -->
# 0.107



