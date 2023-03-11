setwd("//media//kswada//MyFiles//R//els_career_academy")

packages <- c("dplyr", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# calculate propensity score weights
# from the propensity scores obtained from logistic regression
# 
# normalize the sampling weight
# ------------------------------------------------------------------------------


# normalize the base year student sampling weight so that it sums to the sample size
ELS.data.imputed$bystuwt

mean(ELS.data.imputed$bystuwt)

ELS.data.imputed$bystuwt <- ELS.data.imputed$bystuwt / mean(ELS.data.imputed$bystuwt)




# ------------------------------------------------------------------------------
# Define survey design
# ------------------------------------------------------------------------------

options(survey.lonely.psu = "adjust")


# define design object that describes the sample characteristics
# the variable psu identifies the primary sampling units (cluster ids)
# the variable STRATA_ID identifies the strata ids
# the variable bystuwt identifies the base-year sampling weights for
# respondents of the 2002 and 2004 waves (Base year and 1st follow-up)

surveyDesign <- svydesign(ids = ~ psu, strata = ~ STRAT_ID, weights = ~ bystuwt, data = ELS.data.imputed, nest = T)

surveyDesign




# ----------
# get weighted percentages of treated and untreated cases
# the treatment variable is "treat" - Ever in career academy from BY Student Questionnaire

( treatmentTable <- svymean(~ treat, surveyDesign) )



# -->
# 8.8% of cases received treatment


# note that raw data:  7.9% of cases received treated
table(ELS.data.imputed$treat)

prop.table(table(ELS.data.imputed$treat))




# ------------------------------------------------------------------------------
# Obtain weights for estimating ATT
# ------------------------------------------------------------------------------

# obtain weights for estimating the ATT for propesnity scores
# 1 for students who participated in a career academy and odds of treatment for students who did not participate

ELS.data.imputed$weightATT <- with(ELS.data.imputed, ifelse(treat == 1, 1, pScores / (1 - pScores))) 

ELS.data.imputed$weightATTRf <- with(ELS.data.imputed, ifelse(treat == 1, 1, pScoresRf / ( 1 - pScoresRf))) 

ELS.data.imputed$weightATTGBM <- with(ELS.data.imputed, ifelse(treat == 1, 1, pScoresGBM / (1 - pScoresGBM))) 



# summary of the ATT weights for treated and untreated groups
with(ELS.data.imputed, by(weightATT, treat, summary))

with(ELS.data.imputed, by(weightATTRf, treat, summary))

with(ELS.data.imputed, by(weightATTGBM, treat, summary))



# ------------------------------------------------------------------------------
# Obtain weights for estimating ATE
# ------------------------------------------------------------------------------

# For each individual, the propensity score weight for estimating the ATE is the inverse of the probability of exposure to the condition the individual was exposed to.

ELS.data.imputed$weightATE <- with(ELS.data.imputed, ifelse(treat == 1, 1 / pScores, 1 / (1 - pScores)))

ELS.data.imputed$weightATERf <- with(ELS.data.imputed,ifelse(treat== 1,  1 / pScoresRf, 1 / (1 - pScoresRf)))

ELS.data.imputed$weightATEGBM <- with(ELS.data.imputed,ifelse(treat == 1, 1 / pScoresGBM, 1 / (1 - pScoresGBM)))



with(ELS.data.imputed, by(weightATE, treat, summary))

with(ELS.data.imputed, by(weightATERf, treat, summary))

with(ELS.data.imputed, by(weightATEGBM, treat, summary))



# -->
# Individuals with extreme weights for the ATE are those who are either very likely to participate in the treatment given their covariate values
# but did not or very unlikely to participate but did so.



# ------------------------------------------------------------------------------
# obtain weights for multiple imputed datasets
# ------------------------------------------------------------------------------

library(mitools)


# add propensity score mean to imputed datasets
allImputations <- update(allImputations, weightATT = ifelse(treat == 1, 1, pScores /(1 - pScores)))


with(allImputations, by(weightATT, treat, summary))

