setwd("//media//kswada//MyFiles//R//els_career_academy")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Define survey design
# ------------------------------------------------------------------------------

# define design object that describes the sample characteristics the variable psu identifies the primary sampling units (cluster ids)
# the variable STRATA_ID identifies the strata ids
# the variable bystuwt identifies the base-year sampling weights for respondents of the 2002 and 2004 waves (Base year and 1st follow-up)
# nest = "T" relabels cluster ids to make them nested within strata.

library(survey)

options(survey.lonely.psu = "adjust")


surveyDesign1 <- svydesign(ids = ~ psu, strata = ~ STRAT_ID, weights = ~ bystuwt, data = imputation1, nest = T)


surveyDesignAll <- svydesign(ids = ~ psu, strata = ~ STRAT_ID, weights = ~ bystuwt, data = allImputations, nest = T)



# ----------
names(surveyDesign1)


names(surveyDesignAll)

