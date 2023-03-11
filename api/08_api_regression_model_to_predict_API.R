setwd("//media//kswada//MyFiles//R//api")

packages <- c("dplyr", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  subsample from the California Academic Performance Index
# ------------------------------------------------------------------------------

data(api)

str(apiclus1)


# The svydesign function returns an object containing the survey data and metadata
dclus1 <- svydesign(id = ~ dnum, weights = ~ pw, data = apiclus1, fpc = ~ fpc)



# ------------------------------------------------------------------------------
# regression model
# ------------------------------------------------------------------------------
# Regression models show that these socieconomic variables predict API score and whether the school achieved its API target

regmodel <- svyglm(api00 ~ ell + meals, design = dclus1)

summary(regmodel)



# ----------
logitmodel <- svyglm(I(sch.wide == "Yes") ~ ell + meals, design = dclus1, family = quasibinomial())

summary(logitmodel)


