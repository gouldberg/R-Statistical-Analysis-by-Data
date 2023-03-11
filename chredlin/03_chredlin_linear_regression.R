setwd("//media//kswada//MyFiles//R//chredlin")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  chredlin
# ------------------------------------------------------------------------------

data("chredlin", package = "faraway")

str(chredlin)




# ------------------------------------------------------------------------------
# Linear regression model
# ------------------------------------------------------------------------------


linmod <- lm(involact ~ race + fire + theft + age + log(income), data = chredlin)

# linmod <- lm(involact ~ race + fire + theft + age + income, data = chredlin)


summary(linmod)




# ----------
# variance inflation factor

car::vif(linmod)



# -->
# We do not have variables with VIF > 5




# ------------------------------------------------------------------------------
# Model terms significance:  Sequential Analysis of Variance  (Type I analysis of variance)
# ------------------------------------------------------------------------------

anova(linmod)


# 1. intercept only                         vs. + race
# 2. incercept + race                       vs. + fire
# 3. incercept + race + fire                vs. + theft ....





# ----------
# for reference
anova(lm(involact ~ race + fire + theft + age + log(income), data = chredlin))




# ------------------------------------------------------------------------------
# Model terms significance:  Type II test
# ------------------------------------------------------------------------------

car::Anova(linmod)


# 1. race not included  vs. linmod
# 2. fire not included  vs. linmod
# 3. theft not included  vs. linmod ...


