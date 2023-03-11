setwd("//media//kswada//MyFiles//R//phdpubs")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PhdPubs data
# ------------------------------------------------------------------------------

data("PhdPubs", package = "vcdExtra")


dim(PhdPubs)

str(PhdPubs)


car::some(PhdPubs)



# ----------
PhdPubs <- within(PhdPubs, {
  female <- factor(female)
  married <- factor(married)
})



# ----------
modp <- glm(articles ~ ., data = PhdPubs, family = poisson(link = "log"))



# ------------------------------------------------------------------------------
# Test the significance of each othe predictors relative to the full model  (F-test)
# ------------------------------------------------------------------------------

# single term deletion
# Test the significance of each of the predictors relative to the full model, Use F-test (not chi2 test)
drop1(modp, test="F")

# drop1(mod.pois, test="Chi")


# -->
# The z-statistics from the summary() are less reliable and so the F-test is preferred.  (AIC-based criteria is applied)
# not much difference



# ------------------------------------------------------------------------------
# Terms added sequentially (first to last)
# ------------------------------------------------------------------------------

anova(modp, test = "Chisq")

