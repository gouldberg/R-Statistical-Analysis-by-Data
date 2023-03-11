setwd("//media//kswada//MyFiles//R//codparasites")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Cod Parasites
# ------------------------------------------------------------------------------

data("CodParasites", package = "countreg")


dim(CodParasites)

str(CodParasites)


car::some(CodParasites)



# ----------
CodParasites$prevalence <- ifelse(CodParasites$intensity == 0, "no", "yes")



# ----------
modp <- glm(intensity ~ length + area * year, data = CodParasites, family = poisson(link = "log"))



# ------------------------------------------------------------------------------
# Test the significance of each othe predictors relative to the full model  (F-test)
# ------------------------------------------------------------------------------

# single term deletion
# Test the significance of each of the predictors relative to the full model, Use F-test (not chi2 test)
drop1(modp, test="F")

# drop1(modp, test="Chi")


# -->
# The z-statistics from the summary() are less reliable and so the F-test is preferred.  (AIC-based criteria is applied)
# not much difference



# ------------------------------------------------------------------------------
# Terms added sequentially (first to last)
# ------------------------------------------------------------------------------

anova(modp, test = "Chisq")

