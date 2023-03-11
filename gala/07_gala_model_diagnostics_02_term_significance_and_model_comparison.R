setwd("//media//kswada//MyFiles//R//gala")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gala
# ------------------------------------------------------------------------------

data("gala", package = "faraway")

str(gala)

dim(gala)


car::some(gala)



# ----------
mod_obj <- modp.step2
mod_obj <- modp2



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


# -->
car::Anova(modp)




# ------------------------------------------------------------------------------
# Terms added sequentially (first to last)
# ------------------------------------------------------------------------------

anova(modp, test = "Chisq")

