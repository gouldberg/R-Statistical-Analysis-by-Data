# setwd("//media//kswada//MyFiles//R//squid")
setwd("//media//kswada//MyFiles//R//Generalized_linear_model_and_heterogeneity//squid")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Squid
# ------------------------------------------------------------------------------

Squid <- read.table(file = "Squid.txt", header = TRUE)


str(Squid)


dim(Squid)


car::some(Squid)



# ----------
Squid$fMONTH = factor(Squid$MONTH)



# ----------
M1 <- lm(Testisweight ~ DML * fMONTH, data = Squid)

mod_obj <- M1



# ------------------------------------------------------------------------------
# Test the significance of each othe predictors relative to the full model  (F-test)
# ------------------------------------------------------------------------------

# single term deletion
# Test the significance of each of the predictors relative to the full model, Use F-test (not chi2 test)
drop1(mod_obj, test="F")

# drop1(mod_obj, test="Chi")


# -->
# The z-statistics from the summary() are less reliable and so the F-test is preferred.  (AIC-based criteria is applied)
# not much difference



# -->
car::Anova(mod_obj)

anova(mod_obj)



# ------------------------------------------------------------------------------
# Terms added sequentially (first to last)
# ------------------------------------------------------------------------------

anova(mod_obj, test = "Chisq")

