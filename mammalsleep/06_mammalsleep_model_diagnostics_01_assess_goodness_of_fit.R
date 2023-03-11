setwd("//media//kswada//MyFiles//R//mammalsleep")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  mammalsleep
# ------------------------------------------------------------------------------

data("mammalsleep", package = "faraway")


str(mammalsleep)


mammalsleep$pdr <- with(mammalsleep, dream / sleep)


head(mammalsleep)



# ----------
summary(qlmod)



# ----------
qlmod <- glm(pdr ~ log(body) + log(lifespan) + danger, family = quasibinomial, data = mammalsleep)


mod_obj <- qlmod



# ------------------------------------------------------------------------------
# Goodness of Fit:  deviance vs null.deviance
# ------------------------------------------------------------------------------

# Chi-square test does not reject. but this is no a particulary well-fitting model
pchisq(deviance(mod_obj), mod_obj$null.deviance, lower = FALSE)




# ------------------------------------------------------------------------------
# Goodness of Fit:  The proportion of deviance explained  (Nagelkerke)
# ------------------------------------------------------------------------------

# Nagelkerke R^2
n <- mod_obj$df.null + 1

(1 - exp((mod_obj$dev - mod_obj$null)/n)) / (1 - exp(-mod_obj$null/n))



# ----------
# McFadden's peudo R^2
1 - mod_obj$deviance / mod_obj$null.deviance



# ------------------------------------------------------------------------------
# Goodness of Fit:  Hosmer - Lemeshow Test
# ------------------------------------------------------------------------------

library(ResourceSelection)



# g = 10:  number of bins to use to calcualte quantiles
( hl <- hoslem.test(mod_obj$y, fitted(mod_obj), g = 10) )


# -->
# Hosmer - Lemeshow Test:  p = 0.9998 indicates good fit



# Observed vs Expected
cbind(hl$observed, hl$expected)


