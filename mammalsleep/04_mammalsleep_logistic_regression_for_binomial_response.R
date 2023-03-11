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



# ------------------------------------------------------------------------------
# Logistic regression for binomial responses
# ------------------------------------------------------------------------------

lmod <- glm(cbind(dream, sleep) ~ log(body) + log(brain) + log(lifespan) + log(gestation) + predation + exposure + danger, family = binomial, data = mammalsleep)



# ----------
# Note that this summary shows not test by deviance,
# but test by z-value (= beta / se(beta)), which is approximately normally distributed.
summary(lmod)


# shortned summary
faraway::sumary(lmod)




# ----------
lmod2 <- step(lmod, direction = "both")


summary(lmod2)



# -->
# Still not significant



