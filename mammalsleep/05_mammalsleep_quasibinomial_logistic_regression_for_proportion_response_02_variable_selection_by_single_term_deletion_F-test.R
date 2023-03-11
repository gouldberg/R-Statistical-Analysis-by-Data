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
# Variable selection by single term deletion F-test
# ------------------------------------------------------------------------------

# Since we now have a free dispersion parameter, we can not use step()

drop1(qlmod, test = "F")


# -->
# We might eliminate predation as the least significant variable



# ----------
qlmod2 <- glm(pdr ~ log(body) + log(brain) + log(lifespan) + log(gestation) + exposure + danger, family = quasibinomial, mammalsleep)

drop1(qlmod2, test = "F")

qlmod2 <- glm(pdr ~ log(body) + log(brain) + log(lifespan) + log(gestation) + danger, family = quasibinomial, mammalsleep)

drop1(qlmod2, test = "F")

qlmod2 <- glm(pdr ~ log(body) + log(lifespan) + log(gestation) + danger, family = quasibinomial, mammalsleep)

drop1(qlmod2, test = "F")

qlmod2 <- glm(pdr ~ log(body) + log(lifespan) + danger, family = quasibinomial, mammalsleep)

drop1(qlmod2, test = "F")



# ----------
qlmod <- qlmod2

summary(qlmod)



# -->
# Notice that the dispersion parameter is far less than the default value of one (= 0.041)
