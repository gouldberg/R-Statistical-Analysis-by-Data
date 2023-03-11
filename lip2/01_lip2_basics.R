setwd("//media//kswada//MyFiles//R//lip2")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  lip2
#   - The data of the movement of lips during speech production
#     There are 20 replications and 51 sampling points.
# ------------------------------------------------------------------------------

data(lip, package = "fda")

dim(lip)


head(lip)


# -----------
liptime



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

matplot(x = liptime, y = lip, type = 'l',
        xlab='Normalized Time', ylab='lip position (mm)')


# -->
# The position of the lower lip when saying the word "Bob" 20 times.
# There are distinct opening and shutting phases of the mouth surrounding a fairly linear trend that corresponds to the vocalization of the vowel.

# Muscle tissue behaves in many ways like a spring.
# This observations suggests that we consider fitting a second-order equation to these data.

