setwd("//media//kswada//MyFiles//R//lawn")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  lawn
# ------------------------------------------------------------------------------

data("lawn", package = "faraway")

str(lawn)


car::some(lawn)



# ------------------------------------------------------------------------------
# Confidence interval of SD
# ------------------------------------------------------------------------------

confint(cmod, method = "boot")



# -->
# If the same machine were tested at the same speed, standard deviation of the times observed is [0 - 23.1576] at 95%

# If different machines were sampled from the same manufacturer and tested at the same speed once only,
# standard deviaton of the times observed is [-0.09 - 20.9377] at 95%


# -->
# the random variation by machine is larger than fixed effect variation manufacturer
