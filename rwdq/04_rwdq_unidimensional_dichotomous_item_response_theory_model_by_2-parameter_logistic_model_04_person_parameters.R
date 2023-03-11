setwd("//media//kswada//MyFiles//R//rwdq")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  RWDQ (Work Design Questionnaire R Package Authors)
# ------------------------------------------------------------------------------

data("RWDQ", package = "MPsychoR")

str(RWDQ)


car::some(RWDQ)




# ------------------------------------------------------------------------------
# Person parameters
# ------------------------------------------------------------------------------

( ppars <- ltm::factor.scores(fit2pl2, resp.patterns = RWDQ1)$score.dat[, "z1"] )


