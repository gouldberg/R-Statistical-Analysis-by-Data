setwd("//media//kswada//MyFiles//R//asti")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ASTI
#   - dataset from Koller et al. (2017) who analyzed the Adult Self-Transcendence Inventory (ASTI), a self-report scale measuring the
#     target construct of wisdom.
#   - The ASTI has five subscales: self-knowledge and integraton (SI), peace of mind (PM), non-attachment (NA), self-transcendence (ST),
#     and presence in the here-and-now and growth (PG).
#     PG subscale has six items; 4 of them are on a 3-point scale and two of them on a 4-point scale.
# ------------------------------------------------------------------------------

data("ASTI", package = "MPsychoR")


str(ASTI)



