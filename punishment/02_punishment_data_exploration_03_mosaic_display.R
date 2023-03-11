setwd("//media//kswada//MyFiles//R//punishment")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Punishment
# ------------------------------------------------------------------------------

data("Punishment", package = "vcd")


data <- Punishment


data


str(Punishment)



# ----------
# convert data.frame to table

tab <- xtabs(Freq ~ memory + attitude + age + education, data = data)


dimnames(tab) <- list(
  Memory = c("yes", "no"),
  Attitude = c("no", "moderate"),
  Age = c("15-24", "25-39", "40+"),
  Education = c("Elementary", "Secondary", "High")
)



# Memory * Attitude * Age * Education
tab



# ------------------------------------------------------------------------------
# mosaic display:  multi-way
# ------------------------------------------------------------------------------


dat2_2 <- margin.table(dat2, c(1,2))


mosaic(tab, shade = TRUE, gp_args = list(interpolate = 1:4), labeling = labeling_residuals)



# ----------
interp <- function(x) pmin(x / 6, 1)

mosaic(tab, shade = TRUE, gp_args = list(interpolate = interp))





