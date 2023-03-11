setwd("//media//kswada//MyFiles//R//crabs")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  crabs
#   - data from Campbell, describing 5 morphological measurements on 50 crabs each of two colour forms and both sexes,
#     of the species Leptograpsus variegatus collected at Fremantle, W. Australia.
#   - Variables
#       - FL: frontal lobe size (mm)
#       - RW: rear width (mm)
# ------------------------------------------------------------------------------

data("crabs", package = "MASS")


str(crabs)


car::some(crabs)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

# scatterplot of crabe measurements of the genus Leptograpsus according to the species and sex
plot(crabs$FL, crabs$RW, bg = c("grey50", "white")[crabs$sex], pch = c(21, 22)[crabs$sp], cex = c(1.8, 1)[crabs$sp])



# ----------
mod <- lm(crabs$RW ~ crabs$FL)

summary(mod)

abline(mod)