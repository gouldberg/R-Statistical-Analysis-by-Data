setwd("//media//kswada//MyFiles//R//gait")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gait
# ------------------------------------------------------------------------------

data("gait", package = "fda")

str(gait)

dim(gait)



# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------

# boy1  Knee Angle and Hip Angle
# time range is 0.025 to 0.975 (20 points)
gait[,"boy1","Knee Angle"]

gait[,"boy1","Hip Angle"]

gait[,"boy1",c("Hip Angle", "Knee Angle")]

gait[,1,1:2]



# -----------
# Time 1 data
head(gait[1,,])



# ----------
summary(gait[,,"Hip Angle"])

summary(gait[,,"Knee Angle"])


