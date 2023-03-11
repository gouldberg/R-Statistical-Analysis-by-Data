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
# data exploration:  plot trajectories
# ------------------------------------------------------------------------------

# Hip Angle and Knee Angle of sample 4 subjects

idx <- sample(1:39, size = 4, replace = FALSE)


par(mfrow=c(2,2))

for(i in 1:length(idx)) matplot(gait[,idx[i],1:2], type = "b", main = paste0("subject: ", idx[i]))
