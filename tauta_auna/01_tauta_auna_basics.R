setwd("//media//kswada//MyFiles//R//tauta_auna")


packages <- c("dplyr", "shapes")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

source(file = "//media//kswada//MyFiles//R_basics//functions_for_morphometrics.R")



# ------------------------------------------------------------------------------
# data:  tauta and auna
#   - Outlines of fossil rodent molars belonging to two different species, Megacricetodon tautavelensis and Megacricetodon aunayi.
#   - The original dataset contains 64 points sampled in a clockwise way on upper left molar of 29 and 31 individuals for each species.
#
#   - Prior to digitization, teeth were numerized under a stereographic camera.
#     Depending on their orientation below the camera, teeth could actually look wider or longer.
# ------------------------------------------------------------------------------

tauta <- read.table("//media//kswada//MyFiles//references//MorphometricsWithR//e-supplement//tauta.R")

auna <- read.table("//media//kswada//MyFiles//references//MorphometricsWithR//e-supplement//auna.R")

str(tauta)

str(auna)



# ----------
# The datasets are stored in tow data frames, of k columns, and n * p rows.
# For convenience, we transform them in 2 array objects, tauta and auna, of respective dimensions

taut <- array(NA, dim = c(64, 2, 29))

aun <- array(NA, dim = c(64, 2, 31))

taut[,1,] <- tauta[,1];  taut[,2,] <- tauta[,2]

aun[,1,] <- auna[,1];  aun[,2,] <- auna[,2]



# ------------------------------------------------------------------------------
# basics:  show outlines by sample
# ------------------------------------------------------------------------------

idx_t <- sample(dim(taut)[3], size = 3, replace = FALSE)
idx_a <- sample(dim(aun)[3], size = 3, replace = FALSE)


par(mfrow=c(2,3), mar=c(2,2,2,2))

for(i in idx_t){
  plot(taut[,,i], axes = FALSE, xlab = "", ylab = "", main = paste("taut", i))
  polygon(taut[,,i])
}

for(i in idx_a){
  plot(aun[,,i], axes = FALSE, xlab = "", ylab = "", main = paste("aun", i))
  polygon(aun[,,i])
}

