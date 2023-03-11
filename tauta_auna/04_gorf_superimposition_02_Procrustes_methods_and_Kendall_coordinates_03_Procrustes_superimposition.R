setwd("//media//kswada//MyFiles//R//iris")

packages <- c("dplyr", "shapes")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

source(file = "//media//kswada//MyFiles//R_basics//functions_for_morphometrics.R")



# ------------------------------------------------------------------------------
# data:  gorf and gorm
# ------------------------------------------------------------------------------

dim(gorf.dat)

dim(gorm.dat)




# ------------------------------------------------------------------------------
# Full Procrustes superimposition
# ------------------------------------------------------------------------------

M1 <- gorf.dat[,,1]
M2 <- gorf.dat[,,2]



# Full Procrustes superimposition M1 on M2
fp_obj <- fPsup(M1, M2)

fp_obj



# ----------
graphics.off()

par(mfrow=c(2,2))
plot(M1, axes = F, xlab = "", ylab = "", main = "original config")
polygon(M1)

plot(M2, axes = F, xlab = "", ylab = "", main = "reference config")
polygon(M2)

plot(fp_obj$Mp1, axes = F, xlab = "", ylab = "", main = "superimposed config", pch = 10, col = "red")
polygon(fp_obj$Mp1)

plot(fp_obj$Mp2, axes = F, xlab = "", ylab = "", main = "reference config after fP")
polygon(fp_obj$Mp2)



# ----------
# sum of interlandmark distances between M1 and M2

fp_obj$DF




# ------------------------------------------------------------------------------
# Partial Procrustes superimpostiion
# ------------------------------------------------------------------------------

M1 <- gorf.dat[,,1]
M2 <- gorf.dat[,,2]



# partial Procrustes superimposition M1 on M2
pp_obj <- pPsup(M1, M2)

pp_obj



# ----------
graphics.off()

par(mfrow=c(2,2))
plot(M1, axes = F, xlab = "", ylab = "", main = "original config")
polygon(M1)

plot(M2, axes = F, xlab = "", ylab = "", main = "reference config")
polygon(M2)

plot(pp_obj$Mp1, axes = F, xlab = "", ylab = "", main = "superimposed config", pch = 10, col = "red")
polygon(pp_obj$Mp1)

plot(pp_obj$Mp2, axes = F, xlab = "", ylab = "", main = "reference config after fP")
polygon(pp_obj$Mp2)



# ----------
# sum of interlandmark distances between M1 and M2 (= sin(rho))
fp_obj$DF
sin(pp_obj$rho)


# partial Procrustes distances
pp_obj$DP


# trigonometric Procrustes distance
# smallest curvilinear length between preshapes on the hupershpere or the angle between superimposed configurations
# and the origin of the hypershpere.
pp_obj$rho



# -->
# When the differences between shapes are small, full Procrustes distances, partial Procrustes distances and trigonometric Procrustes distances
# are nearly similar.



