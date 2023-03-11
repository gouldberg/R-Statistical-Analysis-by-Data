setwd("//media//kswada//MyFiles//R//gorf")

packages <- c("dplyr", "shapes")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

source(file = "//media//kswada//MyFiles//R_basics//functions_for_morphometrics.R")



# ------------------------------------------------------------------------------
# data:  gorf and gorm
# ------------------------------------------------------------------------------

dim(gorf.dat)

dim(gorm.dat)



# ------------------------------------------------------------------------------
# Examine which landmarks may have some influence on shape difference btween configurations
#   - Lele and Richtsmeier suggest sorting the vector returend by the division of fm(M1) and fm(M2) and screening the landmarks that belong to
#     distances that appear at the extreme of the ranked vector.
# ------------------------------------------------------------------------------

gorf.dat[,,1]



# replace some influential value to 1st landmark's y
gorf.dat_tmp <- gorf.dat[,,1]

gorf.dat_tmp[1,2] <- 250

gorf.dat_tmp



# ----------
# Form Difference Matrix (FDM):  The rank matrix of the ratio of interlandmark distances of config 1 to that of config 2
p <- dim(gorf.dat[,,1])[1]

Ra1 <- matrix((rank(FM(gorf.dat[,,1]) / FM(gorf.dat[,,2]), na.last = "keep") + 0.5) / 2, ncol = p)

Ra1_2 <- matrix((rank(FM(gorf.dat_tmp) / FM(gorf.dat[,,2]), na.last = "keep") + 0.5) / 2, ncol = p)



# ----------
Name <- NA

for(i in 1:(p-1)){
  Name[(sum(((p-1):1)[1:i]) - p + i + 1):(sum(((p-1):1)[1:i]))] <- paste(i, (i + 1):p, sep = "-")
}

Name

Ra2 <- order(rank(fm(gorf.dat[,,1]) / fm(gorf.dat[,,2]), na.last = NA))
Ra2_2 <- order(rank(fm(gorf.dat_tmp) / fm(gorf.dat_tmp), na.last = NA))

Name[Ra2]
Name[Ra2_2]


# -->
# Here landmarks 1, 4 and 8 lie at the extreme of the distribution and influence difference in shape between configurations.



# ------------------------------------------------------------------------------
# Examine which landmarks may have some influence on shape difference btween configurations
#   - Alternatively, remove the rank of median value from the vector difference, and keep the absolute values of the results 
# ------------------------------------------------------------------------------

fdm <- fm(gorf.dat_tmp) / fm(gorf.dat[,,2])

Ra <- abs(fdm - median(fdm))


# In ordering the distribution of the transformed distance ratios,
# distances including landmarks that have been altered will appear at the end of the vector and distances
# that have been less altered by shpae change will appear first in the vector.
Ra3 <- order(rank(Ra, na.last = NA))

Name[Ra3]




# ------------------------------------------------------------------------------
# Examine which landmarks may have some influence on shape difference btween configurations
#   - Calculate the sum of diveregences to the median value for each landmark
# ------------------------------------------------------------------------------

FDM <- FM(gorf.dat_tmp) / FM(gorf.dat[,,2])

rownames(FDM) <- 1:8

FDM1 <- abs(FDM - median(FDM, na.rm = T))

round(apply(FDM1, 2, sum, na.rm = T), 2)



# -->
# landmarks of greates influence will have higher scores.  --> landmark 1



# ------------------------------------------------------------------------------
# Full review and inspect the Form Distance Matrix:  Scatter of raios between interlandmark distances
# ------------------------------------------------------------------------------

j <- cbind(rep(1:8, 8), as.numeric(FDM))


par(mfrow=c(1,1), mar = c(2,2,2,2))
plot(j[-which(is.na(FDM)),], ylim = c(0, max(FDM, na.rm = T)), cex = 0, xlab = "Landmark", ylab = "Form difference", font.lab = 2)

text(j[-which(is.na(FDM)),], label = gl(8,7), cex = 0.7)
abline(h = median(FDM, na.rm = T), lty = 3)



# -->
# Only landmark 1 seems to really influence the difference between both configurations
# (this is expected from the result of the generalized resistant-fit procedure)
