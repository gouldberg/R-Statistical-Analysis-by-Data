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
# Generalized Procrustes analysis (GPA)
#   - The basic idea is to define a mean shape as a reference for allowing some assessment of the variability of the shape sample.
#     The mean shape must be a parameter of central tendency of the shape distribution.
#     The generalized Procrustes analysis (GPA) is a method that searches the average shape whose sum of pairwise squared coordinates
#     with other rotated configurations is minimized.
# ------------------------------------------------------------------------------


# Generalized Procrustes analysis
fgpa_obj <- fgpa(gorf.dat)

fgpa_obj



# ----------
idx <- 10


graphics.off()

par(mfrow=c(2,2))
plot(gorf.dat[,,idx], axes = F, xlab = "", ylab = "", main = "object config")
polygon(gorf.dat[,,idx])

plot(fgpa_obj$mshape, axes = F, xlab = "", ylab = "", main = "mean shape config")
polygon(fgpa_obj$mshape)

plot(fgpa_obj$rotated[,,idx], axes = F, xlab = "", ylab = "", main = "superimposed config", pch = 10, col = "red")
polygon(fgpa_obj$rotated[,,idx])



# ----------
centsiz(fgpa(gorf.dat)$mshape)[[1]]

centsiz(mshape(fgpa(gorf.dat)$rotated))[[1]]


# -->
# Notice that the mean of rotated configurations has a relatively smaller size than the mean shape
# used as a reference (the mean shape has unit size)
# This is because the Procrustes adjustment considers both size and rotation for minimizing interlandmark distances.



# ------------------------------------------------------------------------------
# Generalized Procrustes analysis (GPA):
# Rphlf and Slice algorithm to ensure that the sum of centroid sizes for all configurations reaches the number of configurations
# ------------------------------------------------------------------------------

# Generalized Procrustes analysis
fgpa2_obj <- fgpa2(gorf.dat)

fgpa2_obj



# ----------
idx <- 10


graphics.off()

par(mfrow=c(2,2))
plot(gorf.dat[,,idx], axes = F, xlab = "", ylab = "", main = "object config")
polygon(gorf.dat[,,idx])

plot(fgpa2_obj$mshape, axes = F, xlab = "", ylab = "", main = "mean shape config")
polygon(fgpa2_obj$mshape)

plot(fgpa2_obj$rotated[,,idx], axes = F, xlab = "", ylab = "", main = "superimposed config", pch = 10, col = "red")
polygon(fgpa2_obj$rotated[,,idx])



# ----------
centsiz(fgpa2(gorf.dat)$mshape)[[1]]

centsiz(mshape(fgpa2(gorf.dat)$rotated))[[1]]



# ------------------------------------------------------------------------------
# Generalized Procrustes analysis (GPA) by shapes::procGPA
# ------------------------------------------------------------------------------

# Generalized Procrustes analysis
gpa_obj <- shapes::procGPA(gorf.dat)

gpa_obj



# ----------
idx <- 10


graphics.off()

par(mfrow=c(2,2))
plot(gorf.dat[,,idx], axes = F, xlab = "", ylab = "", main = "object config")
polygon(gorf.dat[,,idx])

plot(gpa_obj$mshape, axes = F, xlab = "", ylab = "", main = "mean shape config")
polygon(gpa_obj$mshape)

plot(gpa_obj$rotated[,,idx], axes = F, xlab = "", ylab = "", main = "superimposed config", pch = 10, col = "red")
polygon(gpa_obj$rotated[,,idx])



# ----------
centsiz(shapes::procGPA(gorf.dat)$mshape)[[1]]

centsiz(mshape(procGPA(gorf.dat)$rotated))[[1]]



# ----------
par(mfrow = c(1,1))
plotshapes(gpa_obj$rotated, joinline = c(1,6,7,8,2,3,4,5,1))



# ------------------------------------------------------------------------------
# Partial generalized Procrustes analysis (GPA) by shapes::procGPA
#   - it does not rescale the size of centered preshape configurations of optimizing the superimposition fit.
# ------------------------------------------------------------------------------

# Generalized Procrustes analysis
pgpa_obj <- pgpa(gorf.dat)

pgpa_obj



# ----------
idx <- 10


graphics.off()

par(mfrow=c(2,2))
plot(gorf.dat[,,idx], axes = F, xlab = "", ylab = "", main = "object config")
polygon(gorf.dat[,,idx])

plot(pgpa_obj$mshape, axes = F, xlab = "", ylab = "", main = "mean shape config partial GPA")
polygon(pgpa_obj$mshape)

plot(pgpa_obj$rotated[,,idx], axes = F, xlab = "", ylab = "", main = "superimposed config partial GPA", pch = 10, col = "red")
polygon(pgpa_obj$rotated[,,idx])

plot(gpa_obj$mshape, axes = F, xlab = "", ylab = "", main = "mean shape config full GPA")
polygon(gpa_obj$mshape)



# -->
# similar result by full GPA and partial GPA 



# ----------
centsiz(pgpa(gorf.dat)$mshape)[[1]]

centsiz(mshape(pgpa(gorf.dat)$rotated))[[1]]


