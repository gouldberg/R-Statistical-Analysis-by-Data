setwd("//media//kswada//MyFiles//R//wing")

packages <- c("dplyr", "pixmap")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wing
# ------------------------------------------------------------------------------

M <- read.pnm("//media//kswada//MyFiles//references//MorphometricsWithR//e-supplement//wing.ppm")

M


str(M)


# -----------
plot(M)



# ------------------------------------------------------------------------------
# Convert to gray-scale image
# ------------------------------------------------------------------------------

Mg <- as(M, "pixmapGrey")

Mg


str(Mg)



# ----------
plot(Mg)



# ------------------------------------------------------------------------------
# Convert to gray-scale image and contour plot by image()
#   - displaying images with various gray-scale depths, or by using contour plot
# ------------------------------------------------------------------------------

graphics.off()

layout(matrix(1:4, 2, 2))

image(t(Mg@grey[dim(Mg@grey)[1]:1,]), col = gray(0:255/255), asp = 9/16, axes = F, main = "Gray-scale: 8-bits")

image(t(Mg@grey[dim(Mg@grey)[1]:1,]), col = gray(0:3/3), asp = 9/16, axes = F, main = "Gray-scale: 2-bits")

image(t(Mg@grey[dim(Mg@grey)[1]:1,]), col = gray(0:1/1), asp = 9/16, axes = F, main = "Monochrome: 1-bit")

contour(t(Mg@grey[dim(Mg@grey)[1]:1,]), levels = 0:10/10, asp = 9/16, axes = F, main = "Contour plot", drawlabels = F)


