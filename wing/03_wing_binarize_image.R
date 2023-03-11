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

y <- as(M, "pixmapGrey")

y


str(y)



# ----------
plot(y)



# ------------------------------------------------------------------------------
# Binarize image using a homemade threshold filter
# ------------------------------------------------------------------------------

par(mar = c(1, 3, 2, 1))

Y <- y@grey


# ----------
graphics.of
layout(matrix(1:4, 2, 2))

plot(y, main = "Gray-scale image")

y@grey[which(Y >= 0.3)] <- 1
y@grey[which(Y < 0.3)] <- 0
plot(y, main = "Bin image, threshold = 0.3")

y@grey[which(Y >= 0.5)] <- 1
y@grey[which(Y < 0.5)] <- 0
plot(y, main = "Bin image, threshold = 0.5")

y@grey[which(Y >= 0.7)] <- 1
y@grey[which(Y < 0.7)] <- 0
plot(y, main = "Bin image, threshold = 0.7")


length(y@grey[which(Y < 0.9)])


