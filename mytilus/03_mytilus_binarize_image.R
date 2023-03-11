setwd("//media//kswada//MyFiles//R//mytilus")

packages <- c("dplyr", "pixmap")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  mytilus
# ------------------------------------------------------------------------------

M <- read.pnm("//media//kswada//MyFiles//references//MorphometricsWithR//e-supplement//mytilus.ppm")

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

y@grey[which(Y >= 0.1)] <- 1
y@grey[which(Y < 0.1)] <- 0
plot(y, main = "Bin image, threshold = 0.1")

y@grey[which(Y >= 0.3)] <- 1
y@grey[which(Y < 0.3)] <- 0
plot(y, main = "Bin image, threshold = 0.3")

y@grey[which(Y >= 0.9)] <- 1
y@grey[which(Y < 0.9)] <- 0
plot(y, main = "Bin image, threshold = 0.9")


length(y@grey[which(Y < 0.9)])


