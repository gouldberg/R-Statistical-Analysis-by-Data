setwd("//media//kswada//MyFiles//R//prostate2")

packages <- c("dplyr", "gamair", "mgcv")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  prostate
#   - Contains the data for protein mass spectral (intensity) for blood samples with 3 type
#        - a healthy prostate gland (1:healthy), an enlarged gland (2:enlarged) and prostate cancer (3:cancerous)
#   - Blood testing being much less invasive than biopsy, it is desirable to be able to predict prostate condition from the spectra.
#   - intensity:  a 624 * 264 matrix, with one spectrum per row.
#   - MZ:  the protein masses at which the spectra were measured
# ------------------------------------------------------------------------------

data(prostate, package = "gamair")


str(prostate)


car::some(prostate)


prostate$intensity



# ------------------------------------------------------------------------------
# basic analysis
# ------------------------------------------------------------------------------

# Example protein mass spectra for 3 subjects with healthy, enlarged, and cancerous prostate gland.
# The spectra are vertically shifted for visibility.

graphics.off()
par(mfrow=c(1,3))

idx1 <- sample(which(prostate$type == 1), size = 3, replace = FALSE)
idx2 <- sample(which(prostate$type == 2), size = 3, replace = FALSE)
idx3 <- sample(which(prostate$type == 3), size = 3, replace = FALSE)

plot(prostate$intensity[idx1[1],], type = "l", col = "black", lty = 1, main = paste0("type 1: healthy ", paste0(idx1, collapse = "/")), ylim = c(-5, 40))
lines(prostate$intensity[idx1[2],] + 5, col = "blue", lty = 2)
lines(prostate$intensity[idx1[3],] + 10, col = "red", lty = 3)

plot(prostate$intensity[idx2[1],], type = "l", col = "black", lty = 1, main = paste0("type 2: enlarged ", paste0(idx1, collapse = "/")), ylim = c(-5, 40))
lines(prostate$intensity[idx2[2],] + 5, col = "blue", lty = 2)
lines(prostate$intensity[idx2[3],] + 10, col = "red", lty = 3)

plot(prostate$intensity[idx3[1],], type = "l", col = "black", lty = 1, main = paste0("type 3: cancerous ", paste0(idx1, collapse = "/")), ylim = c(-5, 40))
lines(prostate$intensity[idx3[2],] + 5, col = "blue", lty = 2)
lines(prostate$intensity[idx3[3],] + 10, col = "red", lty = 3)


