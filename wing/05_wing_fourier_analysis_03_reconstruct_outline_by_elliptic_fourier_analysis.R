setwd("//media//kswada//MyFiles//R//wing")

packages <- c("dplyr", "pixmap")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

source(file = "//media//kswada//MyFiles//R_basics//functions_for_morphometrics.R")



# ------------------------------------------------------------------------------
# data:  wing
# ------------------------------------------------------------------------------

M <- read.pnm("//media//kswada//MyFiles//references//MorphometricsWithR//e-supplement//wing.ppm")

M


str(M)


# -----------
plot(M)



# ------------------------------------------------------------------------------
# Smooth the outline to avoid imperfection due to automatic digitization
#   - We have to determine the number i of smoothing iterations.
#     Haines and Crampton recommend having ntot / p < sqrt(i / 2) where ntot is the total number of pixels of the outline
#     and p is the number of points to be equally sampled on the outline
# ------------------------------------------------------------------------------

M0 <- as.matrix(data.frame(X = X, Y = Y))


# ----------
( numb <- 2 * (length(X) / 64) ^ 2 )

par(mfrow=c(1,1), mar=c(2,2,2,2))

M2 <- smoothout(cbind(X, Y), numb + 1)

M2


plot(M2)



# ------------------------------------------------------------------------------
# Reconstruct of outlines by elliptic Fourier analysis
# ------------------------------------------------------------------------------

M_obj <- M2
# M_obj <- M0


ef1 <- efourier(M_obj)

ef1



# ----------
layout(matrix((1:9), 3, 3))

par(mar=c(2,2,2,2))

for(i in 1:9){

  ief1 <- iefourier(an = ef1$an, bn = ef1$bn, cn = ef1$cn, dn = ef1$dn, k = i, n = 64, ao = ef1$ao, co = ef1$co)
  
  plot(M_obj, type = "l", asp = 1, frame = F, main = paste("Harmonics 0 to", i), col = "grey")
  
  polygon(M_obj, col = "grey", border = NA)
  
  lines(ief1$x, ief1$y, type = "l")
}


# -->
# gray shape:  original outline
# thin black outlines:  reconstructed outline

# as early as the sixth harmonic, the outline is nearly perfectly reconstructed.


