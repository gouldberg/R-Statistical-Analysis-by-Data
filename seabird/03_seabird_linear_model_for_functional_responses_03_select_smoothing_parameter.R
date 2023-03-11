setwd("//media//kswada//MyFiles//R//canadian_weather")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  seabird
# ------------------------------------------------------------------------------
data("seabird", package = "fda")

str(seabird)

car::some(seabird)



# ------------------------------------------------------------------------------
# Set up argument
# ------------------------------------------------------------------------------

# set up the arguments for fRegress:  insert each column in turn in matrix Z into the corresponding position in a list object xfdlist
p = 15
xfdlist = vector("list",p)

names(xfdlist) = c("const", "diet", birds)
betalist = xfdlist

for (j in 1:p) xfdlist[[j]] = Zmat[,j]



# ------------------------------------------------------------------------------
# Choose smoothing parameters
# ------------------------------------------------------------------------------

betabasis1 = create.bspline.basis(c(1, 20), 21, 4, yearCode)

betafdPari = fdPar(betabasis1, 2)


loglam = seq(-2, 4, 0.25)

SSE.CV = rep(0, length(loglam))


# IT TAKES TIME !!!:  3 minutes
# We use fRegressCV() to compute the cross-validated integrated squared error (LOOCV)

for(i in 1:length(loglam)){
  print(loglam[i])
  betafdPari$lambda = 10^loglam[i]
  betalisti = betalist
  
  for (j in 1:2) betalisti[[j]] = betafdPari
  CVi = fRegress.CV(birdfd3, xfdlist, betalisti, CVobs=1:26)
  
  SSE.CV[i] = CVi$SSE.CV
}



# ----------
SSE.CV

plot(loglam, SSE.CV, type='b', cex.lab=1.5, cex.axis=1.5, lwd=2,
     xlab='log smoothing parameter', ylab='cross validated sum of squares')


# -->
# Indicate a unique minimum with lambda approximately 10^0.5, although the discontinuities in the plot suggest that the cross-validated error sum
# of squares can be rather sensitive to non-smooth variation in the response funciton


