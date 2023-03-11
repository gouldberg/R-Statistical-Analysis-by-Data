setwd("//media//kswada//MyFiles//R//handwrit")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  handwrit
#   - handwriting of "fda" of 20 smaples
# ------------------------------------------------------------------------------
data("handwrit", package = "fda")

str(handwrit)



# ------------------------------------------------------------------------------
# Multivariate principal components analysis
# ------------------------------------------------------------------------------
# use 3 harmonics
nharm = 3

fdapcaList = pca.fd(fdafd, nharm)



# ----------
# plot unrotated eigenfunctions
plot.pca.fd(fdapcaList, expand=.2)



# ------------------------------------------------------------------------------
# Perform a VARIMAX rotation of these eigenfunctions
# ------------------------------------------------------------------------------
fdarotpcaList = varmx.pca.fd(fdapcaList)

sum(fdarotpcaList$varprop)

plot.pca.fd(fdarotpcaList, expand=.2)



# ------------------------------------------------------------------------------
# plot the log eigenvalues
# ------------------------------------------------------------------------------
# plot the log eigenvalues up to j = 12 with the least-squares linear trend in the eigenvalue with indices 4 to 12.
neig = 12

x = matrix(1, neig - nharm, 2)

x[,2] = (nharm + 1):neig


fdaeig = fdapcaList$values

y = as.matrix(log10(fdaeig[(nharm+1):neig]))

c = lsfit(x, y, int = FALSE)$coef



# ----------
op <- par(mfrow=c(1,1), cex=1.2)

plot(1:neig, log10(fdaeig[1:neig]), "b", xlab="Eigenvalue Number", ylab="Log10 Eigenvalue")

lines(1:neig, c[1] + c[2] * (1:neig), lty=2)

par(op)



# -->
# The first 3 log eigenvalues seem well above the linear trend in the next nine,
# suggesting that the leading three harmonics are important.

# Together they account for 62% of the variation in the scrips



# ------------------------------------------------------------------------------
# Evaluate the harmonics
# ------------------------------------------------------------------------------

fdameanfd  = mean(fdafd)

fdameanmat = eval.fd(fdatime, fdameanfd)



# ----------
# evaluate the harmonics
harmfd  = fdarotpcaList$harm

# harmmat = eval.fd(fdatime, harmfd)



# ----------
fdapointtime = seq(0, 2300, len = 201)

fdameanpoint = eval.fd(fdapointtime, fdameanfd)

harmpointmat = eval.fd(fdapointtime, harmfd)



fac = 0.1

harmplusmat = array(0, c(201, 3, 2))
harmminsmat = array(0, c(201, 3, 2))

for(j in 1:3){
  harmplusmat[,j,] = fdameanpoint[,1,] + fac * harmpointmat[,j,]
  harmminsmat[,j,] = fdameanpoint[,1,] - fac * harmpointmat[,j,]
}



# ------------------------------------------------------------------------------
# Plots two of the VARIMAX-rotated eigenfunctions as perturbations of the mean script.
# ------------------------------------------------------------------------------

par(mfrow=c(1,1))

j = 3

plot(fdameanmat[,1,1] - 0.035,  fdameanmat[,1,2], "l", lwd = 2,
     xlim = c(-0.075, 0.075), ylim = c(-0.04, 0.04), xlab = "",  ylab = "")

lines(harmplusmat[,j,1] - 0.035, harmplusmat[,j,2], lty=2)
lines(harmminsmat[,j,1] - 0.035, harmminsmat[,j,2], lty=2)


j = 2

lines(fdameanmat[,1,1] + 0.035, fdameanmat[,1,2], lty=1, lwd=2)
lines(harmplusmat[,j,1] + 0.035, harmplusmat[,j,2], lty=2)
lines(harmminsmat[,j,1] + 0.035, harmminsmat[,j,2], lty=2)



# -->
# The rotated harmonic on the left mostly captures variation in the lower loop of "f", and the harmonic on the right displays primarily variations
# in its upper loop. This suggests that variabilities in these two loops are independent of each other.
