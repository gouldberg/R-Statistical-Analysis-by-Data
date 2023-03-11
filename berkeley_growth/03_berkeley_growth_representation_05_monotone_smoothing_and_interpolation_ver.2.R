setwd("//media//kswada//MyFiles//R//berkeley_growth")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  berkeley growth
#   - The data from the Berkeley Growth Study (Tuddenham and Snyder, 1954).
# ------------------------------------------------------------------------------

data("growth", package = "fda")

str(growth)



# ----------
# 39 boys and 31 points
# a 31 by 39 numeric matrix giging the heights in centimeters of 39 boys at 31 ages
dim(growth$hgtm)


# 54 girls and 31 points
# a 31 by 54 numeric matrix giging the heights in centimeters of 54 girls at 31 ages
dim(growth$hgtf)



# ----------
head(growth$hgtm)


head(growth$hgtf)



# ----------
growth$age


# -->
# The ages are not equally spaced;
# There are four measurements while the child is one year old, annual measurements from two to eight years,
# followed by heights measured biannually



# ------------------------------------------------------------------------------
# Monotone smoothing and interpolations
# ------------------------------------------------------------------------------

age     = growth$age


# ----------
# Monotone smooth
# A useful rule to remember is to fix the order of the spline basis to be at least two higher than the highest order derivative to be used.
# By this rule, a cubic spline basis is a good choice as long as you do not need to look at any of its derivatives.

# Here B-spline of order 6 = quintic polynomials so the acceleration will be cubic (= 3 + 2 + 1)
gr.basis = create.bspline.basis(norder = 6, breaks = growth$age)
plot(gr.basis)



# ----------
# Consider only the first 10 girls
children = 1:10
ncasef   = length(children)



# ----------
# matrix of starting values for coefficients
# nbasis = nbreaks + norder - 2 = 31 + 6 - 2 = 35
gr.basis$nbasis

cvecf           = matrix(0, gr.basis$nbasis, ncasef)

dimnames(cvecf) = list(gr.basis$names,
                       dimnames(growth$hgtf)[[2]][children])


cvecf


# ----------
# Create an initial functional data object with these coefficients
gr.fd0  = fd(coef = cvecf, basisobj = gr.basis)
gr.fd0



# ----------
# Create an initial functional parameter object
# Lfdobj = 3 to penalize the rate of change of acceleration

gr.Lfd    = 3

# a nonnegative real number specifying the amount of smoothing to be applied to the estimated functional parameter
gr.lambda = 10^(-1.5)

gr.fdPar  = fdPar(gr.fd0, Lfdobj=gr.Lfd, lambda=gr.lambda)




# ----------
#  Monotonically smooth the female data 
hgtfmonfd   = with(growth, smooth.monotone(age, hgtf[,children], gr.fdPar)) 

hgtfmonfd$beta

hgtfmonfd$yhatfd



# ----------
#  define the range of the ages and set up a fine mesh of ages
age.rng = range(age)
( agefine = seq(age.rng[1], age.rng[2], length=501) )

hgtf.vec = predict(hgtfmonfd$yhatfd, agefine)



# ------------------------------------------------------------------------------
# plot smoothed curves
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

with(growth, matplot(age, hgtf[, children], pch=1:10, cex = 0.7, col=1:10,
                     xlab='Age (years)', ylab='Height (cm)',
                     ylim=c(60, 190) , main = "10 samples of girls"))


matlines(agefine, hgtf.vec, lty=1, col=1)

