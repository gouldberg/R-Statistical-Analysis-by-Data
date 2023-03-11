setwd("//media//kswada//MyFiles//R//swedish_mortality")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Swedish Mortality
# ------------------------------------------------------------------------------



# ------------------------------------------------------------------------------
# Functional linear model
#   - Model:  x(i+1)(t) = beta0(t) + integral( beta1(s, t) * x(i)(t) * ds ) + e(i)(t)
#     For any year from 1752 to 1894, we model the log hazard function for that year using as the functional covariate the log hazard curve
#     ofr the preceding year.
# ------------------------------------------------------------------------------

# Set up for the list of regression coefficient fdPar objects

nbasis     = 23

SwedeRng   = c(0, 80)

SwedeBetaBasis = create.bspline.basis(SwedeRung, nbasis)

SwedeBeta0Par = fdPar(SwedeBetaBasis, 2, 1e-5)

SwedeBeta1fd  = bifd(matrix(0,23,23), SwedeBetaBasis, SwedeBetaBasis)

SwedeBeta1Par = bifdPar(SwedeBeta1fd, 2, 2, 1e3, 1e3)

SwedeBetaList = list(SwedeBeta0Par, SwedeBeta1Par)



# ----------
# Define the dependent and independent variable objects
# Response Curve
NextYear = SwedeLogHazfd[2:144]

# Covariate curves
LastYear = SwedeLogHazfd[1:143]



# ----------
Swede.linmod = linmod(NextYear, LastYear, SwedeBetaList)



Swede.ages = seq(0, 80, 2)

Swede.beta1mat = eval.bifd(Swede.ages, Swede.ages, Swede.linmod$beta1estbifd)

Swede.beta1mat



# ----------
persp(Swede.ages, Swede.ages, Swede.beta1mat, 
      xlab="age", ylab="age",zlab="beta(s,t)",
      cex.lab=1.5, cex.axis=1.5)


# -->
# The ridge in beta1(s,t) is one year off the diagonal
# The esimated intercept function beta0 ranged over values four orders of magnitude smaller than the response functions,
# and can therefore be considered to be essentailly zero.

# The strong ridge one year off the diagonal, namely beta1(s-1, s), indicates that mortality at any age is most strongly related to mortality
# at the previous year for that age less one.

# In other words, mortality is most strongly determined by age-specific factors like infectious diseases in infancy,
# accidents and violent death in early adulthood,
# and aging late in life.
# The height of the surface declines to near zero for large differences between s and t for this reason as well.
