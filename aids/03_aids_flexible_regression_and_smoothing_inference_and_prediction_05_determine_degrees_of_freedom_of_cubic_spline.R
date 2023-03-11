setwd("//media//kswada//MyFiles//R//aids")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  aids
# ------------------------------------------------------------------------------
data("aids", package = "gamlss.data")


str(aids)

car::some(aids)



# ------------------------------------------------------------------------------
# Determine a reasonable value for the missing defrees of freedom of the cubic spline function cs()
#   - Note that in pb() the smoothing parameter and therefore the degrees of freedom are estimated automatically using a local maximum
#     likelihood procedure,
#     while here the estimation of effective degrees of freedom is done globally.
#   - Models with different degrees of freedom can be fitted and their GAIC plotted against the degrees of freedom.
#     This process can be automated using prof.term()
# ------------------------------------------------------------------------------

mod1 <- quote(gamlss(y ~ cs(x, df = this) + qrt, data = aids, family = NBI))

prof.term(mod1, min = 1, max = 15, step = 1, criterion = "GAIC", penalty = 2.5)


# -->
# The profile GAIC (k = 2.5) plot suggests support for effective degrees of freedom for the cubic smoothing function in x of 8.24
# (in addition to the constant and linear term).

# Alternative penalties values could be used, e.g., k = 2, 3, 4 or log n


