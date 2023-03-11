setwd("//media//kswada//MyFiles//R//texaselectr")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Texas Electr
# ------------------------------------------------------------------------------

data("TexasElectr", package = "pder")


str(TexasElectr)


dim(TexasElectr)


car::some(TexasElectr)



# ------------------------------------------------------------------------------
# Create variables for Seemingly Unrelated Regression (SUR) model
#
#   - The behavior of a producer is described by a minimum cost equation along with equations of factor demand.
#     In this case, there are two advantages in considering the whole system of equations:
#        1. the errors of the different equations for an observation may be crrelated.
#        2. economic theory may impose restrictions on different coefficients of the system
#
#   - The cost function returns the minimum cost of production C for a given vector of prices of the F production factors p = (p1, p1, p2, ...)
#     and the level of output q.
#     The minimum cost fnction is C(p, q)
#        1. homogeneous of degree 1 with respect to the factor prices C(lambda * p, q) = lambda * C(p, q)
#        2. demand functions ofor production factors are the derivatives of the minimum cost function with respect to factor prices,
#           i.e., the gradient of the cost function
#        3. THe Hessian matrix of the cost function is symmetric
# ------------------------------------------------------------------------------

# compute prices in logarithms, dividing them by their sample mean, and also dividing them by one of the prices, here fuel price.
# beta-q is then the cost elasticity with respect to the production level at the sample mean
# beta-i the share of factor i in the cost at the sample mean.
TexasElectr <- mutate(TexasElectr,
                      pf = log(pfuel / mean(pfuel)),
                      pl = log(plab / mean(plab)) - pf,
                      pk = log(pcap / mean(pcap)) - pf)


# ----------
TexasElectr <- mutate(TexasElectr, q = log(output / mean(output)))

TexasElectr <- mutate(TexasElectr,
                      C = expfuel + explab + expcap,
                      sl = explab / C,
                      sk = expcap / C,
                      C = log(C / mean(C)) - pf)


# ----------
# Compute the squares and the interaction terms for the variables
TexasElectr <- mutate(TexasElectr,
                      pll = 1/2 * pl ^ 2,
                      plk = pl * pk,
                      pkk = 1/2 * pk ^ 2,
                      qq = 1/2 * q ^ 2)



# ------------------------------------------------------------------------------
# Define 3 equations of the system, one for total cast and the other two for factor shares
# ------------------------------------------------------------------------------

cost <- C ~ pl + pk + q + pll + plk + pkk + qq

shlab <- sl ~ pl + pk

shcap <- sk ~ pl + pk








