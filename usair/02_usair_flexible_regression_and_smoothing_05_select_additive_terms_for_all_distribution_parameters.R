setwd("//media//kswada//MyFiles//R//usair")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  usair
# ------------------------------------------------------------------------------
data("usair", package = "gamlss.data")


str(usair)

car::some(usair)



# ------------------------------------------------------------------------------
# Selecting additive terms uisng the GAIC for all distribution parameters
# Strategy A:
#    - 1. Use a forward GAIC selection procedure to select an appropriate model fro mu, with sigma, nu, and tau fitted as constants
#    - 2. mu obtained in (1) (= mu(1)) + nu and tau constant --> forward selection to obtain sigma(2)
#    - 3. mu(1) + sigma(2) + tau constant --> forward selection to obtain nu(3)
#    - 4. mu(1) + sigma(2) + nu(3) --> forward selection to obtain tau(4)
#    - 5. mu(1) + sigma(2) + tau(4) --> backward selection to obtain nu from nu(3)
#    - 6. mu(1) + tau(4) + nu(5) --> backward selection to obtain sigma from sigma(2)
#    - 7. sigma(6) + nu(5) + tau(4) --> backward selection to obtain mu from mu(1)
# ------------------------------------------------------------------------------

m1 <- gamlss(y ~ 1, data = usair, family = GA, n.cyc = 50)


m2 <- stepGAICAll.A(m1, scope = list(lower = ~1, upper = ~x1 + x2 + x3 + x4 + x5 + x6), k = log(41))


m2


# -->
# Since here distribution is specified by "GA", mu and sigma are selected.
# The terms x1, x2, x3, x4 and x5 are selected fro the model for mu while x1 and x5 are selected for sigma.



# ------------------------------------------------------------------------------
# Selecting additive terms uisng the GAIC for all distribution parameters
# Strategy B:  forces all the distribution parameters to have the same terms
# ------------------------------------------------------------------------------


m3 <- stepGAICAll.B(m1, scope = list(lower = ~1, upper = ~x1 + x2 + x3 + x4 + x5 + x6), k = log(41))


m3


# -->
# The variables x1, x2, and x4 were selected (for both mu and sigma)

