setwd("//media//kswada//MyFiles//R//dbbmi")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  dbbmi
# ------------------------------------------------------------------------------
data("dbbmi", package = "gamlss.data")


str(dbbmi)

car::some(dbbmi)



# ------------------------------------------------------------------------------
# sample data
# ------------------------------------------------------------------------------


# Sample 1000 observations
IND <- sample.int(7040, 1000, replace = FALSE)

dbbmi1 <- dbbmi[IND,]



# ----------
graphics.off()
par(mfrow=c(1,2))

plot(bmi ~ age, data = dbbmi, pch = 15, cex = 0.5, col = gray(0.5))
title("(a)")

plot(bmi ~ age, data = dbbmi1, pch = 15, cex = 0.5, col = gray(0.5))
title("(b)")



# ------------------------------------------------------------------------------
# Estiamte the smoothing parameters and the power parameter "eta" using a global GAIC criterion with penalty k by find.hyper()
# ------------------------------------------------------------------------------

# It is prefereable to use cubic splines rather than P-splines for smoothing functions
mod <- quote(gamlss(bmi ~ cs(Tage, df = p[1]), sigma.fo = ~cs(Tage, df = p[2]), nu.fo = ~cs(Tage, df = p[3]), 
                    c.spar = c(-1.5, 2.5),
                    family = BCCGo, data = dbbmi1, control = gamlss.control(trace = FALSE, n.cyc = 1000, gd.tol = Inf)))


# for experiment: apply pb() instead of css()  --> !!! CONVERGENCE IS SLOW !!!
# mod_p <- quote(gamlss(bmi ~ pb(Tage, df = p[1]), sigma.fo = ~pb(Tage, df = p[2]), nu.fo = ~pb(Tage, df = p[3]), 
#                    c.spar = c(-1.5, 2.5),
#                    family = BCCGo, data = dbbmi1, control = gamlss.control(trace = FALSE, n.cyc = 1000, gd.tol = Inf)))


# Note that "Tage" is to be found
# other: used to optimize other non-parameters
# parameters:  the starting values in the search of the optimum hyper-parameters and/or non-linear parameters 
op <- find.hyper(model = mod_p, other = quote(Tage <- age ^ p[4]),
                 parameters = c(6, 2, 2, 0.3), lower = c(0.1, 0.1, 0.1, 0.005),
                 steps = c(0.1, 0.1, 0.1, 0.05), factr = 2e9, parscale = c(1, 1, 1, 1), k = 4)
 


# ----------
# The effective degrees of freedom for mu, sigma, nu that minimize GAIC(4), and power parameter
op$par


op$value


# -->
# Note that power transformation paramter p[4] is not sensible here ...
# Even if you chance the starting value for p[4], the value does not change much.



# ------------------------------------------------------------------------------
# Fit chosen model
# ------------------------------------------------------------------------------

Tage <- (dbbmi1$age) ^ (op$par[4])

m0D <- gamlss(bmi ~ cs(Tage, df = op$par[1]), sigma.fo = ~cs(Tage, df = op$par[2]), nu.fo = ~cs(Tage, df = op$par[3]), 
                    c.spar = c(-1.5, 2.5),
                    family = BCCGo, data = dbbmi1)


# ----------
# effective degrees of freedom
# (including 2 for the constant and linear terms)
m0D$mu.df

m0D$sigma.df

m0D$nu.df



# ----------
# compare models
GAIC(m0A, m0B, m0C, m0D, k = 4)


# -->
# Note that in the resulting table the extra one degree of freedom for estimating the power parameter is not included in df or GAIC











