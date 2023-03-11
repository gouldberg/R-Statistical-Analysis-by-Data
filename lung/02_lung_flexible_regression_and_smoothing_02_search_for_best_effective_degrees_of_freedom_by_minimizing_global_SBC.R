setwd("//media//kswada//MyFiles//R//lung")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  lung
# ------------------------------------------------------------------------------

lung <- read.csv("data.csv", header = T)

str(lung)

car::some(lung)


# ----------
# only males data

dm <- subset(lung, sex == 1)

dim(dm)



# ----------
# apply a log transformation to height and age

dm <- transform(dm, la = log(age), lh = log(height))



# ------------------------------------------------------------------------------
# Search for best effective degrees of freedom by minimizing a global SBC by find.hyper()
# ------------------------------------------------------------------------------

# This should use cubic splines instead of penalized splines.

### THIS TAKES TIME !!!: 16 minutes ###

k1 <- log(nrow(dm))


mod <- quote(gamlss(fev ~ cs((log(height)), df = p[1]) + cs((log(age)), df = p[2]),
                    sigma.fo = ~cs((log(height)), df = p[3]) + cs((log(age)), df = p[4]),
                    nu.fo = ~1, tau.fo = ~1,
                    family = BCTo, data = dm, control = gamlss.control(trace = FALSE, n.cyc = 100)))


best <- find.hyper(model = mod, par = c(6, 6, 3, 3), lower = c(0.01, 0.01, 0.01, 0.01), steps = c(0.1, 0.1, 0.1, 0.1), k = k1)


best
edfAll(m3)


# -->
# The resulting effective degrees of freedom are very similar to model m3.



