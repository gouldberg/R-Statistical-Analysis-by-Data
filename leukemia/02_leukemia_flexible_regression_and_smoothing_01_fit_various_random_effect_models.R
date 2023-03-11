setwd("//media//kswada//MyFiles//R//leukemia")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Leukemia
# ------------------------------------------------------------------------------

data("Leukemia", package = "gamlss.data")


str(Leukemia)

car::some(Leukemia)



# ------------------------------------------------------------------------------
# Fit six different models for the mu parameter of response height with random intercept
# ------------------------------------------------------------------------------

g1 <- gamlss(height ~ treatment + age + re(random = ~1 | case), data = Leukemia, trace = FALSE)


g2 <- gamlss(height ~ treatment + pb(age) + re(random = ~1 | case), data = Leukemia, trace = FALSE)


g3 <- gamlss(height ~ treatment + pb(age) + re(random = ~age | case), data = Leukemia, n.cyc = 200, trace = FALSE)


g4 <- gamlss(height ~ treatment + pvc(age, by = treatment) + re(random = ~age | case), data = Leukemia, trace = FALSE)


# smooth age interacts with subjects
# it takes a long time to converge
Z.case <- with(Leukemia, getZmatrix(age, inter =10))
Z.block <- list(case = pdSymm(~age), case = pdIdent(~Z.case-1))

g5 <- gamlss(height ~ treatment + re(fixed = ~Z.case, random = Z.block), data = Leukemia, bf.cyc = 1, c.crit = 0.1, d.tol = Inf, method = mixed(3, 10), trace = FALSE)


# smooth age interacts with both treatment and subjects
# it takes a long time to fit and convergence is difficult
g6 <- gamlss(height ~ treatment + pvc(age, by = treatment) + re(fixed = ~Z.case, random = Z.block), data = Leukemia, bf.cyc = 1, c.crit = 0.1, gd.tol = Inf, method = mixed(3, 20), trace = FALSE)



# ------------------------------------------------------------------------------
# Check models
# ------------------------------------------------------------------------------

GAIC(g1, g2, g3, g4, g5, g6)

GAIC(g1, g2, g3, g4, g5, g6, k = log(nrow(Leukemia)))


# -->
# AIC selects model 55, which is rather complex, whereas the SBC selects the simper model g2.



# ----------
plot(g1)
plot(g2)
plot(g3)
plot(g4)
plot(g5)
plot(g6)



# ----------
library(lattice)

with(Leukemia, xyplot(fitted(g1) + fitted(g2) + fitted(g3) + fitted(g4) + fitted(g5) + fitted(g6) ~ age, groups = case,
                      type = "a", auto.key = list(space = "no", points = FALSE, lines = TRUE)))




# ----------
wp(g2)
wp(g5)


# -->
# Unfortunately both g2 and g5 have rather undesirable residuals

