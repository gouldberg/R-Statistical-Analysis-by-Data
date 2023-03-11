# setwd("//media//kswada//MyFiles//R//squid")
setwd("//media//kswada//MyFiles//R//Generalized_linear_model_and_heterogeneity//squid")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Squid
# ------------------------------------------------------------------------------

Squid <- read.table(file = "Squid.txt", header = TRUE)


str(Squid)


dim(Squid)


car::some(Squid)



# ----------
Squid$fMONTH = factor(Squid$MONTH)



# ------------------------------------------------------------------------------
# Linear Regression as starting point
#   - Previous work on the related species Loligo vulgaris showed graphically that maturity was a function of both size and season,
#     and that size-at-maturity differed between seasons (Raya et al., 1999)
# ------------------------------------------------------------------------------

M1 <- lm(Testisweight ~ DML * fMONTH, data = Squid)

summary(M1)




# ------------------------------------------------------------------------------
# simple model validation
# ------------------------------------------------------------------------------

op <- par(mfrow = c(2,2), mar=c(4,4,2,2))

plot(M1, which = c(1), col = 1, add.smooth = T, caption = "")
plot(Squid$fMONTH, resid(M1), xlab = "Month", ylab = "Residuals")
plot(Squid$DML, resid(M1), xlab = "DML", ylab = "Residuals")
lines(lowess(Squid$DML, resid(M1)), col = "red")

par(op)



# -->
# Note that there is a clear violation of homogeneity.
# The larger the length (DML), the larger the variation.
# So instead of assuming that the residuals have variance var(ei) = constant sigma^2, it might make more sense to assume that
# var(ei) increases when DML(i) increases.



# ------------------------------------------------------------------------------
# plot observations and conditional plot of the residuals versus DLM
# ------------------------------------------------------------------------------

# colours observations of the same month.
plot(M1, which = c(1), col = Squid$MONTH, add.smooth = FALSE, caption = "")


# -->
# The graph does not show any clear grouping.



# ----------
# coplot of the residuals versus DLM, conditional on month for the linear regresssion model.
# the lower left panel corresponds to month 1, the lower right to month 4, and the upper right to month 12.
library(lattice)
E <- resid(M1)
coplot(E ~ DML | fMONTH, data = Squid)


# -->
# The residual variation differs per month, but in some months (e.g., 3, 9, and 10) the residual spread also increases for larger DML values.
# So, both are influential: residual spread is influenced by both month and length

