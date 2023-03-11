setwd("//media//kswada//MyFiles//R//squid")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Squid
# ------------------------------------------------------------------------------
Squid <- read.table(file = "Squid.txt", header = TRUE)


str(Squid)


Squid$fMONTH = factor(Squid$MONTH)



# ------------------------------------------------------------------------------
# The varConstPower Variance Structure (constant plus power of the variance covariate)
#   - var(eij) ~ sigma-2 * (delta1 + | DML(ij) |^delta2)^2
#   - If delta1 = 1 and delta2 = 0, we are back to the linear regression model.
#   - According to Pinheiro and Bates (2000), this variance structure works better than the varExp if the variance convariate has values close to zero.
# ------------------------------------------------------------------------------

vf6 <- varConstPower(form = ~ DML)

M.gls6 <- gls(Testisweight ~ DML * fMONTH, weights = vf6, data = Squid)

summary(M.gls6)




# ----------
vf7 <- varConstPower(form =~ DML | fMONTH)

M.gls7 <- gls(Testisweight ~ DML * fMONTH, weights = vf7, data = Squid)

summary(M.gls7)




# ------------------------------------------------------------------------------
# plot observations and conditional plot of the residuals versus DLM
# ------------------------------------------------------------------------------

# colours observations of the same month.
plot(M.lm, which = c(1), col = Squid$MONTH, add.smooth = FALSE, caption = "")


# -->
# The graph does not show any clear grouping.



# ----------
# coplot of the residuals versus DLM, conditional on month
# the lower left panel corresponds to month 1, the lower right to month 4, and the upper right to month 12.
# You should use standardised residuals instead of the ordianry residuals for the model varidation.
# These are obtained by calculating the observed minus the fitted values and then dividing by the square root of the variance.
library(lattice)
E2 <- resid(M.gls6, type = "normalized")
coplot(E2 ~ DML | fMONTH, data = Squid, ylab = "Normalised residuals")

E2 <- resid(M.gls7, type = "normalized")
coplot(E2 ~ DML | fMONTH, data = Squid, ylab = "Normalised residuals")
