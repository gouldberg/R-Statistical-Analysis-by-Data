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
# The varComb Variance Structure
#   - var(eij) = sigma^2 * e^(2*delta*DLMij)
#   - We can allow for both and increase in residual spread for larger DML values as well as a different spread per month.
#     This is a combination of varIdent and varExp.
# ------------------------------------------------------------------------------

vf8 <- varComb(varIdent(form = ~ 1 | fMONTH), varExp(form = ~ DML) )

M.gls8 <- gls(Testisweight ~ DML * fMONTH, weights = vf8, data = Squid)


summary(M.gls8)



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
E2 <- resid(M.gls8, type = "normalized")
coplot(E2 ~ DML | fMONTH, data = Squid, ylab = "Normalised residuals")
