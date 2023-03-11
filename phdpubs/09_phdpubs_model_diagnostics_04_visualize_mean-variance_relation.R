setwd("//media//kswada//MyFiles//R//phdpubs")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PhdPubs data
# ------------------------------------------------------------------------------

data("PhdPubs", package = "vcdExtra")


dim(PhdPubs)

str(PhdPubs)


car::some(PhdPubs)



# ----------
PhdPubs <- within(PhdPubs, {
  female <- factor(female)
  married <- factor(married)
})



# ------------------------------------------------------------------------------
# Visualize Mean-Variance Relation
# ------------------------------------------------------------------------------

fit.pois <- fitted(modp, type="response")

fit.qpois <- fitted(mod.qpois, type="response")

fit.nbin <- fitted(mod.nbin, type="response")

fit.nbin2 <- fitted(mod.nbin2, type="response")



# ----------
cutq <- function(x, q = 10){
  quantile <- cut(x, breaks = quantile(x, probs = (0:q)/q), include.lowest = TRUE, labels = 1:q)
  quantile
}


( group <- cutq(fit.nbin, q = 20) )

( qdat <- aggregate(PhdPubs$articles, list(group), FUN = function(x) c(mean = mean(x), var = var(x))) )

( qdat <- data.frame(qdat$x) )

colnames(qdat) <- c("mean", "var")

( qdat <- qdat %>% arrange(mean) )



# ----------
qdat$qvar <- summary(mod.qpois)$dispersion * qdat$mean

qdat$nbvar <- qdat$mean + (qdat$mean^2) / nbin$theta



# ----------
par(mfrow = c(1,1))

with(qdat, {
  plot(var ~ mean, xlab = "Mean number of articles", ylab = "Variance", pch = 16, cex = 1.2, cex.lab = 1.2)
  abline(h = mean(PhdPubs$articles), col = gray(.40), lty = "dotted")
  lines(mean, qvar, col = "red", lwd = 2)
  lines(mean, nbvar, col = "blue", lwd = 2)
  lines(lowess(mean, var), lwd = 2, lty = "dashed")
  text(3, mean(PhdPubs$articles), "Posisson", col = gray(.40))
  text(3, 6.7, "negbin", col = "blue")
  text(3, 8.5, "lowess")
})



# -->
# Points show the observed means and variances for 20 quantile groups based on the fitted values in the negative-binomial model.

# The variances implied by the quasi-Poisson and negative-binomial models are in reasonable accord with the data and with each other up to a mean of about 2.5.
# They diverge substantially at the upper end, for the 20-30% of the most productive candicdates, where the quadratic variance function of the negative binomial  provides a better fit


