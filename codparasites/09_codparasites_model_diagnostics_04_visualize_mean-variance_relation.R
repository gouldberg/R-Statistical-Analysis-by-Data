setwd("//media//kswada//MyFiles//R//codparasites")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Cod Parasites
# ------------------------------------------------------------------------------

data("CodParasites", package = "countreg")


dim(CodParasites)

str(CodParasites)


car::some(CodParasites)



# ----------
CodParasites$prevalence <- ifelse(CodParasites$intensity == 0, "no", "yes")



# ------------------------------------------------------------------------------
# Visualize Mean-Variance Relation
# ------------------------------------------------------------------------------

fit.pois <- fitted(modp, type="response")

fit.qpois <- fitted(mod.qpois, type="response")

fit.nbin <- fitted(mod.nbin, type="response")



# ----------
cutq <- function(x, q = 10){
  quantile <- cut(x, breaks = quantile(x, probs = (0:q)/q), include.lowest = TRUE, labels = 1:q)
  quantile
}


( group <- cutq(fit.nbin, q = 20) )

( qdat <- aggregate(na.omit(CodParasites)$intensity, list(group), FUN = function(x) c(mean = mean(x), var = var(x))) )

( qdat <- data.frame(qdat$x) )

colnames(qdat) <- c("mean", "var")

( qdat <- qdat %>% arrange(mean) )



# ----------
qdat$qvar <- summary(mod.qpois)$dispersion * qdat$mean

qdat$nbvar <- qdat$mean + (qdat$mean^2) / nbin$theta



# ----------
par(mfrow = c(1,1))

with(qdat, {
  plot(var ~ mean, xlab = "Mean Intensity", ylab = "Variance", pch = 16, cex = 1.2, cex.lab = 1.2)
  abline(h = mean(na.omit(CodParasites)$intensity), col = gray(.40), lty = "dotted")
  lines(mean, qvar, col = "red", lwd = 2)
  lines(mean, nbvar, col = "blue", lwd = 2)
  lines(lowess(mean, var), lwd = 2, lty = "dashed")
  text(25, var(na.omit(CodParasites)$intensity), "Posisson", col = gray(.40))
  text(25, 2000, "negbin", col = "blue")
  text(25, 1000, "lowess")
})



# -->
# Points show the observed means and variances for 20 quantile groups based on the fitted values in the negative-binomial model.

# The variances implied by the quasi-Poisson and negative-binomial models are in reasonable accord with the data and with each other up to a mean of about 5.
# They diverge substantially at the upper end

