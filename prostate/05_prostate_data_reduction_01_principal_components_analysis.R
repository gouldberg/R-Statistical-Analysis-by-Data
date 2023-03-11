setwd("//media//kswada//MyFiles//R//prostate")

packages <- c("dplyr", "Hmisc", "rms", "lattice", "ggplot2")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Function for Scree Plot
# ------------------------------------------------------------------------------

# Variance explaned by PCs: Scree Plot  (showing x: PCs  y: individual variances explained  text:  cumulative)

addscree <- function(x, npcs=min(10, length(x$sdev)), plotv=FALSE, col=1, offset=0.8, adj=0, pr=FALSE, lwd=1, lty=1){
  vars <- x$sdev^2
  cumv <- cumsum(vars) / sum(vars)
  if(pr) print(cumv)
  text(1:npcs, vars[1:npcs] + offset * par('cxy')[2], as.character(round(cumv[1:npcs], 2)), srt=45, adj=adj, cex=0.65, xpd=NA, col=col)
  if(plotv) lines(1:npcs, vars[1:npcs], type="b", col=col, lwd=lwd, lty=lty)
}



# ------------------------------------------------------------------------------
# Dimension Reduction by Principal Components Analysis:  imputed raw data and imputed transformed data
# ------------------------------------------------------------------------------
Ekg <- model.matrix(~ ekg, data = imp)[,-1]

head(Ekg)

pfn <- data$pfn



# ----------
# NOTE:  categorical variables are used as numeric value
prin.raw <- princomp(~ sz + sg + ap + sbp + dbp + age + wt + hg + Ekg + pfn + bm + hx, cor = TRUE, data = imp)
prin.trans <- princomp(~ sz + sg + ap + sbp + dbp + age + wt + hg + ekg + pf + bm + hx, cor = TRUE, data = data_trans)

summary(prin.raw)
summary(prin.trans)


# ----------
plot(prin.raw, type="lines", main="", ylim=c(0,3))
addscree(prin.raw)
addscree(prin.trans, plotv=TRUE, col="red", offset=-0.8, adj=1, lwd=2, lty=2)



# -->
# Imputed raw data:
# It requires 10 of the 16 possible components to explain > 0.8 of the variance
# and the first 5 components explain 0.49 of the variance of the system.
# Two of the 16 dimensions are almost totally redundant.  (you can see very flat at comp 5 and comp 6)


# Transfomed data:
# We have only 12 degrees of freedom for the 12 predictors.
# It requires only 9 of the 12 possible components to explain > 0.8 of the variance
# and the first 5 components explain 0.66 of the variance of the system.

