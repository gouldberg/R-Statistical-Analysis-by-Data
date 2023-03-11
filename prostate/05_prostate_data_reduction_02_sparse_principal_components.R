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
# Dimension Reduction by Sparse Principal Components
#   - Sparse principal components combines some elements of variable clustering, scoring of variables within clusters, and redanduncy analysis
# ------------------------------------------------------------------------------
Ekg <- model.matrix(~ ekg, data = imp)[,-1]

head(Ekg)

pfn <- data$pfn


imp_s <- as.data.frame(imp)
imp_s$Ekg <- Ekg
imp_s$pfn <- data$pfn
imp_s <- imp_s %>% dplyr::select(-ekg, -pf)



# ----------
library(pcaPP)

# We specify k = 10
spca.raw <- sPCAgrid(imp_s, k = 12, method = "sd", cener = mean, scale = sd, scores = TRUE, maxiter = 30)

spca.trans <- sPCAgrid(data_trans, k = 12, method = "sd", cener = mean, scale = sd, scores = TRUE, maxiter = 30)



# ----------
# Compare to ordinary principal component analysis
summary(prin.raw)
summary(spca.raw)

summary(prin.trans)
summary(spca.trans)



# ----------
# Compare loadings to ordinary principal component analysis
spca.trans$loadings
prin.trans$loadings



# ----------
plot(prin.raw, type="lines", main="", ylim=c(0,3))
addscree(prin.raw, col="black", offset=0.8, adj=1, lwd=1, lty=1)
addscree(prin.trans, plotv=TRUE, col="red", offset=-0.8, adj=1, lwd=1, lty=2)

plot(spca.raw, type="lines", main="", ylim=c(0,3))
addscree(spca.raw, col="black", offset=0.8, adj=1, lwd=2, lty=1)
addscree(spca.trans, plotv=TRUE, col="red", offset=-0.8, adj=1, lwd=2, lty=2)



# -->
# Sparse principal components does worse than ordinary principal component analysis ...
