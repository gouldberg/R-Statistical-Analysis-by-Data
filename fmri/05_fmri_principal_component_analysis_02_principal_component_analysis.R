setwd("//media//kswada//MyFiles//R//fmri1")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  fMRI Imaging
# ------------------------------------------------------------------------------

data(fmri1, package = "astsa")

str(fmri1)

head(fmri1)


data(fmri, package = "astsa")

str(fmri)

head(fmri)



# ------------------------------------------------------------------------------
# Principal Component Analysis
#  - Which locations are responding with the most spectral power, and which locations do not contribute to the spectral power at the stimulus signal period ?
# ------------------------------------------------------------------------------

# at freq k=4
u = eigen(fxx[,,4], symmetric=TRUE)

lam = u$values 

lam[1] / sum(lam)



# -->
# the proportion of the power at frequency 4/128 attributed to the 1st principal components series is about 98%
# Because the first principal component explains nearly all of the total power at the stimulus frequency,
# there is no need to explore the other principal component series at this frequency



# ----------
evec = u$vectors

sig.e1 = matrix(0,8,8)

for (l in 2:5){  # last 3 evecs are 0
  sig.e1 = sig.e1 + lam[l] * evec[,l] %*% Conj(t(evec[,l])) / (lam[1] - lam[l]) ^ 2
}

sig.e1 = Re(sig.e1) * lam[1] * sum(kernel("daniell", c(1,1))$coef^2)

p.val = round(pchisq(2 * abs(evec[,1]) ^ 2 / diag(sig.e1), 2, lower.tail=FALSE), 3)

cbind(colnames(fmri1)[-1], abs(evec[,1]), p.val) # print table values



# -->
# Magnitudes of the PC vector at the stimulus frequency
# As expected, the analysis indicates that location 6 is not contributing to the power at this frequency.
# But SURPRISINGLY !!!, the analysis suggests location 5 (cerebellum 1) is responding to the stimulus.
