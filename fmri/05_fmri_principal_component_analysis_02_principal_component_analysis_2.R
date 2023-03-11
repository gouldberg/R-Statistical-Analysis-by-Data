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
# Now we focus on average across subjects of "Awake-Heat" and "Awake-Shock"
# Averaging "Awake-Heat" and "Awake-Shock"
# ------------------------------------------------------------------------------

# Awake-Heat
ah <- list(fmri$L1T2, fmri$L2T2, fmri$L3T2, fmri$L4T2, fmri$L5T2, fmri$L6T2, fmri$L7T2, fmri$L8T2, fmri$L9T2)

ah <- bind_cols(lapply(1:length(ah), function(x) rowMeans(as.matrix(ah[[x]])))) %>% as.data.frame()

colnames(ah) <- c("L1T2", "L2T2", "L3T2", "L4T2", "L5T2", "L6T2", "L7T2", "L8T2", "L9T2")


# Awake-Shock
as <- list(fmri$L1T3, fmri$L2T3, fmri$L3T3, fmri$L4T3, fmri$L5T3, fmri$L6T3, fmri$L7T3, fmri$L8T3, fmri$L9T3)

as <- bind_cols(lapply(1:length(as), function(x) rowMeans(as.matrix(as[[x]])))) %>% as.data.frame()

colnames(as) <- c("L1T3", "L2T3", "L3T3", "L4T3", "L5T3", "L6T3", "L7T3", "L8T3", "L9T3")


plot.ts(ah, main = "Awake-Heat average across subjects")

plot.ts(as, main = "Awake-Shock average across subjects")



# ------------------------------------------------------------------------------
# Periodogram for each locations
# ------------------------------------------------------------------------------

n <- nrow(ah)

Per_ah = abs(mvfft(as.matrix(ah)))^2/n

Per_as = abs(mvfft(as.matrix(as)))^2/n


graphics.off()
par(mfrow=c(3,3), mar=c(3,2,2,1),  mgp = c(1.6,.6,0), oma=c(0,1,0,0))
for (i in 1:9) plot(0:20, Per_ah[1:21,i], type="l", ylim=c(0,8), main=colnames(ah)[i], 
                    xlab="Cycles", ylab="", xaxp=c(0,20,5))


graphics.off()
par(mfrow=c(3,3), mar=c(3,2,2,1),  mgp = c(1.6,.6,0), oma=c(0,1,0,0))
for (i in 1:9) plot(0:20, Per_as[1:21,i], type="l", ylim=c(0,8), main=colnames(as)[i], 
                    xlab="Cycles", ylab="", xaxp=c(0,20,5))


# ------------------------------------------------------------------------------
# Estimate spectral density
# ------------------------------------------------------------------------------

fxx_ah = mvspec(as.matrix(ah), kernel("daniell", c(1,1)), taper=.5, plot=FALSE)$fxx

fxx_as = mvspec(as.matrix(as), kernel("daniell", c(1,1)), taper=.5, plot=FALSE)$fxx

dim(fxx_ah)

dim(fxx_as)



# ----------
l.val_ah = rep(NA, 64)
l.val_as = rep(NA, 64)

for (k in 1:64) {
  u = eigen(fxx_ah[,,k], symmetric=TRUE, only.values=TRUE)
  l.val_ah[k] = u$values[1]
  u = eigen(fxx_as[,,k], symmetric=TRUE, only.values=TRUE)
  l.val_as[k] = u$values[1]
}



# -->
# we will focus primarily on the signal period of 64s, which translates to 4 cycles in 256s or omega = 4/128 = 32 cycles per time point
# Estimated spectrum of the 1st principal component series by calculating the largest eigenvalue for each j = 0,1,2,...,64.



# largest peak (=2) at the stimulus frequency 4/128
par(mfrow=c(1,1))
plot(l.val_ah, type="l", xaxp=c(0,64,8), xlab="Cycles (Frequency x 128)", ylab="First Principal Component")
axis(1, seq(4,60,by=8), labels=FALSE)


par(mfrow=c(1,1))
plot(l.val_as, type="l", xaxp=c(0,64,8), xlab="Cycles (Frequency x 128)", ylab="First Principal Component")
axis(1, seq(4,60,by=8), labels=FALSE)



# ------------------------------------------------------------------------------
# Principal Component Analysis
# ------------------------------------------------------------------------------

# at freq k=4
u_ah = eigen(fxx_ah[,,4], symmetric=TRUE)

lam_ah = u_ah$values 

u_as = eigen(fxx_as[,,4], symmetric=TRUE)

lam_as = u_as$values 



lam_ah[1] / sum(lam_ah)

lam_as[1] / sum(lam_as)


# -->
# the proportion of the power at frequency 4/128 attributed to the 1st principal components series is about 96% and 98%
# Because the first principal component explains nearly all of the total power at the stimulus frequency,
# there is no need to explore the other principal component series at this frequency



# ----------
evec_ah = u_ah$vectors

sig.e1_ah = matrix(0,9,9)

for (l in 1:9){
  sig.e1_ah = sig.e1_ah + lam_ah[l] * evec_ah[,l] %*% Conj(t(evec_ah[,l])) / (lam_ah[1] - lam_ah[l]) ^ 2
}

sig.e1_ah = Re(sig.e1_ah) * lam_ah[1] * sum(kernel("daniell", c(1,1))$coef^2)

p.val_ah = round(pchisq(2 * abs(evec_ah[,1]) ^ 2 / diag(sig.e1_ah), 2, lower.tail=FALSE), 5)



# ----------
evec_as = u_as$vectors

sig.e1_as = matrix(0,9,9)

for (l in 1:9){
  sig.e1_as = sig.e1_as + lam_as[l] * evec_as[,l] %*% Conj(t(evec_as[,l])) / (lam_as[1] - lam_as[l]) ^ 2
}

sig.e1_as = Re(sig.e1_as) * lam_as[1] * sum(kernel("daniell", c(1,1))$coef^2)

p.val_as = round(pchisq(2 * abs(evec_as[,1]) ^ 2 / diag(sig.e1_as), 2, lower.tail=FALSE), 5)



# ----------
# print table values
cbind(colnames(ah), abs(evec_ah[,1]), p.val_ah)

cbind(colnames(as), abs(evec_as[,1]), p.val_as)




