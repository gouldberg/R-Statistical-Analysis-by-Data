setwd("//media//kswada//MyFiles//R//fmri1")

packages <- c("dplyr", "fda")
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
# Functional Principal Component Analysis
# ------------------------------------------------------------------------------

# Smooth with lambda as determined before
gaitfdPar  <- fdPar(asbasis, harmaccelLfd20, lambda=1e-2)

nharm = 4

gaitpca.fd <- pca.fd(gaitfd, nharm=nharm, gaitfdPar)

gaitpca2.fd <- varmx.pca.fd(gaitpca.fd)



# ------------------------------------------------------------------------------
# Plot principal components
# ------------------------------------------------------------------------------
op <- par(mfrow=c(2,2))
plot.pca.fd(gaitpca.fd)
par(op)



# ----------
op <- par(mfrow=c(2,2))
plot.pca.fd(gaitpca2.fd)
par(op)



# ------------------------------------------------------------------------------
# Plot harmonics using cycle plots
# ------------------------------------------------------------------------------
# plot harmonics using cycle plots
op <- par(mfrow=c(2,2))
plot.pca.fd(gaitpca.fd, cycle=TRUE)
par(op)



# ----------
op <- par(mfrow=c(2,2))
plot.pca.fd(gaitpca2.fd, cycle=TRUE)
par(op)
