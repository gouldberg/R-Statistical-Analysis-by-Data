setwd("//media//kswada//MyFiles//R//seabird")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  seabird
# ------------------------------------------------------------------------------
data("seabird", package = "fda")

str(seabird)

car::some(seabird)



# ------------------------------------------------------------------------------
# Shows base 10 logarithms of counts of the 13 species averaged over transects and sites, and separated according to diet.
# ------------------------------------------------------------------------------

sites = c('Uganik', 'Uyak')

sel   = seabird$Bay %in% sites

UU    = seabird[sel,]

head(UU)



# ----------
# Drop 2 species with many NAs
NAs       = sapply(UU, function(x) sum(is.na(x)))

NAs      = which(NAs > 2)

birdindex = (1:15)[-NAs]

birds     = names(UU)[birdindex]



# ----------
# Compute mean counts taken over both sites and transects
meanCounts = matrix(NA, 20, 13)

dimnames(meanCounts) = list(1986:2005, birds)


for(i in 1:20){
  sel = (UU$Year == rownames(meanCounts)[i])
  meanCounts[i, ] = sapply(UU[sel, birds], mean, na.rm=TRUE)
}

selYear   = !is.na(meanCounts[,1])

( logCounts = log10(meanCounts[selYear,]) )



# ----------
# time vectors in years and in indices in 1:20

yearObs  = as.numeric(rownames(logCounts))

yearCode = (1:20)[selYear]



# ----------
shellfishindex = c(1,2,5,6,12,13)

fishindex      = (1:13)[-shellfishindex]

ylim = range(logCounts)


op = par(mfrow=c(2,1), mar=c(2, 4, 4, 1)+.1)
matplot(yearObs, logCounts[, shellfishindex], xlab='', ylab='',
        ylim=ylim, main='Shellfish Diet', type='b', col=1)
meanShellfish = apply(meanCounts[, shellfishindex], 1, mean)
lines(yearObs, log10(meanShellfish[!is.na(meanShellfish)]), lwd=3)
abline(h=0, lty='dotted')

matplot(yearObs, logCounts[, fishindex], xlab='', ylab='',
        ylim=ylim, main='Fish Diet', type='b', col=1)
meanFish = apply(meanCounts[, shellfishindex], 1, mean)
lines(yearObs, log10(meanFish[!is.na(meanFish)]), lwd=3)
abline(h=0, lty='dotted')

par(op)



# -->
# Shows base 10 logarithms of counts of the 13 species averaged over transects and sites, and separated according to diet.
# In each panel, the mean count taken across birds is shown as a heavy solid line.

# It is obvious that there are substantial differences in abundances over species that are consistent across the years of observation,
# and there is more variation in abundance among the fish-eating birds.

# The two mean functions suggest a slight tendency for the fish-eating birds to be increasing in abundance relative to what we see for shellfish
# and mollusk eaters, although this may be due to the sharply increasing trend for one fish-eating species.

