setwd("//media//kswada//MyFiles//R//canadian_weather")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  seabird
# ------------------------------------------------------------------------------
data("seabird", package = "fda")

str(seabird)

car::some(seabird)



# ----------
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



# ------------------------------------------------------------------------------
# pre-process
# ------------------------------------------------------------------------------

# Compute mean counts taken over transects only within sites so we have 2 observations for each bird species each year.
# Two of these counts are zero, and are replaced by 1/(2*n)

meanCounts2 = matrix(NA, 20, 26)

for(i in 1:20) for (j in 1:2) {
  sel = (UU$Year == rownames(meanCounts)[i] & as.character(UU$Bay) == sites[j])
  meanCountsij = sapply(UU[sel, birds], mean, na.rm=TRUE)
  n = sum(sel)
  if (n > 0) {
    meanCountsij[meanCountsij == 0] = 1/(2*n)
  }
  meanCounts2[i,(j-1)*13+(1:13)] = meanCountsij
}

selYear2   = !is.na(meanCounts2[, 1])
yearCode  = (1:20)[selYear2]
all.equal(yearCode, c(1:12, 14:20))



# ----------
# In this matrix, the 1st 13 columns are for the Uganik site and the remaining 13 for the Uyak site,
# and each row corresponds to a year of observation
logCounts2 = log10(meanCounts2[selYear2,])

head(logCounts2)



# ------------------------------------------------------------------------------
# Represent response with polygonal basis
# ------------------------------------------------------------------------------
# Represent log mean counts exactly with a polygonal basis
# since we are less interested in estimating smooth trends for each species than we are in estimating the functional diet effect

# By interpolating the data in this way, we are sure to retain all the information in the original data.

# Set up the polygonal basis and fit the data in the 19 by 26 matrix locCounts2, and yearCode = c(1:12, 14:20)
# because no data are collected fo 1986, year 13.


birdbasis = create.polygonal.basis(yearCode)

birdlist2 = smooth.basis(yearCode, logCounts2, birdbasis)

birdfd2 = birdlist2$fd


plot(birdfd2)

