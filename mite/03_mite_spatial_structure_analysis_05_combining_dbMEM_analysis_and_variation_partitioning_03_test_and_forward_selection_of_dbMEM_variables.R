# setwd("//media//kswada//MyFiles//R//mite")
setwd("//media//kswada//MyFiles//R//Spatial_data_analysis//mite")

packages <- c("dplyr", "ape", "spdep", "ade4", "adegraphics", "adespatial", "vegan")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  mite
#  - Substrate (7 classes),  Shrubs (3 classes), and Microtopography (2 classes)
# ------------------------------------------------------------------------------

load("./data/mite.RData")


dim(mite)
car::some(mite)


dim(mite.env)
car::some(mite.env)


dim(mite.xy)
car::some(mite.xy)



source("./functions/plot.links.R")
source("./functions/sr.value.R")
source("./functions/quickMEM.R")
source("./functions/scalog.R")



# ------------------------------------------------------------------------------
# Test and forward selection of the dbMEM variables
# ------------------------------------------------------------------------------

# Run the global dbMEM analysis on the *detrended* mite data

mite.det.dbmem.rda <- rda(mite.h.det ~., mite.dbmem)


anova(mite.det.dbmem.rda)



# ----------
# Since the analysis is significant, compute the adjusted R2
# and run a forward selection of the dbMEM variables

( mite.det.dbmem.R2a <- 
    RsquareAdj(mite.det.dbmem.rda)$adj.r.squared )


( mite.det.dbmem.fwd <- 
    forward.sel(mite.h.det, 
                as.matrix(mite.dbmem), 
                adjR2thresh = mite.det.dbmem.R2a) )


# ----------
# Number of significant dbMEM

( nb.sig.dbmem <- nrow(mite.det.dbmem.fwd) )


# Identify the significant dbMEM sorted in increasing order
( dbmem.sign <- sort(mite.det.dbmem.fwd$order) )


# Write the significant dbMEM to a new object (reduced set)
dbmem.red <- mite.dbmem[ ,c(dbmem.sign)]



# ------------------------------------------------------------------------------
# Arbitrarily split the significant dbMEM into broad and fine scale
# ------------------------------------------------------------------------------

# Broad scale: dbMEM 1, 3, 4, 6, 7
dbmem.broad <- dbmem.red[ , 1 : 5]


# Fine scale: dbMEM 10, 11, 20
dbmem.fine <- dbmem.red[ , 6 : 8]


