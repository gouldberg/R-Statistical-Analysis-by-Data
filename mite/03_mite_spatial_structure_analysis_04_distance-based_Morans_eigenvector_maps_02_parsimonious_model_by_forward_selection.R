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
# Distance-based Moran eigenvector maps (dbMEM)
#   - 
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Construct the matrix of dbMEM variables
# ------------------------------------------------------------------------------

mite.dbmem.tmp <- dbmem(mite.xy, silent = FALSE)

mite.dbmem <- as.data.frame(mite.dbmem.tmp)



# ----------
# Truncation distance used above:
( thr <- give.thresh(dist(mite.xy)) )



# ----------
# Display and count the eigenvalues
attributes(mite.dbmem.tmp)$values

length(attributes(mite.dbmem.tmp)$values)


# Argument silent = FALSE allows the function to display the truncation level.



# ------------------------------------------------------------------------------
# Run the global dbMEM analysis on the *detrended* Hellinger-transformed data
# ------------------------------------------------------------------------------

( mite.dbmem.rda <- rda(mite.h.det ~ ., mite.dbmem) )


anova(mite.dbmem.rda)



# ------------------------------------------------------------------------------
# Compute adjusted R2 and run a forward variable selection
# ------------------------------------------------------------------------------

( mite.R2a <- RsquareAdj(mite.dbmem.rda)$adj.r.squared )



# ----------
( mite.dbmem.fwd <- forward.sel(mite.h.det, as.matrix(mite.dbmem), 
                               adjR2thresh = mite.R2a) )


mite.dbmem.fwd



# ----------
# Number of signif. dbMEM
( nb.sig.dbmem <- nrow(mite.dbmem.fwd) )


# Identity of the significant dbMEM in increasing order
( dbmem.sign <- sort(mite.dbmem.fwd[ ,2]) )


# Write the significant dbMEM to a new object
dbmem.red <- mite.dbmem[ ,c(dbmem.sign)]



# ------------------------------------------------------------------------------
# parsimonious model
# ------------------------------------------------------------------------------

( mite.dbmem.rda2 <- rda(mite.h.det ~ ., data = dbmem.red) )


summary(mite.dbmem.rda2)


# -->
# Proportion of variance explained by the first 3 canonical axes with respect to the total explained varianece is 0.8895
# see "Accmulated constrinaed eigenvalues" section



# ----------
( mite.fwd.R2a <- RsquareAdj(mite.dbmem.rda2)$adj.r.squared )



# -->
# adjusted R2 with 8 dbMEM variables retained by forward selection is 24.18%
# 3 canonical axes expain 24.18 * 0.8895 = 21.5% of the total vaiance



# ----------
# global test
anova(mite.dbmem.rda2)


# ----------
# test by axis
( axes.test <- anova(mite.dbmem.rda2, by = "axis") )



# ----------
# Number of significant axes
( nb.ax <- length(which(axes.test[ ,ncol(axes.test)] <=  0.05)) )


# but here we override with nb.ax = 3
nb.ax <- 3



# ------------------------------------------------------------------------------
# Plot the significant canonical axes
# ------------------------------------------------------------------------------

mite.rda2.axes <- 
  scores(mite.dbmem.rda2, 
         choices = c(1:nb.ax), 
         display = "lc", 
         scaling = 1)


par(mfrow = c(1,nb.ax))

for(i in 1:nb.ax){
  sr.value(mite.xy, mite.rda2.axes[ ,i], 
           sub = paste("RDA",i), 
           csub = 2)
}



