setwd("//media//kswada//MyFiles//R//japanesepines")

packages <- c("dplyr", "spatstat", "maptools")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  japanesepines
# ------------------------------------------------------------------------------

data("japanesepines", package = "spatstat")

class(japanesepines)

str(japanesepines)



# ----------
# convert to Spatial Points class
spjpines <- as(japanesepines, "SpatialPoints")

str(spjpines)

# Convert to unit square using the elide methods
spjpines1 <- elide(spjpines, scale=TRUE, unitsq=TRUE)



# ----------
data(redwoodfull, package = "spatstat")

class(redwoodfull)

spred <- as(redwoodfull, "SpatialPoints")



# ----------
data(cells, package = "spatstat")

class(cells)

spcells <- as(cells, "SpatialPoints")



# ----------
summary(spjpines1)

summary(spred)

summary(spcells)



# ------------------------------------------------------------------------------
# Compare kernel density and inhomogeneous Poisson process
# ------------------------------------------------------------------------------

library(RColorBrewer)

gp <- brewer.pal(8, "Reds")


graphics.off()


# ----------
# redwood full:  kernel density

print(spplot(kernels, at=c(0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000), col.regions=colorRampPalette(gp)(15)[1:12], 
             names.attr=c(paste("Q bw=",round(bw, digits=4), sep="", collapse=""),
                          "Q bw=0.05", "Q bw=0.1","Q bw=0.15", paste("G bw=", round(.5*bw, digits=4),
                                                                     sep="", collapse=""), "G bw=0.025", "G bw=0.05","G bw=0.075"), cex=0.7, colorkey=FALSE))

# redwood full:  inhomogeneous Poisson process
print(spplot(parint_r, at=seq(0, 400, length.out=8), col.regions=colorRampPalette(gp)(7), sp.layout=list(lyt_r)))



# ----------
# japanese pines:  kernel density

print(spplot(kernels_j, at=c(0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000), col.regions=colorRampPalette(gp)(15)[1:12], 
             names.attr=c(paste("Q bw=",round(bw, digits=4), sep="", collapse=""),
                          "Q bw=0.05", "Q bw=0.1","Q bw=0.15", paste("G bw=", round(.5*bw, digits=4),
                                                                     sep="", collapse=""), "G bw=0.025", "G bw=0.05","G bw=0.075"), cex=0.7, colorkey=FALSE))

# japanese pines:  inhomogenous Poisson process

print(spplot(parint_j, at=seq(0, 200, length.out=8), col.regions=colorRampPalette(gp)(7), sp.layout=list(lyt_j)))



# ----------
# cells:  kernel density

print(spplot(kernels_c, at=c(0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000), col.regions=colorRampPalette(gp)(15)[1:12], 
             names.attr=c(paste("Q bw=",round(bw, digits=4), sep="", collapse=""),
                          "Q bw=0.05", "Q bw=0.1","Q bw=0.15", paste("G bw=", round(.5*bw, digits=4),
                                                                     sep="", collapse=""), "G bw=0.025", "G bw=0.05","G bw=0.075"), cex=0.7, colorkey=FALSE))

# cells:  inhomogenous Poisson process

print(spplot(parint_c, at=seq(0, 200, length.out=8), col.regions=colorRampPalette(gp)(7), sp.layout=list(lyt_c)))



# -->
# different gradients by kernel density and inhomogeneous Poisson process
# different between redwood full and japanese pines

