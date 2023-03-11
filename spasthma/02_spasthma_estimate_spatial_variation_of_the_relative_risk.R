setwd("//media//kswada//MyFiles//R//spasthma")

packages <- c("dplyr", "rgdal", "spatstat", "maptools")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  spasthma
# ------------------------------------------------------------------------------
library(rgdal)

spasthma <- readOGR("//media//kswada//MyFiles//data//spasthma", "spasthma")
spbdry <- readOGR("//media//kswada//MyFiles//data//spasthma//spbdry.shp", "spbdry")
spsrc <- readOGR("//media//kswada//MyFiles//data//spasthma//spsrc.shp", "spsrc")
sproads <- readOGR("//media//kswada//MyFiles//data//spasthma//sproads.shp", "sproads")


# ----------
# SpatialPointsDataFrame
str(spasthma)

# SpatialPolygonsDataFrame
str(spbdry)

# SpatialPointsDataFrame
str(spsrc)

# SpatialLinesDataFrame
str(sproads)



# ------------------------------------------------------------------------------
# preparation
# ------------------------------------------------------------------------------
pppasthma <- as(spasthma, "ppp")
pppasthma$window <- as(spbdry, "owin")

marks(pppasthma) <- relevel(pppasthma$marks$Asthma, "control")

pppasthma
str(pppasthma)



# ----------
cases <- unmark(subset(pppasthma, marks(pppasthma) =="case"))
controls <- unmark(subset(pppasthma, marks(pppasthma) =="control"))

ncases <- npoints(cases)
ncontrols <- npoints(controls)



# ------------------------------------------------------------------------------
# estimate of the disease risk by the ratio between the intensity of the cases and controls
# ------------------------------------------------------------------------------
# we have preferred to set the bandwidth manually to a value of 0.06, which provides a reasonable degree of smoothing
bwasthma <- .06

kcases <- density(cases, bwasthma)
kcontrols <- density(controls, bwasthma)


# The results are in an im object (a Pixel Image Object), which is a square grid with missing values in the points outside the study area.
kcases



# ----------
# coerce this object to a SpatialGridDataFrame object to hold missing values and coerce to a SpatialPixelsDataFrame to drop the missing cells
spkratio0 <- as(kcases, "SpatialGridDataFrame")
names(spkratio0) <- "kcases"
spkratio0$kcontrols <- as(kcontrols, "SpatialGridDataFrame")$v
spkratio <- as(spkratio0, "SpatialPixelsDataFrame")



# ----------
# estimate of the disease risk by the ratio between the intensity of the cases and controls
spkratio$kratio <- spkratio$kcases / spkratio$kcontrols
spkratio$logratio <- log(spkratio$kratio) - log(ncases/ncontrols)


spkratio
spkratio$kratio



# ------------------------------------------------------------------------------
# Assess departure from the null hypothesis (spkratio$kratio = 1 or spkratio$logratio = 0) by Monte Carlo simulation test
#  - The Monte Carlo test is based on the fact that cases and controls are equally distributed under the null hypothesis.
#    In that case, if we change the label of a case to be a control (or vice versa), the new set of cases (or controls)
#    still have the same spatial distribution and will have the same risk function.
#    If that is not the case, then the re-labelling of cases and controls will produce different risk functions
# ------------------------------------------------------------------------------
niter <- 99
ratio <- rep(NA, niter)
pvaluemap <- rep(0, nrow(spkratio))
rlabelratio <- matrix(NA, nrow = niter, ncol = nrow(spkratio))


set.seed(1)
for(i in 1:niter)
{
  pppasthma0 <- rlabel(pppasthma)
  casesrel <- unmark(subset(pppasthma0, marks(pppasthma0) =="case"))
  controlsrel <- unmark(subset(pppasthma0, marks(pppasthma0) =="control"))
  
  kcasesrel <- density(casesrel, bwasthma)
  kcontrolsrel <- density(controlsrel, bwasthma)
  kratiorel <- eval.im(kcasesrel / kcontrolsrel)
  rlabelratio[i,] <- as(as(kratiorel, "SpatialGridDataFrame"), "SpatialPixelsDataFrame")$v
  pvaluemap <- pvaluemap + (spkratio$kratio < rlabelratio[i,])
}


cellsize <- kcontrols$xstep * kcontrols$ystep
ratiorho <- cellsize * sum((spkratio$kratio - ncases / ncontrols)^2)
ratio <- cellsize * apply(rlabelratio, 1, function(X, rho0 ){sum((X - rho0) ^ 2)}, rho0 = ncases / ncontrols)
pvaluerho <- (sum(ratio > ratiorho) + 1) / (niter + 1)


# ----------
pvaluerho


# -->
# The results for the test with null hypothesis turned out to be non-significant (p-value of 0.61),
# which means that the observed risk ratio is consistent with a constant riks ratio.



# ----------
# 0.05 and 0.,95 contours of the p-value surface
spkratio$pvaluemap <- (pvaluemap + 1) / (niter + 1)
imgpvalue <- as.image.SpatialGridDataFrame(spkratio["pvaluemap"])
clpvalue <- contourLines(imgpvalue, levels=c(0,.05, .95, 1))
cl <- ContourLines2SLDF(clpvalue)


library(RColorBrewer)
cl05 <- cl[cl$level == "0.05",]
xzx <- slot(slot(cl05, "lines")[[1]], "Lines")
cl05a <- SpatialLines(list(Lines(xzx, ID="0.05")))
lyt05 <- list("sp.lines", cl05a, lwd=2, lty=2, col="grey95")
lyt95 <- list("sp.lines", cl[cl$level == "0.95",], lwd=2, lty=1)
lytb <- list("sp.polygons", spbdry)
lytp <- list("sp.points", spsrc, cex=0.9, pch=4, col="grey95", lwd=3)
brks <- quantile(spkratio$kratio[spkratio$kratio>0], seq(0,1,1/10), na.rm=TRUE)
brks[1] <- 0
lbrks <- formatC(brks, 3, 6, "g", " ")
cols <- colorRampPalette(brewer.pal(7, "Reds"))(length(brks)-1)
colorkey <- list(labels=lbrks, at=(0:10)/10, height=.5)


# kernel ratio of the intensity of cases and controls.
# the continuous and dashed lines show the surfaces associated with 0.95 and 0.05 p-values, respectively, grey crosses mark the pollution sources.
# The value of rho0 which marks a flat constant risk is 0.2
print(spplot(spkratio, "kratio",
             col.regions=cols,
             do.log=TRUE, 
             colorkey=colorkey,
             at=c(0, brks[-c(1,11)], max(spkratio$kratio, na.rm=TRUE)),
             sp.layout=list(lyt05, lyt95, lytb, lytp) 
))



