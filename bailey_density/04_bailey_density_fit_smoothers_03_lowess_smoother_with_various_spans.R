# setwd("//media//kswada//MyFiles//R//bailey_density")
setwd("//media//kswada//MyFiles//R//Generalized_additive_model//bailey_density")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Bailey Density
# ------------------------------------------------------------------------------

DF1 <- read.table(file = "BaileyDensity.txt", header = TRUE)


str(DF1)


dim(DF1)


car::some(DF1)



# ----------
# missing values for the spatial coordinates to be removed
colSums(is.na(DF1))


DF  <- na.exclude(DF1)



# ------------------------------------------------------------------------------
# compare various span widths
# ------------------------------------------------------------------------------

M1A <- loess(Dens ~ MeanDepth, data = DF, span = 0.1)

M1B <- loess(Dens ~ MeanDepth, data = DF, span = 0.5)

M1C <- loess(Dens ~ MeanDepth, data = DF, span = 1)

P1A <- predict(M1A, se = TRUE)

P1B <- predict(M1B, se = TRUE)

P1C <- predict(M1C, se = TRUE)


P   <- c(P1A$fit, P1B$fit, P1C$fit)

SEs <- c(P1A$se.fit, P1B$se.fit, P1C$se.fit)

MD  <- rep(DF$MeanDepth, 3)

ID  <- factor(rep(c("span = 0.1","span = 0.5","span = 1"), each = nrow(DF)))

Y <- rep(DF$Dens, 3)

X <- rep(DF$MeanDepth, 3)



# ----------
# With polygons of grey areas representing 95% point-wise confidence intervals

xyplot(Y ~ X | ID, col = 1,
       xlab = list(label = "Mean depth", cex = 1.5),
       ylab = list(label = "Density", cex = 1.5),
       layout = c(3,1),
       strip = strip.custom(bg = 'white', par.strip.text = list(cex = 1.2)),
       scales = list(alternating = T, x = list(relation = "same"), y = list(relation = "same")),
       panel = function(x, y, subscripts,...){
         y1  <- P[subscripts]
         x1  <- MD[subscripts]
         se1 <- SEs[subscripts]
         panel.polygon(c(x1, rev(x1)), c(y1-2*se1, rev(y1+2*se1)), col =grey(0.5), border=NULL, density =50)
         panel.lines(x1,y1, lwd = 3, col = 1)
         panel.points(x, y, col = 1)
       })

