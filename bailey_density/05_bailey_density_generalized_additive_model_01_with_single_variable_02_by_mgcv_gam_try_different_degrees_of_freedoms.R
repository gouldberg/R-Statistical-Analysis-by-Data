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
# GAM with 6 different values of degrees of freedom
# ------------------------------------------------------------------------------

M4A <- mgcv::gam(Dens ~ s(MeanDepth, fx = TRUE, k = 3), data=DF)
M4B <- mgcv::gam(Dens ~ s(MeanDepth, fx = TRUE, k = 4), data=DF)
M4C <- mgcv::gam(Dens ~ s(MeanDepth, fx = TRUE, k = 5), data=DF)
M4D <- mgcv::gam(Dens ~ s(MeanDepth, fx = TRUE, k = 6), data=DF)
M4E <- mgcv::gam(Dens ~ s(MeanDepth, fx = TRUE, k = 8), data=DF)
M4F <- mgcv::gam(Dens ~ s(MeanDepth, fx = TRUE, k = 10), data=DF)

P4A <- predict(M4A, newdata=MyData4, se=TRUE)
P4B <- predict(M4B, newdata=MyData4, se=TRUE)
P4C <- predict(M4C, newdata=MyData4, se=TRUE)
P4D <- predict(M4D, newdata=MyData4, se=TRUE)
P4E <- predict(M4E, newdata=MyData4, se=TRUE)
P4F <- predict(M4F, newdata=MyData4, se=TRUE)


AllP <- c(P4A$fit, P4B$fit, P4C$fit, P4D$fit, P4E$fit, P4F$fit)

AllX <- rep(MyData4$MeanDepth, 6)

AllSE <- c(P4A$se.fit, P4B$se.fit, P4C$se.fit, P4D$se.fit, P4E$se.fit, P4F$se.fit)

Names <- c("df = 2", "df = 3", "df = 4", "df = 5", "df = 7", "df = 9")

ID <- rep(Names, each = 100)

Y1 <- rep(DF$Dens, 6)

X1 <- rep(DF$MeanDepth, 6)

ID1 <- rep(Names, each = nrow(DF))



# ----------
xyplot(Y1 ~ X1 | factor(ID1),
       xlab = list(label = "Mean depth", cex = 1.5),
       ylab = list(label = "Density", cex = 1.5),
       strip = strip.custom(bg = 'white', par.strip.text = list(cex = 1.2)),
       scales = list(alternating = T, x = list(relation = "same"), y = list(relation = "same")),
       panel = function(x,y,subscripts,...){
         panel.points(x,y, col =1, pch = 1)
         ThsP <- ID1[subscripts][1]
         xx1 <- AllX[ID==ThsP]
         yy1 <- AllP[ID==ThsP]
         panel.lines(xx1, yy1, col = 1, lwd = 5)}
)

