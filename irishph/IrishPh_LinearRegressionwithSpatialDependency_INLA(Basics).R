# ------------------------------------------------------------------------------
# Data: IrishPh
#
# Linear regression model with spatial dependency
# 
# Model pH of the water as a function of SDI (sodium dominance index), altitude (log transformed) and whether a site is forested
#
# The original research used sampled data from 257 rivers in Ireland during 2002 and 2003.  We use only the 2003 data.
# One of the aims underlying the study was to find a different tool for identifying acid-sensitive waters, which currently uses measurs of pH.
# The problem with pH is that it is extremely variable within a catchment, and depends on both flow conditions and underlying geology.
# As an slternative measure, the Sodium Dominance Index (SDI) is defined as the contribution indicator of the acid sensitivity of rivers.
# SDI:  the contribution of sodium (Na+) to the sum of the major cations (positively charged atoms or molecules)
#
# The motivation for this research is the increase in plantation forestry cover in Irish landscapes and its potential impacts on aquatic resources.
# 
# ------------------------------------------------------------------------------
rm(list=ls())

setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "Hmisc", "lattice", "ggplot2", "sp", "gstat", "ggmap")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

source("./ZuurSpatialTemporalAndSpatialTemporalEcologicalDataAnalysiswithRINLA/MCMCSupportHighstatV4.R")


# ------------------------------------------------------------------------------
# Load Data and Basics
# ------------------------------------------------------------------------------
data <- read.table(file = "./ZuurSpatialTemporalAndSpatialTemporalEcologicalDataAnalysiswithRINLA/IrishPh.txt", header=T, dec=".")
dim(data)
str(data)

Hmisc::describe(data)

is.na(data)
colSums(is.na(data))


data$fForested <- factor(data$Forested, levels = c(1, 2), labels = c("Yes", "No"))
data$LogAlt <- log10(data$Altitude)


# ------------------------------------------------------------------------------
# Linear regression with 3-way interactions by INLA
# ------------------------------------------------------------------------------
library(INLA)
I1 <- inla(pH ~ LogAlt * SDI * fForested, family = "gaussian", control.predictor = list(compute = TRUE), data = data)
summary(I1)


Beta1 <- I1$summary.fixed[, c("mean", "sd", "0.025quant", "0.975quant")] 
print(Beta1, digits = 2)

# pH(i) ~ N(mu(i), 0.375^2)
tau <- I1$marginals.hyperpar$`Precision for the Gaussian observations`
sigma <- inla.emarginal(function(x) (1/sqrt(x)), tau)
sigma


# Model validation
Fit1 <- I1$summary.fitted.values[,"mean"]
E1   <- data$pH - Fit1


X <- model.matrix(~ LogAlt * SDI * fForested, data = data)
Fit1a <- X %*% Beta1[,"mean"]
cbind(Fit1, Fit1a)


# ------------------------------------------------------------------------------
# Choose mesh for initial analysis
# ------------------------------------------------------------------------------
# To avoid problems with extreme large or small parameters for the Matern correlation function, we express the coordinates in kilometers.
data$EastingKM  <- data$Easting / 1000
data$NorthingKM <- data$Northing / 1000


# Distribution of distance between 2 sites
# 50% of the distances between sites is less than 200 kilometres
Loc <- cbind(data$EastingKM, data$NorthingKM)
head(Loc)
D <- dist(Loc)
par(mfrow = c(1,2), mar = c(5,5,2,2), cex.lab = 1.5)
hist(D, freq = TRUE, main = "",  xlab = "Distance between sites (km)", ylab = "Frequency")
text(10, 2000, "A", cex = 1.5)

plot(x = sort(D), y = (1:length(D))/length(D), type = "l", xlab = "Distance between sites (km)", ylab = "Cumulative proportion")
text(10, 1, "B", cex = 1.5)


# Make the various patterns of mesh
mesh1 <- inla.mesh.2d(Loc, max.edge=c(10, 10), cutoff = 0)
mesh2 <- inla.mesh.2d(Loc, max.edge=c(10, 10), cutoff = 10)
mesh3 <- inla.mesh.2d(Loc, max.edge=c(50, 50))    
mesh4 <- inla.mesh.2d(Loc, max.edge=c(75, 75), cutoff = 1)
mesh5 <- inla.mesh.2d(Loc, max.edge=c(25, 50), cutoff = 1)
mesh6 <- inla.mesh.2d(Loc, max.edge=c(50, 80), cutoff = 1)
mesh7 <- inla.mesh.2d(Loc, max.edge=c(100, 120), cutoff = 1)
mesh8 <- inla.mesh.2d(Loc, max.edge=c(150, 150), cutoff = 1)

# Explanation:
#   -cutoff: Points with a value smaller than this are replaced
#            by a single vertex.
#   -max.edge: The largest allowable edge length (inside and outside)
#              The larger these values the less points on the mesh...
#              the faster the computing time...the less precise the solution.
#              If the second value is 0, then there is no external part.
#   -boundary: Useful for islands and fjords.


hull <- inla.nonconvex.hull(Loc)
mesh9 <- inla.mesh.2d(boundary = hull, max.edge=50, cutoff = 5)


# Make a plot of the meshes
par(mfrow=c(3,3), mar=c(1,1,1,1))
for (i in 1:9){
  plot(get(paste('mesh', i, sep = '')), main = "",asp=1)
  points(Loc, col = 2, pch = 16, cex = 1)
}

# Number of vertices --> Mesh 1 has 4943 vertices, meaning that the model will estimate 4943 of the w(k)s
c(mesh1$n, mesh2$n, mesh3$n, mesh4$n, mesh5$n, mesh6$n,  mesh7$n, mesh8$n, mesh9$n)


# Our general strategy is to work with a mesh of size 700-800 vertices (Mesh 5) during the initial analysis
# and for final presentation use a finer mesh
# See Mesh 5
# Every sampling location is on a vertex, meaning that if sampling location i is on vertex k then the value of u(spatial random effects) is a weighted average of the three wks
# that correspond to the vertices that make up the correspoinding triangle.
par(mfrow=c(1,1), mar=c(0,0,0,0))
plot(mesh5, asp=1, main = "")
points(Loc, col = 1, pch = 16, cex = 1.5)


# ------------------------------------------------------------------------------
# Define the weight factors aik
# ------------------------------------------------------------------------------
A2      <- inla.spde.make.A(mesh2, loc = Loc)
A5      <- inla.spde.make.A(mesh5, loc = Loc)

# We have 210 sampling locations and the mesh as 731 vertices.
dim(A5)
dim(A2)

A5


# ------------------------------------------------------------------------------
# Define the SPDE
# ------------------------------------------------------------------------------
# alpha = 2 meaning v = 1 in calculation of Matern correlation values is default.
spde   <- inla.spde2.matern(mesh5, alpha = 2)
spde2  <- inla.spde2.matern(mesh2, alpha = 2)
str(spde)


# ------------------------------------------------------------------------------
# Define the spatial field
# ------------------------------------------------------------------------------
w.index <- inla.spde.make.index('w', n.spde = spde$n.spde)
w2.index <- inla.spde.make.index('w', n.spde = spde2$n.spde)
str(w.index)


# ------------------------------------------------------------------------------
# Define the stack
# ------------------------------------------------------------------------------
# the elements of the projector matrix "A" are linked to the elements of the "effects" argument
Xm <- model.matrix(~ LogAlt * SDI * fForested, data = data)
N <- nrow(data)
colnames(Xm)

X <- data.frame(Alt          = Xm[,2],
                SDI          = Xm[,3],
                fFor         = Xm[,4],
                Alt.SDI      = Xm[,5],
                Alt.fFor     = Xm[,6],
                SDI.fFor     = Xm[,7],
                Alt.SDI.fFor = Xm[,8]
)

StackFit <- inla.stack(tag = "Fit", data = list(y = data$pH),  A = list(1, 1, A5),  effects = list(Intercept = rep(1, N), X = X, w = w.index))
str(StackFit)


# ------------------------------------------------------------------------------
# Define the formula for the spatial model
# ------------------------------------------------------------------------------
# "-1" means that the default intercept is being dropped from the model, and instead we use our own Intercept component.
# This is required when multipe response variables are fitted in the samme model. (optional here.)
f2a <- y ~ -1 + Intercept + Alt  + SDI + fFor + Alt.SDI + Alt.fFor + SDI.fFor + Alt.SDI.fFor
f2b <- y ~ -1 + Intercept + Alt  + SDI + fFor + Alt.SDI + Alt.fFor + SDI.fFor + Alt.SDI.fFor + f(w, model = spde)


# ------------------------------------------------------------------------------
# Execute the linear regression without spatial field and with spatial field
# ------------------------------------------------------------------------------
IM2a <- inla(f2a,
             family = "gaussian", 
             data = inla.stack.data(StackFit),
             control.compute = list(dic = TRUE, waic = TRUE),
             control.predictor = list(A = inla.stack.A(StackFit)))
summary(IM2a)


# The model with the spatial field
IM2b <- inla(f2b,
             family = "gaussian", 
             data=inla.stack.data(StackFit),
             control.compute = list(dic = TRUE, waic = TRUE),
             control.predictor = list(A = inla.stack.A(StackFit)))
summary(IM2b)


# And compare the three models with DICs and WAICs
dic2  <- c(IM2a$dic$dic, IM2b$dic$dic)
waic2 <- c(IM2a$waic$waic, IM2b$waic$waic)
Z.out     <- cbind(dic2, waic2)
rownames(Z.out) <- c("Gaussian lm",  "Gaussian lm + SPDE")
Z.out


# And this is the model with mesh 2 --> 4856 vertices
StackFit2 <- inla.stack(tag = "Fit", data = list(y = data$pH),  A = list(1, 1, A2),  effects = list(Intercept = rep(1, N), X = X, w = w2.index))
f2b.mesh2 <- y ~ -1 + Intercept + Alt  + SDI + fFor + Alt.SDI + Alt.fFor + SDI.fFor + Alt.SDI.fFor + f(w, model = spde2)   #<-- CHANGED
IM2b.Mesh2 <- inla(f2b.mesh2,
                   family = "gaussian", 
                   data=inla.stack.data(StackFit2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(A = inla.stack.A(StackFit2)))

dic3  <- c(IM2a$dic$dic, IM2b$dic$dic, IM2b.Mesh2$dic$dic)
waic3 <- c(IM2a$waic$waic, IM2b$waic$waic, IM2b.Mesh2$waic$waic)
Z.out     <- cbind(dic3, waic3)
rownames(Z.out) <- c("Gaussian lm", "Gaussian lm + SPDE", "Gaussian lm + SPDE with mesh 2")
Z.out


# ------------------------------------------------------------------------------
# Compare posterior mean values and the 95% credible intervals of the fixed parameters
#
# There only minimal differences between the fixed parts of the tow models.
# This is perhaps an indication that the spatial random effect is not that important
# ------------------------------------------------------------------------------
IM2a$summary.fixed[, c("mean", "0.025quant", "0.975quant")]
IM2b$summary.fixed[, c("mean", "0.025quant", "0.975quant")]

Combined <- rbind(IM2a$summary.fixed[, c("mean", "0.025quant", "0.975quant")],
                  IM2b$summary.fixed[, c("mean", "0.025quant", "0.975quant")]
)
Combined$WhichModel <- rep(c("lm", "spatial lm"), each = 8)
Combined$WhichVariable <- rep(rownames(IM2a$summary.fixed), 2)
colnames(Combined) <- c("Mean", "Lo", "Up", "WhichModel", "WhichVariable")
Combined

p <- ggplot()
p <- p + geom_point(data = Combined, aes(x = WhichModel, y = Mean))
p <- p + geom_errorbar(data = Combined, aes(x = WhichModel,  ymax = Up,  ymin = Lo),  width=0.2)
p <- p + geom_hline(yintercept = 0, linetype = 2) 
p <- p + xlab("Parameters") + ylab("Values")
p <- p + theme(text = element_text(size=15)) 
p <- p + facet_wrap( ~ WhichVariable, scales = "free_y")
p <- p + theme(legend.position="none") 
p


# ------------------------------------------------------------------------------
# Hyperparameters
#
# We have strong spatial correlation up to about 40km.
# r = 106.80 km, meaning that spatial dependency diminishes for distance larger than 106.80km.
# ------------------------------------------------------------------------------
SpFi.w <- inla.spde2.result(inla = IM2b, name = "w", spde = spde, do.transfer = TRUE)
names(SpFi.w)

Kappa <- inla.emarginal(function(x) x, SpFi.w$marginals.kappa[[1]] )
sigmau <- inla.emarginal(function(x) sqrt(x), SpFi.w$marginals.variance.nominal[[1]] )
r <- inla.emarginal(function(x) x, SpFi.w$marginals.range.nominal[[1]] )
Kappa
sigmau
r

# Show correlation structure:  Cor.M versus Distance
D     <- as.matrix(dist(mesh5$loc[,1:2]))
d.vec <- seq(0, max(D), length = 100)      
Cor.M <- (Kappa * d.vec) * besselK(Kappa * d.vec, 1) 
Cor.M[1] <- 1

par(mfrow=c(1,1), mar = c(5,5,2,2))
plot(x = d.vec, y = Cor.M, pch = 16, type = "l",  cex.lab = 1.5, xlab = "Distance (km)",  ylab = "Correlation", xlim = c(0, 200))


# # 95% credible interval of Cor.M versus Distance
packages <- c("tidyr", "DT", "tibble", "gridExtra", "rgdal", "ggmap", "rgeos")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)
correlation <- expand.grid(level = c(0.5, 0.025, 0.975), distance = pretty(c(0, max(D)), 200)) %>%
  mutate(kappa = inla.qmarginal(level, SpFi.w$marginals.kappa$kappa.1), correlation = ifelse(distance == 0, 1, kappa * distance * besselK(kappa * distance, 1)),
    level = factor(level, levels = c(0.5, 0.025, 0.975), labels = c("median", "lcl", "ucl"))) %>%
  select(-kappa) %>% spread(level, correlation)

ggplot(correlation, aes(x = distance, y = median, ymin = lcl, ymax = ucl)) + geom_ribbon(alpha = 0.1) + geom_line() + ylab("Correlation") + coord_cartesian(ylim = 0:1)


# ------------------------------------------------------------------------------
# Spatial random field
# --> Posterior mean values of the spatial field
# ------------------------------------------------------------------------------
# each value (731) in w.pm belongs to a specific vertex on mesh 5
w.pm <- IM2b$summary.random$w$mean  #mu.srf 

# project the 731 posterior mean values on this grid
w.proj <- inla.mesh.projector(mesh5, xlim = range(Loc[,1]), ylim = range(Loc[,2])) 
# Or
w.proj <- inla.mesh.projector(mesh5) 


# 100 * 100 dimentions, a projection (interpolation and extrapolation) of the random field w.
# default a lattice of 100 by 100
# the boundaries of this lattice are given by the extremens of the observed coordinates
w.pm100_100 <- inla.mesh.project(w.proj, w.pm)


# Posterior mean spatial random field ("levelplot")
grid <- expand.grid(x = w.proj$x, y = w.proj$y)
grid$z <- as.vector(w.pm100_100)          

library(grid)   
head(grid)  
levelplot(z ~ x * y,
          data = grid, 
          aspect = "iso",
          scales = list(draw = TRUE),
          xlab = list("Easting (km)", cex = 1.5),
          ylab = list("Northing (km)", cex = 1.5),
          main = list("Posterior mean spatial random field", cex = 1.5),
          panel=function(...){
            panel.levelplot(...)
            grid.points(x = data$EastingKM, 
                        y = data$NorthingKM, 
                        pch = 1,
                        size = unit(0.5, "char"))}  )


# Posterior mean spatial random field ("ggplot2")
w.pm        <- IM2b$summary.random$w$mean  #mu.srf 
w.proj      <- inla.mesh.projector(mesh5) 
w.pm100_100 <- inla.mesh.project(w.proj, w.pm)
long        <- as.data.frame(w.proj$lattice$loc)
colnames(long) <- c("EastingKM", "NorthingKM")
long <- long %>% mutate(w.pm = as.vector(w.pm100_100))
ggplot(long, aes(x = EastingKM, y = NorthingKM,  fill = w.pm, z = w.pm)) + geom_tile() + xlab("Easting (km)") + ylab("Northing (km)") +
  geom_contour(binwidth = 0.1,  colour = "black"
               #aes(linetype = factor(abs(..level..)))
               ) + scale_fill_gradient2() 


# ------------------------------------------------------------------------------
# Model selection
# ------------------------------------------------------------------------------
f3 <- y ~ -1 + Intercept + Alt  + SDI + fFor + Alt.SDI + Alt.fFor + SDI.fFor + f(w, model = spde)

f4a <- y ~ -1 + Intercept + Alt + SDI + fFor + Alt.fFor + SDI.fFor + f(w, model = spde)
f4b <- y ~ -1 + Intercept + Alt + SDI + fFor + Alt.SDI + SDI.fFor + f(w, model = spde)
f4c <- y ~ -1 + Intercept + Alt + SDI + fFor + Alt.SDI + Alt.fFor +  f(w, model = spde)

f5 <- y ~ -1 + Intercept + Alt + SDI + fFor +  Alt.fFor + SDI.fFor + f(w, model = spde)
f5a <- y ~ -1 + Intercept + Alt  + SDI + fFor + SDI.fFor + f(w, model = spde)
f5b <- y ~ -1 + Intercept + Alt + SDI + fFor +  Alt.fFor + f(w, model = spde)

f6 <- y ~ -1 + Intercept + Alt + SDI + fFor + Alt.fFor + f(w, model = spde)
f6a <- y ~ -1 + Intercept + Alt  + fFor +  Alt.fFor + f(w, model = spde)
f6b <- y ~ -1 + Intercept + Alt + SDI + fFor + f(w, model = spde)

X3 <- data.frame(Alt = Xm[,2], SDI = Xm[,3], fFor = Xm[,4], Alt.SDI = Xm[,5], Alt.fFor = Xm[,6], SDI.fFor = Xm[,7])
X4a <- data.frame(Alt = Xm[,2], SDI = Xm[,3], fFor = Xm[,4], Alt.fFor = Xm[,6], SDI.fFor = Xm[,7])
X4b <- data.frame(Alt = Xm[,2], SDI = Xm[,3], fFor = Xm[,4], Alt.SDI = Xm[,5], SDI.fFor = Xm[,7])
X4c <- data.frame(Alt = Xm[,2], SDI = Xm[,3], fFor = Xm[,4], Alt.SDI = Xm[,5], Alt.fFor = Xm[,6])
X5 <- data.frame(Alt = Xm[,2], SDI = Xm[,3], fFor = Xm[,4], Alt.fFor = Xm[,6], SDI.fFor = Xm[,7])
X5a <- data.frame(Alt = Xm[,2], SDI = Xm[,3], fFor = Xm[,4], SDI.fFor = Xm[,7])
X5b <- data.frame(Alt = Xm[,2], SDI = Xm[,3], fFor = Xm[,4], Alt.fFor = Xm[,6])
X6 <- data.frame(Alt = Xm[,2], SDI = Xm[,3], fFor = Xm[,4], Alt.fFor = Xm[,6])
X6a <- data.frame(Alt = Xm[,2], fFor = Xm[,4], Alt.fFor = Xm[,6])
X6b <- data.frame(Alt = Xm[,2], SDI = Xm[,3], fFor = Xm[,4])

StackFit3 <- inla.stack(tag = "Fit", data = list(y = data$pH), A = list(1, 1, A5),  effects = list(Intercept = rep(1, N), X = X3, w = w.index))
StackFit4a <- inla.stack(tag = "Fit", data = list(y = data$pH), A = list(1, 1, A5),  effects = list(Intercept = rep(1, N), X = X4a, w = w.index))
StackFit4b <- inla.stack(tag = "Fit", data = list(y = data$pH), A = list(1, 1, A5),  effects = list(Intercept = rep(1, N), X = X4b, w = w.index))
StackFit4c <- inla.stack(tag = "Fit", data = list(y = data$pH), A = list(1, 1, A5),  effects = list(Intercept = rep(1, N), X = X4c, w = w.index))
StackFit5 <- inla.stack(tag = "Fit", data = list(y = data$pH), A = list(1, 1, A5),  effects = list(Intercept = rep(1, N), X = X5, w = w.index))
StackFit5a <- inla.stack(tag = "Fit", data = list(y = data$pH), A = list(1, 1, A5),  effects = list(Intercept = rep(1, N), X = X5a, w = w.index))
StackFit5b <- inla.stack(tag = "Fit", data = list(y = data$pH), A = list(1, 1, A5),  effects = list(Intercept = rep(1, N), X = X5b, w = w.index))
StackFit6 <- inla.stack(tag = "Fit", data = list(y = data$pH), A = list(1, 1, A5),  effects = list(Intercept = rep(1, N), X = X6, w = w.index))
StackFit6a <- inla.stack(tag = "Fit", data = list(y = data$pH), A = list(1, 1, A5),  effects = list(Intercept = rep(1, N), X = X6a, w = w.index))
StackFit6b <- inla.stack(tag = "Fit", data = list(y = data$pH), A = list(1, 1, A5),  effects = list(Intercept = rep(1, N), X = X6b, w = w.index))

IM3 <- inla(f3, family = "gaussian",  data=inla.stack.data(StackFit3), control.compute = list(dic = TRUE), control.predictor = list(A = inla.stack.A(StackFit3)))
IM4a <- inla(f4a, family = "gaussian",  data=inla.stack.data(StackFit4a), control.compute = list(dic = TRUE), control.predictor = list(A = inla.stack.A(StackFit4a)))
IM4b <- inla(f4b, family = "gaussian",  data=inla.stack.data(StackFit4b), control.compute = list(dic = TRUE), control.predictor = list(A = inla.stack.A(StackFit4b)))
IM4c <- inla(f4c, family = "gaussian",  data=inla.stack.data(StackFit4c), control.compute = list(dic = TRUE), control.predictor = list(A = inla.stack.A(StackFit4c)))
IM5 <- inla(f5, family = "gaussian",  data=inla.stack.data(StackFit5), control.compute = list(dic = TRUE), control.predictor = list(A = inla.stack.A(StackFit5)))
IM5a <- inla(f5a, family = "gaussian",  data=inla.stack.data(StackFit5a), control.compute = list(dic = TRUE), control.predictor = list(A = inla.stack.A(StackFit5a)))
IM5b <- inla(f5b, family = "gaussian",  data=inla.stack.data(StackFit5b), control.compute = list(dic = TRUE), control.predictor = list(A = inla.stack.A(StackFit5b)))
IM6 <- inla(f6, family = "gaussian",  data=inla.stack.data(StackFit6), control.compute = list(dic = TRUE), control.predictor = list(A = inla.stack.A(StackFit6)))
IM6a <- inla(f6a, family = "gaussian",  data=inla.stack.data(StackFit6a), control.compute = list(dic = TRUE), control.predictor = list(A = inla.stack.A(StackFit6a)))
IM6b <- inla(f6b, family = "gaussian",  data=inla.stack.data(StackFit6b), control.compute = list(dic = TRUE), control.predictor = list(A = inla.stack.A(StackFit6b)))

IM2b$dic$dic
IM3$dic$dic
IM4$dic$dic
IM4a$dic$dic
IM4b$dic$dic
IM4c$dic$dic
IM5$dic$dic
IM5a$dic$dic
IM5b$dic$dic
IM6$dic$dic
IM6a$dic$dic
IM6b$dic$dic


# ------------------------------------------------------------------------------
# Model Validation
# ------------------------------------------------------------------------------
Fit6.a <- IM6$summary.fitted.values[1:210,"mean"]
FitIndex <- inla.stack.index(StackFit, tag = "Fit")$data
Fit6.a <- IM6$summary.fitted.values[FitIndex,"mean"]

E6 <- data$pH - Fit6.a
plot(x = Fit6.a, y = E6)


# ------------------------------------------------------------------------------
# Model interpretation
# ------------------------------------------------------------------------------
# fixed part
Out6 <- IM6$summary.fixed
print(Out6, digits = 2)

# precision and variance
tau <- IM6$marginals.hyperpar$`Precision for the Gaussian observations`
sigma <- inla.emarginal(function(x) (1/sqrt(x)), tau)
sigma

# 
range(data$LogAlt)          
range(data$SDI)          
MyData <- expand.grid( 
  LogAlt    = seq(1.47, 2.94, length = 25),
  SDI       = seq(6.3, 74.25, length = 25),
  fForested = levels(data$fForested))
dim(MyData)
head(MyData)

Xmm <- model.matrix(~ LogAlt + SDI + fForested + LogAlt : fForested,  data = MyData)
Xp <- data.frame(Alt = Xmm[,2], SDI = Xmm[,3], fFor = Xmm[,4], Alt.fFor = Xmm[,5])
StackCov <- inla.stack(tag = "Covariates", data = list(y = NA), A = list(1, 1),  effects = list(Intercept = rep(1, nrow(Xp)), Xp = Xp))
All.stacks <- inla.stack(StackFit, StackCov)	              

f7 <- y ~ -1 + Intercept + Alt  + SDI + fFor +  Alt.fFor + f(w, model = spde)
IM7 <- inla(f7, family = "gaussian",  data=inla.stack.data(All.stacks), control.compute = list(dic = TRUE), control.predictor = list(A = inla.stack.A(All.stacks)))

StackFit <- inla.stack(tag = "Fit", data = list(y = data$pH), A = list(1, 1, A5), effects = list(Intercept = rep(1, N), X = X, w = w.index))


# A5 is the projector matrix that links the ws at the 731 vertices to the spatial random intercept u at the 210 sampling locations:  210 * 731
# each row i in A5 contains weighting factors aik, the sum of these weighting factors equals 1
#  - 0:  519 columns  1:  160 columns   not 0 not 1:  33 columns
dim(A5)
table(colSums(A5) > 0)
table(colSums(A5) == 1)
table(colSums(A5) > 1)


dim(inla.stack.A(StackFit))    #210 by 423
#1 intercept + 210 + 212 non-zero colmns 
dim(inla.stack.A(StackCov))	   #1250 1251              
dim(inla.stack.A(All.stacks))  #1460 1673   
dim(IM7$summary.fitted.values) #3133 by 6

colSums(inla.stack.A(StackFit) == 0)
index.Fit <- inla.stack.index(All.stacks, tag = "Fit")$data
index.Cov <- inla.stack.index(All.stacks, tag = "Covariates")$data
F7a <- IM7$summary.fitted.values[index.Fit, c(1,3,5)]  #210 by 3
F7b <- IM7$summary.fitted.values[index.Cov, c(1,3,5)]  #210 by 3


MyData2 <- cbind(MyData, F7b)
dim(MyData2)
colnames(MyData2)

for (i in 1: 300){
  p <- wireframe(mean ~ LogAlt + SDI,  data = MyData2, group = fForested,
                 #zlim = c(0,1),
                 shade = TRUE, scales = list(arrows = FALSE), drape = TRUE,  colorkey = FALSE, screen = list(z = i, x = -60 - i /10))
  print(p)
}

#Pick on value of i
i <- 20
p <- wireframe(mean ~ LogAlt + SDI,  data = MyData2, group = fForested,
               shade = FALSE, scales = list(arrows = FALSE), drape = FALSE,  colorkey = FALSE, screen = list(z = i, x = -60 - i /5))
print(p)




###############################################
#12.16.5 Second approach to calculate fitted values


Astk <- as.matrix(inla.stack.A(All.stacks))
dim(Astk)

F7c <- Astk %*%  IM7$summary.fitted.values[1461:3132, "mean"]
length(F7c)

Fit7 <- cbind(F7a[,"mean"], F7c[index.Fit])
head(Fit7)

Pred <- cbind(F7b[,"mean"], F7c[index.Cov])
head(Pred)











# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
library(rgdal)

long_meter <- long %>% filter(!is.na(w.pm)) %>% transmute(Easting = EastingKM * 1000, Northing = NorthingKM * 1000, w.pm)
long_sp <- SpatialPointsDataFrame(coords = long_meter[, 1:2], data = long_meter[, 3, drop = FALSE], proj4string = CRS("+init=epsg:29900")) %>%
  spTransform(CRS("+proj=longlat")) %>% as.data.frame() %>% transmute(lon = Easting, lat = Northing, w.pm)


# long_sp mapped on Ireland map
ireland <- geocode("Ireland")
ireland_map <- get_map(ireland, zoom = 7)
ggmap(ireland_map) + geom_point(data = long_sp, aes(colour = w.pm), alpha = 0.5) + scale_colour_gradient2()


# countour lines
cl <- contourLines(x = w.proj$x * 1000, y = w.proj$y * 1000, z = w.pm100_100, nlevels = 40)
cl <- lapply(seq_along(cl), function(i) {cbind(cl[[i]]$x, cl[[i]]$y) %>% Line() %>% list() %>% Lines(ID = i) }) %>%
  SpatialLines(proj4string = CRS("+init=epsg:29900")) %>% SpatialLinesDataFrame(data.frame(w.pm = sapply(cl, "[[", "level"))) %>% spTransform(CRS("+proj=longlat"))
cl <- fortify(cl) %>% rename(lon = long) %>% mutate(w.pm = cl$w.pm[group])
ggmap(ireland_map) + geom_path(data = cl, aes(colour = w.pm, group = group)) + scale_colour_gradient2()


# 
w.mat <- IM2b$summary.random$w$mean %>% as.matrix(nrow = 1)
data_sp <- data %>% select(lon = Easting, lat = Northing) %>% SpatialPoints(proj4string = CRS("+init=epsg:29900")) %>% spTransform(CRS("+proj=longlat")) %>%
  as.data.frame() %>% mutate(uvec = as.vector(A5 %*% w.mat), Sign = uvec >= 0, u = abs(uvec))
ggmap(ireland_map) + geom_path(data = cl, aes(colour = w.pm, group = group)) + geom_point(data = data_sp, aes(size = u, shape = Sign)) + scale_colour_gradientn(colours = terrain.colors(20))


###########################
ukgrid = "+init=epsg:29902"
latlong = "+init=epsg:4326"

GP <- cbind(grid$x,  grid$y)
GP$GP_ID <- 1:nrow(GP)

# Create coordinates variable
coords <- cbind(Easting = as.numeric(as.character( grid$x)), Northing = as.numeric(as.character( grid$y)))

# Create the SpatialPointsDataFrame
GP_SP <- SpatialPointsDataFrame(coords, data = data.frame(grid$z, GP$GP_ID), proj4string = CRS("+init=epsg:29902"))

plot(GP_SP)
head(GP_SP@coords)

GP_SP_LL <- spTransform(GP_SP, CRS(latlong))
colnames(GP_SP_LL@coords)[colnames(GP_SP_LL@coords) == "Easting"] <- "Longitude"
colnames(GP_SP_LL@coords)[colnames(GP_SP_LL@coords) == "Northing"] <- "Latitude"
head(GP_SP_LL@coords)
plot(GP_SP_LL@coords)

range(GP_SP_LL@coords[,1])
range(GP_SP_LL@coords[,2])
grid$Lat <- GP_SP_LL@coords[,2]
grid$Lon <- GP_SP_LL@coords[,1]

levelplot(z ~ Lat * Lon,
          data = grid, 
          aspect = "iso",
          scales = list(draw = TRUE),
          xlab = list("Longitude", cex = 1.5),
          ylab = list("Latitude", cex = 1.5),
          main = list("Posterior mean spatial random field", cex = 1.5),
          panel=function(...){
            panel.levelplot(...)
            grid.points(x = data$Longitude, 
                        y = data$Latitude, 
                        pch = 1,
                        size = unit(0.5, "char"))
            panel.lines(x = fSF2$long,
                        y = fSF2$lat)                                   
          }  )
range(fSF2$long)
range(fSF2$lat)

library(rgdal)
DSN <- "/Users/highstat/applicat/HighlandStatistics/Books/BGS/SpatialTemp/RCode/Chapter13/ie2/Ireland.shp"

SF <- readOGR(dsn = DSN, layer = "Ireland")
names(SF)
SF$NAME
SF$AREA
SF$PERIMETER
SF2 <- subset(SF, NAME == "Republic of Ireland" & group == "0.1")
plot(SF2)

fSF2 <- fortify(SF2)
head(fSF2)

levelplot(z ~ x * y,
          data = grid, 
          aspect = "iso",
          scales = list(draw = TRUE),
          xlab = list("Easting (km)", cex = 1.5),
          ylab = list("Northing (km)", cex = 1.5),
          main = list("Posterior mean spatial random field", cex = 1.5),
          panel=function(...){
            panel.levelplot(...)
            grid.points(x = data$Easting / 1000, 
                        y = data$Northing / 1000, 
                        pch = 1,
                        size = unit(0.5, "char"))}  )

plot(fSF2$long, fSF2$lat, type = "p")

poly <- SF2  
writeOGR(poly, dsn = '.', layer = 'Ire', driver = "ESRI Shapefile")  

DSN <- "/Users/highstat/applicat/HighlandStatistics/Books/BGS/SpatialTemp/RCode/Chapter13/ie3/Ire.shp"
SF <- readOGR(dsn = DSN, layer = "Ire")
plot(SF)

names(SF)
SF$NAME

IE_utm <- spTransform(SF, CRS("+proj=utm +zone=29N"))
plot(IE_utm)

fIE <- fortify(IE_utm)

names(fIE) 
head(fIE)
fIE$group

xyplot(lat ~ long | group,  data =  fIE,  type = "l") 

fIE2 <- subset(fIE, group == "0.1")

xyplot(lat ~ long, data =  fIE2, aspect = "iso", type = "l") 



pts <- fIE2[, c("long" , "lat")] 
coordinates(pts) <- c("long", "lat") 
migmap <- gmap(x = pts, type = "hybrid", zoom = 5)
plot(migmap)
head(fIE2)










# ------------------------------------------------------------------------------
# Visualizing spatial field by ggplots
# ------------------------------------------------------------------------------
library(reshape2)
xygrid <- expand.grid(w.proj$x, w.proj$y)
Data3D <- data.frame(x = xygrid[,1], y = xygrid[,2], z = melt(w.pm100_100)[,3])
names(Data3D) <- c("x", "y", "z")
# x and y are the spatial positions on a 100 by 100 grid.
# z contains the corresponding mean spatial field.
p <- ggplot(Data3D, aes(x, y, z = z), col = rgb(1, 0.5, 0.5, 0.7))
p <- p + stat_contour(geom="polygon", aes(fill = ..level..))
p <- p + geom_tile(aes(fill = z))
p <- p + stat_contour(geom="polygon", aes(fill = ..level..))
p <- p + stat_contour(aes(colour = ..level..))
p <- p + xlab("X km") + ylab("Y km") 
p         


# Plot results in the map of Ireland

# Convert Easting and Northing to Longitude and Latitude  
coords <- cbind(Easting = data$Easting, Northing = data$Northing)
IP_SP <- SpatialPointsDataFrame(coords, data = data.frame(data$pH, data$ID), proj4string = CRS("+init=epsg:29902"))
IP_SP_LL <- spTransform(IP_SP, CRS("+init=epsg:4322"))

colnames(IP_SP_LL@coords)[colnames(IP_SP_LL@coords) == "Easting"] <- "Longitude"
colnames(IP_SP_LL@coords)[colnames(IP_SP_LL@coords) == "Northing"] <- "Latitude"    

data$Longitude <- IP_SP_LL@coords[,1]  
data$Latitude  <- IP_SP_LL@coords[,2]  

# - Big points for big residuals 
# - Small dots for small residuals
# - Different colour for positive and negative residuals 
# The w.pm below is the spatial field at the sampling locations.

xy     <- cbind(data$Longitude, data$Latitude)
w.proj <- inla.mesh.projector(mesh5, loc =  Loc )
w.pm   <- inla.mesh.project(w.proj, IM2b$summary.random$w$mean)


# Use different font sizes and colours dependening on the values of g.mean
MyCex <- 2 * abs(w.pm) / max(w.pm)
SignResidual <- as.numeric(w.pm >=0) + 1
MyCol <- c("red", "yellow")[SignResidual] 
#Yellow residual is a positive residual
#Red residual is negative   


packages <- c("raster", "dismo", "rasterVis")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

pts <- Data3D 
coordinates(pts) <- c("x", "y") 
r1 <- rasterFromXYZ(pts)
projection(r1) <- paste("+init=epsg:29900")
plot(r1) #This is the spatial field again

# We can put that in gmap...and let gmap determine in which
# part of the world we are.

#Ireland
migmap <- gmap(x = r1, type = "terrain", zoom = 7)
plot(migmap)


# Now we need some Mercator magic to convert coordinates
# from one system to another system. The reason for this
# is that we will project the spatial field on top
# of a gmap graph, which requires Mercator coordinates.
rprobGM <- projectRaster(from = r1, crs = CRS("+init=epsg:3857"))



# The study area with the sampling locations
plot(migmap)
points(Mercator(xy), col = MyCol, pch = c(16,17)[SignResidual], cex = MyCex)
# Note the correlation in the residuals! The spatial field
# picks up a East - West patterm? Rain from the Atlantic?

# The main problem here is that the gmap is not defined
# in latitude and longitude but in Mercator coordinates.


# And we also want to plot the entire surface on top of the gmap.
# This means that we also have to convert the  spatial field to Mercator coordinates. 
plot(migmap)
points(Mercator(xy), col = 1, pch = c(16,17)[SignResidual], cex = MyCex)
plot(rprobGM, add = T, legend = F, col = cm.colors(5, alpha = 0.6))
# This is the spatial field converted in to 
# Mercator coordinates
# We only need to do some cropping to remove
# the extrapollated field from the sea.
####################################








