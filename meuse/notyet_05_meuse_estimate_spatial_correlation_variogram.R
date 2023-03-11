setwd("//media//kswada//MyFiles//R//meuse")

packages <- c("dplyr", "maptools", "spdep", "RColorBrewer", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


pal = function(n = 9) brewer.pal(n, "Reds")



# ------------------------------------------------------------------------------
# data:  meuse
# ------------------------------------------------------------------------------
library(sp)

data(meuse)
data(meuse.grid)


# ----------
coordinates(meuse) <- c("x", "y")
coordinates(meuse.grid) <- c("x", "y")
meuse.grid <- as(meuse.grid, "SpatialPixelsDataFrame")


# ----------
head(meuse)
head(meuse.grid)



# ------------------------------------------------------------------------------
# Exploratory variogram analysis:  lagged scatterplots
#   - scatter plots of pairs Z(s(i)) and Z(s(j)), groupd according to their separation distance h(ij) = || s(i) - s(j) ||
# ------------------------------------------------------------------------------

hscat(log(zinc) ~ 1, meuse, (0:9) * 100, pch = 3, cex = .6, col = 'grey')




# ------------------------------------------------------------------------------
# Exploratory variogram analysis:  variogram and variogram cloud
# ------------------------------------------------------------------------------
library(gstat)
cld <- variogram(log(zinc) ~ 1, meuse, cloud = TRUE)
svgm <- variogram(log(zinc) ~ 1, meuse)
d <- data.frame(gamma = c(cld$gamma, svgm$gamma),
                dist = c(cld$dist, svgm$dist),
                id = c(rep("cloud", nrow(cld)), rep("sample variogram", nrow(svgm)))
)
xyplot(gamma ~ dist | id, d,
       scales = list(y = list(relation = "free", 
                              #ylim = list(NULL, c(-.005,0.7)))),
                              limits = list(NULL, c(-.005,0.7)))),
       layout = c(1, 2), as.table = TRUE,
       panel = function(x,y, ...) {
         if (panel.number() == 2)
           ltext(x+10, y, svgm$np, adj = c(0,0.5)) #$
         panel.xyplot(x,y,...)
       },
       xlim = c(0, 1590),
       cex = .5, pch = 3
)


###################################################
### code chunk number 25: geos.Rnw:597-599 (eval = FALSE)
###################################################
## library(gstat)
## variogram(log(zinc) ~ 1, meuse, cloud = TRUE)


###################################################
### code chunk number 26: geos.Rnw:626-628 (eval = FALSE)
###################################################
## sel <- plot(variogram(zinc ~ 1, meuse, cloud = TRUE), digitize = TRUE)
## plot(sel, meuse)


###################################################
### code chunk number 27: geos.Rnw:664-694
###################################################
sel <-
  structure(list(x = c(145.291968730077, 266.107479142605, 320.156523274526,
                       339.232656497557, 323.335878811698, 212.058435010685, 135.753902118561,
                       46.7319470777507, 78.5255024494688, 142.112613192905), y = c(574649.690841889,
                                                                                    581256.265954825, 627502.29174538, 822396.257577002, 1053626.38652977,
                                                                                    1278249.94036961, 1255126.92747433, 792666.669568789, 634108.866858316,
                                                                                    577952.978398357)), .Names = c("x", "y"))
v <- variogram(zinc ~ 1, meuse, cloud = TRUE)
v$gamma <- v$gamma/1e6
sel$y <- sel$y/1e6
p1 <- xyplot(gamma~dist, v,
             panel = function(x, y, ...) {
               panel.xyplot(x, y, ...)
               llines(sel$x, sel$y, col = 'red')
             },
             pch=3, cex = .5, asp = 1, ylab = "gamma (x 1e6)")
x <-
  structure(list(head = c(40, 40, 40, 54, 55, 54, 47, 80, 55, 55,
                          54, 53, 54, 55, 59, 59), tail = c(41, 42, 43, 57, 57, 58, 59,
                                                            99, 121, 122, 123, 125, 125, 125, 125, 132)), .Names = c("head",
                                                                                                                     "tail"), row.names = as.integer(c(NA, 16)), class = c("pointPairs",
                                                                                                                                                                           "data.frame"))
p2 = plot(x, meuse, scales=list(draw=F), col.line = 'red')
print(p1, split = c(1,1,2,1), more = TRUE)
print(p2, split = c(2,1,2,1))


###################################################
### code chunk number 28: geos.Rnw:726-758
###################################################
v <- variogram(log(zinc) ~ 1, meuse)
# INTERACTIVE mode out-commented:
#plot(v, type = 'b', pch = 3)
#fn = function(n = 100) {
#        for (i in 1:n) {
#           meuse$random = sample(meuse$zinc)
#           v = variogram(log(random) ~ 1, meuse)
#           trellis.focus("panel", 1, 1, highlight = FALSE)
#           llines(v$dist, v$gamma, col = 'grey')
#           trellis.unfocus()
#        }
#}
#fn()
#trellis.focus("panel", 1, 1, highlight = FALSE)
#lpoints(v$dist, v$gamma, col = 'black', type = 'b', lwd = 2, pch=3)
#trellis.unfocus()
print(xyplot(gamma ~ dist, v, pch = 3, type = 'b', lwd = 2, col = 'darkblue',
             panel = function(x, y, ...) {
               for (i in 1:100) {
                 meuse$random = sample(meuse$zinc)
                 v = variogram(log(random) ~ 1, meuse)
                 llines(v$dist, v$gamma, col = 'grey')
               }
               panel.xyplot(x, y, ...)
             },
             ylim = c(0, 0.75), xlab = 'distance', ylab = 'semivariance'
))


###################################################
### code chunk number 29: geos.Rnw:770-771 
###################################################
plot(variogram(log(zinc) ~ 1, meuse))


###################################################
### code chunk number 30: geos.Rnw:789-790 
###################################################
plot(variogram(log(zinc) ~ 1, meuse, alpha = c(0, 45, 90, 135)))


###################################################
### code chunk number 31: geos.Rnw:850-851 
###################################################
plot(variogram(log(zinc) ~ 1, meuse, cutoff = 1000, width = 50))


###################################################
### code chunk number 32: geos.Rnw:866-867 
###################################################
variogram(log(zinc) ~ 1, meuse, boundaries = c(0,50,100,seq(250,1500,250)))


###################################################
### code chunk number 33: geos.Rnw:903-905 
###################################################
show.vgms()
show.vgms(model = "Mat", kappa.range = c(.1, .2, .5, 1, 2, 5, 10), max = 10)


###################################################
### code chunk number 34: geos.Rnw:931-937
###################################################
vgm(1, "Sph", 300)
vgm(1, "Sph", 300, 0.5)
v1 <- vgm(1, "Sph", 300, 0.5)
v2 <- vgm(0.8, "Sph", 800, add.to = v1)
v2
vgm(0.5, "Nug", 0)


###################################################
### code chunk number 35: geos.Rnw:962-963
###################################################
vgm()


###################################################
### code chunk number 36: geos.Rnw:984-986 
###################################################
v <- variogram(log(zinc) ~ 1, meuse)
plot(v)


###################################################
### code chunk number 37: geos.Rnw:1002-1003
###################################################
fit.variogram(v, vgm(1, "Sph", 800, 1))


###################################################
### code chunk number 38: geos.Rnw:1017-1018
###################################################
fit.variogram(v, vgm(1, "Sph", 10, 1))


###################################################
### code chunk number 39: geos.Rnw:1032-1035 (eval = FALSE)
###################################################
## v.fit <- fit.variogram(v, vgm(1, "Sph", 10, 1))
## if (attr(v.fit, "singular"))
##     stop("singular fit")


###################################################
### code chunk number 40: geos.Rnw:1050-1081
###################################################
ccol = 'darkblue' #grey(.5)
v <- variogram(log(zinc) ~ 1, meuse)
v.fit <- fit.variogram(v, vgm(1, "Sph", 800, 1))
plot(v, v.fit, pch = 3, panel = function(x,y,subscripts,...) {
  larrows(0,v.fit$psill[1], v.fit$range[2], v.fit$psill[1], 
          col=ccol, ends = 'both', length=.1, angle=15)
  larrows(v.fit$range[2],0, v.fit$range[2], v.fit$psill[1], 
          col=ccol, ends = 'both', length=.1, angle=15)
  larrows(v.fit$range[2],v.fit$psill[1], v.fit$range[2], 
          sum(v.fit$psill), 
          col=ccol, ends = 'both', length=.1, angle=15)
  ltext(v.fit$rang[2]/2, 1.2*v.fit$psill[1], "range", col=ccol,
        adj = c(.5, 0), cex=.9)
  ltext(1.02 * v.fit$rang[2], 0.5 *v.fit$psill[1], "nugget", col=ccol,
        adj = c(0, 0.5), cex=.9)
  ltext(1.02 * v.fit$rang[2], v.fit$psill[1] + 0.5 * v.fit$psill[2], 
        "partial sill", col=ccol, adj = c(0, 0.5), cex=.9)
  vgm.panel.xyplot(x,y,subscripts,...)
}
)



###################################################
### code chunk number 41: geos.Rnw:1106-1107
###################################################
attr(v.fit, "SSErr")


###################################################
### code chunk number 42: geos.Rnw:1150-1153 (eval = FALSE)
###################################################
## library(geoR)
## v.eye <- eyefit(variog(as.geodata(meuse["zinc"]),max.dist=1500))
## ve.fit <- as.vgm.variomodel(v.eye[[1]])


###################################################
### code chunk number 43: geos.Rnw:1185-1186
###################################################
fit.variogram(v, vgm(1, "Sph", 800, 0.06), fit.sills = c(FALSE, TRUE))


###################################################
### code chunk number 44: geos.Rnw:1207-1215
###################################################
v.dir <- variogram(log(zinc)~1,meuse,alpha=(0:3)*45)
v.anis <- vgm(.6, "Sph", 1600, .05, anis=c(45,.3))
print(plot(v.dir, v.anis, pch=3))


###################################################
### code chunk number 45: geos.Rnw:1224-1225
###################################################
fit.variogram.reml(log(zinc)~1, meuse, model=vgm(0.6, "Sph", 800, 0.06))


###################################################
### code chunk number 46: geos.Rnw:1262-1264
###################################################
v.dir <- variogram(log(zinc)~1,meuse,alpha=(0:3)*45)
v.anis <- vgm(.6, "Sph", 1600, .05, anis=c(45, 0.3))


###################################################
### code chunk number 47: geos.Rnw:1266-1267 
###################################################
plot(v.dir, v.anis)
