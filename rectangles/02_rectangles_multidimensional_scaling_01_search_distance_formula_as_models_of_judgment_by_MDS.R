setwd("//media//kswada//MyFiles//R//rectangles")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  rectangles
# ------------------------------------------------------------------------------

data(rectangles, package = "smacof")


str(rectangles)


car::some(rectangles)



# ------------------------------------------------------------------------------
# Examine distance formulas as models of judgment by MDS
# ------------------------------------------------------------------------------

# various distance measures (use object of class "dist")
diss_mx <- dist(rectangles, method = "maximum")

diss_mh <- dist(rectangles, method = "manhattan")

diss_eu <- dist(rectangles, method = "euclidean")

diss_ca <- dist(rectangles, method = "canberra")



# ----------
# multidimensional scaling by ordinal type
res_mx <- mds(delta = diss_mx, type = "ordinal")
# res_mx <- mds(delta = diss_mx, type = "ordinal", init = rect_constr)

res_mh <- mds(delta = diss_mh, type = "ordinal")
# res_mh <- mds(delta = diss_mh, type = "ordinal", init = rect_constr)

res_eu <- mds(delta = diss_eu, type = "ordinal")
# res_eu <- mds(delta = diss_eu, type = "ordinal", init = rect_constr)

res_ca <- mds(delta = diss_ca, type = "ordinal")
# res_ca <- mds(delta = diss_ca, type = "ordinal", init = rect_constr)



# ----------
graphics.off()
par(mfrow = c(2,2))

conf <- res_mx$conf
plot(res_mx, main = "maximum", cex = 2)
lines(conf[1:4,]);          lines(conf[5:8,]); lines(conf[9:12,]); 
lines(conf[13:16,]);        lines(conf[c(1,5,9,13),]); 
lines(conf[c(2,6,10,14),]); lines(conf[c(3,7,11,15),]); 
lines(conf[c(4,8,12,16),])

conf <- res_mh$conf
plot(res_mh, main = "manhattan", cex = 2)
lines(conf[1:4,]);          lines(conf[5:8,]); lines(conf[9:12,]); 
lines(conf[13:16,]);        lines(conf[c(1,5,9,13),]); 
lines(conf[c(2,6,10,14),]); lines(conf[c(3,7,11,15),]); 
lines(conf[c(4,8,12,16),])

conf <- res_eu$conf
plot(res_eu, main = "euclidean", cex = 2)
lines(conf[1:4,]);          lines(conf[5:8,]); lines(conf[9:12,]); 
lines(conf[13:16,]);        lines(conf[c(1,5,9,13),]); 
lines(conf[c(2,6,10,14),]); lines(conf[c(3,7,11,15),]); 
lines(conf[c(4,8,12,16),])

conf <- res_ca$conf
plot(res_ca, main = "canberra", cex = 2)
lines(conf[1:4,]);          lines(conf[5:8,]); lines(conf[9:12,]); 
lines(conf[13:16,]);        lines(conf[c(1,5,9,13),]); 
lines(conf[c(2,6,10,14),]); lines(conf[c(3,7,11,15),]); 
lines(conf[c(4,8,12,16),])



# ----------
graphics.off()
par(mfrow = c(2,2))

plot(res_mx, plot.type = "Shepard")
plot(res_mh, plot.type = "Shepard")
plot(res_eu, plot.type = "Shepard")
plot(res_ca, plot.type = "Shepard")



# ----------
# Stress-1 values

res_mx$stress
res_mh$stress
res_eu$stress
res_ca$stress



# -->
# MDS by maximum distance seems to closest to the original configurations, but Stress-1 values are highest.

# Borg and Leutner examined this experiment by MDS by Manhattan distance (city-block distances)

# Also note that if one allows for some rescaling of the width and height coordinates of the rectangles,
# one can fit the design configuration quite well to the MDS configuration.
# The rescaling also makes psychological sense:  It exhibits a logarithmic shrinkage of the grid lines from left to right
# and from bottom to top, as expected by psychophysical theory.

# The deviasions of the rescaled design grid from the MDS configuration do not seem to be systematic.
# Hence, one may conclude that the subjects did indeed generate their ratings by a composition rule descrived by city-block
# distance formula (including a logarithmic rescaling of intra-dimensional distances according to the Weber-Fechner law).
# The MDS solution also shows that differences in the rectangles' heights are psychologically more important for similarity judgments
# than differences in the rectangles' widths.


