setwd("//media//kswada//MyFiles//R//gala")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gala
# ------------------------------------------------------------------------------

data("gala", package = "faraway")

str(gala)

dim(gala)


car::some(gala)



# ----------
# mod_obj <- modp.step2
mod_obj <- modp2
# mod_obj <- mod.nbin



# ------------------------------------------------------------------------------
# influential statistics for each case
# ------------------------------------------------------------------------------

infl <- influence.measures(mod_obj)



# ----------
# show influence measures for each case
# The summary() method for the "infl" object prints those observartions considered noteworthy on one or more of these statistics,
# as indicated by a "*" next to the value.

summary(infl)




# ------------------------------------------------------------------------------
# Individual index plot:  DFBETAs
#  - Cook's D and DFFITS are overall measure of the total influence that cases have on the regression coefficients and fitted values, respectively.
#    On the other hand, DFBETAs is the simplest measure of influence of observation i, which is the standardized change in the coefficient for each variable due to omitting that observation
# ------------------------------------------------------------------------------
dfbetas <- data.frame(infl$infmat[,2:5])

head(dfbetas)

colnames(dfbetas) <- c("dfb.lg.Ar.", "dfb.l.N.", "dfb.lg.Ad.", "dfb.Sc.")



# observations are colored blue or red according to ...
var <- "dfb.lg.Ar."

op <- par(mar = c(5, 5, 1, 1) + .1)

cols <- ifelse(gala$Species >= 20, "red", "blue")

plot(dfbetas[,var], type = "h", col = cols, xlab = "Observation index", ylab = expression(delta * beta[var]), cex.lab = 1.3)
points(dfbetas[,var], col = cols)
big = abs(dfbetas[,var]) > .25
idx <- 1:nrow(dfbetas)
text(idx[big], dfbetas[big, var], label = rownames(dfbetas)[big], cex = 0.9, pos = ifelse(dfbetas[big, var] > 0, 3, 1), xpd = TRUE)
abline(h = c(-0.25, 0, .25), col = "gray")

par(op)




# ------------------------------------------------------------------------------
# Individual index plot:  scatterplot matrix of DFBETAs
#   - show the pairwise changes in the regression coefficients for the various predictors
# ------------------------------------------------------------------------------

# method = "mahl" to label the most extreme observations according to the Mahalanobis distance of each point from the centroid in the plot.
car::scatterplotMatrix(dfbetas, smooth = FALSE, id = TRUE, showLabels=list(method = "mahal", n = 2, cex = 1, location = "lr"), 
                       ellipse = TRUE, levels = 0.95, robust = FALSE, diagonal = "histogram", col = gray(0.6))


# -->
# The joint effect of observations on pairs of coefficients is more complex than is apparent from the univariate views that appear in the plots along the diagonal.


