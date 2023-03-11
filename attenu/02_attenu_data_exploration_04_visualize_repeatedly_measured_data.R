setwd("//media//kswada//MyFiles//R//attenu")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  attenu
# ------------------------------------------------------------------------------

data("attenu")


str(attenu)

dim(attenu)


car::some(attenu)




# ------------------------------------------------------------------------------
# data exploration:  visualize repeatedly measured data   by ggplot
# ------------------------------------------------------------------------------

library(ggplot2)

gg <- ggplot(attenu, aes(x = log(dist), y = accel^0.25)) + geom_point() +
  stat_smooth(method = "loess", size = 2, fill = "blue", alpha = 0.25) +
  stat_smooth(method = "lm", color = "red", size = 1.25, se = FALSE) +
  labs(y = "accel^0.25", x = "log(dist)")

gg



# ----------
gg + facet_wrap(~ mag)

gg + facet_wrap(~ ifelse(mag < 6, 5, ifelse(mag < 7, 6, 7)))




# ------------------------------------------------------------------------------
# data exploration:  visualize repeatedly measured data   by coplot
# ------------------------------------------------------------------------------

formula = accel^0.25 ~ log(dist) | ifelse(mag < 6, 5, ifelse(mag < 7, 6, 7))


coplot(formula, data = attenu, ylab = "accel^0.25", xlab = "log(dist)", las=1)



# ------------------------------------------------------------------------------
# Data Exploration:  Marginal plots by scatterplotMatrix
# show all bivariate marginal relations
#
#   - by-group representation
#   - ellipse:  shows SDs and correation direction, the concentration is contolled by "levels"
#   - outliers:  by "id = list(n = x)"
#   - formula type:  can apply data transformation
# ------------------------------------------------------------------------------
# By default:
#   - diagonal panels:  nonparametric density estimates, using an adaptive-kernel estimator, with a rug-plot
#   - solid line:  marginal linear least-squares fit, ignoring the other variables
#   - central broken line:  nonparametric regression smooth
#   - outer broken lines:  smooths of the conditional variation of the y values givne x in each panel, like running quartiles
#   - size of the ellipse in the vertical and horizontal directions reflects the SDs of the two variables. 
# ------------------------------------------------------------------------------
#   - ellipse = list(levels = 0.5):  to get separate 50% concentration ellipses for the groups
#     If the data in a panel are bivariately normally distributed, then the ellipse encloses approximately 50% of the points
# ------------------------------------------------------------------------------


library(car)

formula <- ~ accel + dist

scatterplotMatrix(formula, data = attenu,
                  smooth = FALSE,
                  id = list(n = 3), ellipse = TRUE, col = gray(0.3), pch = 20)



# ----------
# data transformation
tmp <- attenu %>% mutate(accel2 = accel^0.25)

formula <- ~ accel2 + log(dist)

scatterplotMatrix(formula, data = tmp,
                  smooth = FALSE,
                  id = list(n = 3), ellipse = TRUE, col = gray(0.3), pch = 20)


# ----------
# by group

tmp <- attenu %>% mutate(accel2 = accel^0.25)

formula <- ~ accel2 + log(dist) | ifelse(mag < 6, 5, ifelse(mag < 7, 6, 7))

scatterplotMatrix(formula, data = tmp,
                  smooth = FALSE, ellipse = list(levels = 0.5), 
                  id = list(n = 3), col = c(gray(0.4), gray(0.8), "black"), pch = c(1, 2, 20))



# ------------------------------------------------------------------------------
# Data exploration:  Multipanel scatterplot
# ------------------------------------------------------------------------------

Myxyplot <- function(Z, MyV, NameY1, MyXlab = "", MyYlab="") {
  AllX  <- as.vector(as.matrix(Z[,MyV]))
  AllY  <- rep(Z[,NameY1] , length(MyV))
  AllID <- rep(MyV, each = nrow(Z))
  
  library(mgcv)
  library(lattice)
  
  P <- xyplot(AllY ~ AllX|factor(AllID), col = 1,
              xlab = list(MyXlab, cex = 1.5),
              #ylab = list("Response variable", cex = 1.5),
              #ylab = list("Pearson residuals", cex = 1.5),
              ylab = list(MyYlab, cex = 1.5),
              #layout = c(2,2),   #Modify
              strip = function(bg='white', ...) strip.default(bg='white', ...),
              scales = list(alternating = T, x = list(relation = "free"), y = list(relation = "same")),
              panel=function(x, y){
                panel.grid(h=-1, v= 2)
                panel.points(x, y, col = 1)
                panel.loess(x, y, span = 0.8,col = 1, lwd = 2)
              })
  
  print(P)
}


tmp <- attenu %>% mutate(accel2 = accel^0.25, dist2 = log(dist), mag2 = ifelse(mag < 6, 5, ifelse(mag < 7, 6, 7)))


MyVar <- c("dist2", "mag2")

Myxyplot(tmp, MyVar, "accel2")

