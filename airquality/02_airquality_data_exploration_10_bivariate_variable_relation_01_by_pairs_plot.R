setwd("//media//kswada//MyFiles//R//airquality")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  airquality
# ------------------------------------------------------------------------------

data("airquality")


str(airquality)

car::some(airquality)


airquality2 <- na.exclude(airquality)



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

formula <- ~ Ozone + Solar.R + Wind + Temp


scatterplotMatrix(formula, data = airquality,
                  smooth = FALSE,
                  id = list(n = 3), ellipse = TRUE, col = gray(0.3), pch = 20)



# ----------
# by group

formula <- ~ Ozone + Solar.R + Wind + Temp

scatterplotMatrix(formula, data = airquality, groups = airquality$Month,
                  smooth = FALSE, ellipse = list(levels = 0.5), 
                  id = list(n = 3), col = c(gray(0.6), "black", "blue", "red", "green"), pch = c(1, 20))



# -->
# by month, the relationships between temperature and ozone are different.
# Important !!!:  Ozone and Wind has different relationship by month




# ------------------------------------------------------------------------------
# Data Exploration:  Marginal plots by psych::describe
# show all bivariate marginal relations
#
#   - stars:  shows the significance of correlations
# ------------------------------------------------------------------------------

library(psych)


var_obj <- c("exra", "year", "nao", "elevation")


# here we apply method = "spearman" due to terrible skewness
pairs.panels(swer[, var_obj], ci = TRUE, smoother = TRUE, stars = TRUE, method = "spearman")




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


MyVar <- c("Solar.R", "Wind", "Temp")


Myxyplot(airquality, MyVar, "Ozone")

