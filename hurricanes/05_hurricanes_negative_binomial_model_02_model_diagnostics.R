setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\hurricanes")

packages <- c("dplyr")

purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Hurricanes
# ------------------------------------------------------------------------------

# data("Hurricanes", package = "rethinking")

data <- read.csv(file = "Hurricanes.txt", header = T, sep = "\t")


dim(data)


str(data)



car::some(data)




# ------------------------------------------------------------------------------
# model diagnostics
# ------------------------------------------------------------------------------

car::residualPlots(mod.nbin, groups = data$female)




# ----------
par(mfrow = c(1,1))

car::qqPlot(rstandard(mod.nbin))


car::qqPlot(rstandard(mod.nbin), group = data$female)



# -->
# 10 is unusual



# ----------
infl <- influence.measures(mod.nbin)


summary(infl)



# ----------
influencePlot(mod.nbin)




# ----------
idx <- c(10, 16, 29, 59, 92)

data[idx,]





# ------------------------------------------------------------------------------
# Individual index plot:  DFBETAs
#  - Cook's D and DFFITS are overall measure of the total influence that cases have on the regression coefficients and fitted values, respectively.
#    On the other hand, DFBETAs is the simplest measure of influence of observation i, which is the standardized change in the coefficient for each variable due to omitting that observation
# ------------------------------------------------------------------------------

dfbetas <- data.frame(infl$infmat[,2:4])

head(dfbetas)




# observations are colored blue or red according to ...

var <- "dfb.mn_p"
var <- "dfb.dmg_"
var <- "dfb.fmnn"


op <- par(mar = c(5, 5, 1, 1) + .1)

cols <- ifelse(data$female == 1, "red", "blue")

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
car::scatterplotMatrix(dfbetas, groups = data$female, smooth = FALSE, id = TRUE, 
                       showLabels=list(method = "mahal", n = 2, cex = 1, location = "lr"), 
                       ellipse = TRUE, levels = 0.95, robust = FALSE, 
                       diagonal = "histogram", col = c(gray(0.8), "black"))






