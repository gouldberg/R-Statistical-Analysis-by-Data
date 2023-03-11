# setwd("//media//kswada//MyFiles//R//cpi_exr_wage")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\silvia")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  silvia
# ------------------------------------------------------------------------------

dat <- read.csv("silvia_dat.txt", header = TRUE, colClasses = "numeric", sep = "\t")


str(dat)


car::some(dat)



# ----------
dat$TM <- as.factor(dat$TM)


dat$DEAL <- as.factor(dat$DEAL)



# ----------
# price down
dat <- dat %>% mutate(pd = P - NP)


# KM == 0
dat <- dat %>% mutate(km_z = KM == 0)




# ----------
mod0 <- lm(P ~ NP + KM + TM + Yr + CT + DEAL, data = dat)

mod1 <- lm(P ~ NP + KM + TM + Yr + CT + DEAL + km_z, data = dat)

mod2 <- lm(P ~ NP + KM + TM + Yr + CT + DEAL + km_z + KM : TM, data = dat)

mod3 <- lm(P ~ NP + KM + TM + Yr + km_z, data = dat)

mod4 <- lm(P ~ NP + KM + TM + I(Yr^2) + I((24 - CT)^2) + km_z, data = dat)

mod_step <- lm(P ~ NP + KM + TM + Yr + km_z + CT + DEAL + TM : Yr + TM : DEAL, data = dat)


mod_obj <- mod3
# mod_obj <- mod4
# mod_obj <- mod_step




# ------------------------------------------------------------------------------
# model diagnostics:  residuals
# ------------------------------------------------------------------------------

car::residualPlots(mod2)

car::residualPlots(mod3)

car::residualPlots(mod4)

car::residualPlots(mod_step)



# -->
# mod2, mod3, mod_step is better than mod4



# ----------
par(mfrow = c(2,2))

plot(mod_obj)




# ------------------------------------------------------------------------------
# model diagnostics:  added-variable plots
# ------------------------------------------------------------------------------


pch <- ifelse(dat$TM == 1, 20, 2)

col <- ifelse(dat$TM == 1, "blue", "black")


par(mfrow=c(1,1))


car::avPlots(mod_obj, id = TRUE, pch = pch, col = col)




# ----------
idx <- c(65, 126, 216, 239)

dat[idx,]



# car 65 and 216
# NP = 204, TM = 1, DEAL = 0
# but, Yr is really different (1 and 6)
# but P = 158 (same)




# ------------------------------------------------------------------------------
# identify influential observations
# ------------------------------------------------------------------------------

infl <- influence.measures(mod_obj)

infl




# ----------
par(mfrow = c(1,1))

car::influencePlot(mod_obj)



# ----------
car::influenceIndexPlot(mod_obj, vars = c("Cook", "studentized", "hat"), id.n = 4)





# ------------------------------------------------------------------------------
# dfbetas
# ------------------------------------------------------------------------------


head(infl$infmat)


dfbetas <- data.frame(infl$infmat[,2:9])

head(dfbetas)



# observations are colored blue or red according to ...

var <- "dfb.KM"

op <- par(mar = c(5, 5, 1, 1) + .1)

cols <- as.numeric(dat$TM)

plot(dfbetas[,var], type = "h", col = cols, xlab = "Observation index", ylab = expression(delta * beta[var]), cex.lab = 1.3)
points(dfbetas[,var], col = cols)

big = abs(dfbetas[,var]) > .25
idx <- 1:nrow(dfbetas)
text(idx[big], dfbetas[big, var], label = rownames(dfbetas)[big], cex = 0.9, pos = ifelse(dfbetas[big, var] > 0, 3, 1), xpd = TRUE)
abline(h = c(-0.25, 0, .25), col = "gray")

par(op)




# ----------
idx <- c(48, 66)

dat[idx,]



# -->
# car 48:  P >= NP  !!!





# ------------------------------------------------------------------------------
# Individual index plot:  scatterplot matrix of DFBETAs
#   - show the pairwise changes in the regression coefficients for the various predictors
# ------------------------------------------------------------------------------

# method = "mahl" to label the most extreme observations according to the Mahalanobis distance of each point from the centroid in the plot.

car::scatterplotMatrix(dfbetas, smooth = FALSE, id = TRUE, 
                       showLabels=list(method = "mahal", n = 2, cex = 1.2, location = "lr"), 
                       ellipse = TRUE, levels = 0.95, robust = FALSE, 
                       diagonal = "histogram", col = gray(0.6))


# -->
# The joint effect of observations on pairs of coefficients is more complex than is apparent from the univariate views that appear in the plots along the diagonal.



