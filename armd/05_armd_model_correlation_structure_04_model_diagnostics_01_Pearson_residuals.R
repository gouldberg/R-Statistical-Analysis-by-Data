
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\armd")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

# https://rdrr.io/cran/nlmeU/src/inst/scriptsR2.15.0/Ch12.R



# ------------------------------------------------------------------------------
# data:  ARMD (Age-Related Macular Degeneration Trial)
# ------------------------------------------------------------------------------


armd.wide <- read.csv("armd.wide.txt", header = T, sep = "\t")


armd0 <- read.csv("armd0.txt", header = T, sep = "\t")



str(armd.wide)


str(armd0)



# ----------
data("armd", package = "nlmeU")


str(armd)




# ------------------------------------------------------------------------------
# raw residuals
# ------------------------------------------------------------------------------

library(lattice)

xlims <- c("4", "12","24","52wks")

ylims <- c(-4.9, 4.9)



# ----------
panel.bwxplot0 <- function(x, y, subscripts, ...){
  panel.grid(h = -1)
  panel.stripplot(x, y, col = "grey", ...)
  panel.bwplot(x, y, pch = "|", ...)
}



bwplot(resid(fm12.3) ~ time.f | treat.f,
       panel = panel.bwxplot0,
       ylab = "Residuals", data = armd)


# -->
# the plots clearly show an increasing variance of the residuals with timepoint.



# ------------------------------------------------------------------------------
# plots of Pearson residuals vs. fitted values
# ------------------------------------------------------------------------------


plot(fm12.3)


plot(fm12.3, resid(., type = "p") ~ fitted(.) | time.f)


# id = 0.01:  The residuals larger, in absolute value, 
# than the 99th percentile of the standard normal distribution

stdres.plot <- plot(fm12.3, resid(., type = "p") ~ jitter(time) | treat.f,
                    id = 0.01, adj = c(-0.3, 0.5), grid = FALSE)


plot(update(stdres.plot, xlim = c(-5, 59), ylim = ylims, grid = "h"))



# -->
# the number of residuals larger, in absolute value, than the 99th percential
# of the standard normal distribution, is not excessive, given the total number of observations.




# ------------------------------------------------------------------------------
# Scatterplots of Pearson residuals versus time per treatment group for model
# Residuals for selected subjects are connected with lines
# ------------------------------------------------------------------------------

residP <- resid(fm12.3, type = "pearson")



# ----------
attach(armd)


# tp == 1:  time = 4wks
idx1 <- tp == 1


# larger, in absolute value, than 99th percentile of the standard normal distribution
idq <- 0.02               

idx <- (abs(residP) > -qnorm(idq/2)) & idx1


outliers.idx <- data.frame(subject, time, treat.f, visual, residP, idx)


id <- armd$subject
outliers <- subset(outliers.idx, idx, select = -idx)


nrow(outliers)

uid <- unique(outliers$subject)

length(uid)

uid

detach(armd)



# ----------
gin <-  rep(FALSE, length(armd$subject))

gin[id %in% uid] <- TRUE

dt <- data.frame(armd, gin=gin, resid.p = residP)

dtGin <- dt[gin, ]



# ----------
myPanel <- function(x, y, subscripts, groups, ...) {
  panel.grid(h = -1, v = 0) 
  gin <- dt$gin
  gins <- gin[subscripts]
  panel.xyplot(x, y)
  x1 <- x[gins]
  y1 <- y[gins]
  subs1 <- subscripts[gins]
  panel.superpose(x1, y1, subs1, groups, type = "l", lty = "13")
}



xyplot(resid.p ~ time.f | treat.f, data = dt,
       panel = myPanel,
       subscripts = TRUE,
       groups = subject,
       scales = list(abbreviate = TRUE),
       aspect = 1,
       xlab = "Standardized residuals",
       ylim = ylims)



# ----------
# alternative plotting

myPanel <- function(x, y, subscripts, ...) {
  panel.grid(h = -1, v = 0) 
  panel.stripplot(x, y, ...)   
  grps <- dt$subject
  gin <- dt$gin
  gins <- gin[subscripts]
  x1 <- x[gins]
  y1 <- y[gins]
  subs1 <- subscripts[gins]
  panel.superpose(x1, y1, subs1, grps, type = "l", lty = "13")
}


bw.object <- bwplot(resid.p ~ time.f | treat.f, 
                    data = dt,
                    panel = myPanel,
                    ylab = "Standardized residuals", 
                    aspect = 1.2)


update(bw.object, xlim = xlims, ylim = ylims)  




# -->
# In the panel for the Active treatment group,
# the residuals obtained for each of the selected individuals tend to have negative values.
