
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\armd")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



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
# Scatterplot matrix of the Pearson residuals for model fm9.2 (complete cases only n = 188)
# ------------------------------------------------------------------------------

residP <- resid(fm9.2, type = "p")

dtAux <- subset(armd, select = c(subject, visual, time, time.f, treat.f)) 

( dtP <- data.frame(dtAux, residP) )



require(reshape)

dtPm <- melt(dtP,
             measure.var = c("residP"),
             id.var = c("subject", "time.f"))


dtPc <- cast(subject ~ time.f ~ variable, data = dtPm)

dtPc <- data.frame(dtPc) 

names(dtPc) <- c("P4wks", "P12wks", "P24wks", "P52wks")


head(dtPc)

range(dtPc, na.rm = TRUE)



# ----------
library(ellipse)
library(lattice)


my.upperPanel <- function(x, y, subscripts, ...){
    panel.xyplot(x, y, type = "n", ...)
    ic <- complete.cases(cbind(x,y))
    mn <- c(mean(x[ic]), mean(y[ic]))
    covx <- var(cbind(x,y), use = "complete.obs")
    # print(covx)
    # ex <- ellipse(covx)
    corrx <- cov2cor(covx)
    corx <- round(corrx[1,2], 2)
    abs.corx <- abs(corx)
    # print(corx)
    cex.value <- 3
    ltext(0, 0, corx, cex = abs.corx * cex.value)
  }


my.lowerPanel <- function(x,y,subscripts,...){
    panel.grid(h = -1, v = -1)
    covx <- var(cbind(x, y), use = "complete.obs")
    # print(covx)
    ex <- ellipse(covx)
    panel.xyplot(ex[ ,1], ex[ ,2], lty = 2, type = "l", ...)
    panel.xyplot(x, y, ...)
  }


mySuperPanel <- function(z, subscripts, panel.subscripts,...){
  panel.pairs(z, subscripts = subscripts,
              panel.subscripts = panel.subscripts,
              as.matrix=TRUE, 
              upper.panel = "my.upperPanel",
              lower.panel = "my.lowerPanel",
              prepanel.limits = function(z) return(c(-4, 4))
  )
}


splom.form <- formula(~ cbind(P4wks, P12wks, P24wks, P52wks))


lattice::splom(splom.form, data = dtPc, as.matrix = TRUE, xlab = "", superpanel = mySuperPanel)



# -->
# 95% confidence ellipses were added.
# The scatterplot clearly show a violation of the assumption of the independence of observations:
# residuals for different measurement occasions are correlated.
# The correlation coefficient decreases with the increasing distance between the timepoints.


