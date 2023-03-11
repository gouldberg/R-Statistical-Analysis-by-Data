
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
# Stripplots (and box-and-whiskers plots) of normalized residuals for each timepoint and treatment group
# ------------------------------------------------------------------------------


panel.bwxplot <- function(x,y, subscripts, ...){
  panel.grid(h = -1, v = 0)
  bwstats <- tapply(y, x, boxplot.stats)
  outy <-  unlist(lapply(bwstats, FUN = function(el) el$out))
  idx0 <- 1:length(y)
  idx <- y %in% outy
  idx1 <- idx0[idx]
  panel.stripplot(x[-idx1], y[-idx1], jitter.data = TRUE, col = "grey", ...)
  panel.bwplot(x, y, pch = "|", ...)
}



# type = "n":  normalized residuals

bwplot(
  resid(fm12.3, type = "n") ~ time.f | treat.f,                                 
  panel = panel.bwxplot,
  ylab = "Normalized residuals",
  data = armd)                 



# -->
# A few more extreme residuals with negative values, smaller than -4.
# Nevertheless, the number of residuals with an absolute value larger than e.g., 2 is about the same.




# ------------------------------------------------------------------------------
# Q-Q plots per timepoint of normalized residuals
# ------------------------------------------------------------------------------

qqnorm(fm12.3, ~resid(., type= "n") | time.f)

qqnorm(fm12.3, ~resid(., type= "p") | time.f)



# -->
# Although normalized residuals should be approximately uncorrelated,
# we graph separate Q-Q plots per time point to remove the influence of any residual correlation.
# The patterns appear to be reasonably close to straight lines.
# Thus, the normality assumption seems to be plausible.




qqPlot <- qqnorm(fm12.3, ~ resid(., type = "n") | time.f)

update(qqPlot, grid = TRUE, xlim = ylims, ylim = ylims, 
       aspect = 1)


