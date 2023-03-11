
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




# ------------------------------------------------------------------------------
# Data exploration:  correlation among measurements
# ------------------------------------------------------------------------------

# monotone patterns
mnt.pat <- c("----", "---X", "--XX", "-XXX", "XXXX") 

armd.wide.mnt <- subset(armd.wide, miss.pat %in% mnt.pat)



# ----------
armd.wide.mnt1 <- within(armd.wide.mnt, {
  miss.pat <- factor(miss.pat, levels = mnt.pat)
})


levels(armd.wide.mnt1$miss.pat)              



# ----------
with(armd.wide.mnt1, {
  fl  <- list(treat.f, miss.pat)
  tapply(subject, fl, FUN = function(x) length(x[!is.na(x)]))
})




# ----------
library(lattice)

my.lowerPanel <- function(x, y, subscripts, ...){
    panel.grid(h = -1, v = -1) 
    panel.xyplot(x, y, ...)
  }



my.upperPanel <- function(x, y, subscripts, ...){
    panel.xyplot(x, y, type = "n", ...)
    corx <- round(cor(x, y, use = "complete.obs"), 2)
    abs.corx <- abs(corx)
    cex.value <- 3
    ltext(50,50, corx, cex = abs.corx* cex.value)
  }



mySuperPanel <- function(z, subscripts, panel.subscripts,...){
  panel.pairs(z, subscripts = subscripts,
              panel.subscripts = panel.subscripts,
              as.matrix = TRUE, 
              upper.panel = "my.upperPanel",
              lower.panel = "my.lowerPanel",
              prepanel.limits = function(z) return(c(1, 90))
  )}



splom.form <- formula( ~cbind(vis0, vis4, vis12, vis24, vis52))


armd.wide.tmp <- subset(armd.wide, miss.pat == "----",
                        select = c(visual0, visual4, visual12, visual24, visual52))


names(armd.wide.tmp) <- c("vis0", "vis4","vis12","vis24","vis52")



splom.object <- splom(splom.form,
                      data = armd.wide.tmp,
                      par.settings = list(fontsize = list(points = 4), axis.text = list(cex = 0.9)),
                      as.matrix = TRUE,
                      xlab = "",
                      superpanel = mySuperPanel
)




print(splom.object)



# -->
# An increase of th evariance of visual acruity measuremens obtained at later timepoints.
# The estimated correlation matrix suggests a moderate to strong correlation of the measurements.
# We also observe that the correlation clearly decreases with the time gap.




# ------------------------------------------------------------------------------
# Data exploration:  covariance-correleation matrix
# ------------------------------------------------------------------------------

visual.x <- subset(armd.wide, select = c(visual0:visual52))


# var-cov matrix
( varx <- var(visual.x, use = "complete.obs") )



# correlation matrix
print(cor(visual.x, use = "complete.obs"),  digits = 2) 



# var-cov diagnal elements
diag(varx)



# corr matrix (alternative way)
cov2cor(varx)

