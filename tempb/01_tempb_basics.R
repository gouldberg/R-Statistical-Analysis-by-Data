
packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  tempb
#  - Daily minimum and maximum temperatures from the weather station in Badajoz, Spain
#    from 1 January 1955 to 31 December 2015.
---------------------------------------------------------------------------

  
data(tempb, package = "ks")


str(tempb)




# ------------------------------------------------------------------------------
# data exploration: time series plot
# ------------------------------------------------------------------------------

MTS::MTSplot(tempb[,c("tmin","tmax")])




# ------------------------------------------------------------------------------
# scatter plot
# ------------------------------------------------------------------------------

library(colorspace)

library(RColorBrewer)

pt.col <- function(pos=1, alpha=1, ...) {return(paste0(brewer.pal(12, "Paired"), format(as.hexmode(round(alpha*255,0)), width=2))[pos])}
seq.col <- function(n, alpha=1, ...){c("transparent", rev(sequential_hcl(n=n-1, h=290, power=1.1, alpha=alpha, ...)))}
seq.col2 <- function(n, alpha=1, ...) {c("transparent",tail(rev(diverge.col(2*n-1, alpha=alpha, ...)), n-1))}
seq.col3d <- function(n, alpha=1, ...){rev(sequential_hcl(n=n, h=290, power=1.1, c.=c(80,70), l=c(30,50), alpha=alpha, ...))}
heat.col <- function(n, alpha=1, ...){heat_hcl(n, h=c(0, -100), l=c(75, 40), c=c(40, 80), power=1, alpha=alpha, ...)}
terrain.col <- function(n, alpha=1, ...){terrain_hcl(n, c=c(65, 0), l=c(45, 95), power=c(1/3, 1.5), alpha=alpha, ...)}
diverge.col <- function(n, alpha=1, ...){cols <- paste0(brewer.pal(n, "PuOr"), as.hexmode(round(alpha*255,0))); cols[n %/% 2+1] <- grey(1, alpha=0); return(cols)}


tempb <- tempb[, c("tmin", "tmax")]


# plot parameters
par(oma=c(0,0,0,0)+0.1, mgp=c(1.8,0.5,0), mar=c(2.9,2.9,0,0)+0.1, cex.axis=1.2, cex.lab=1.2)
xlab <- "Min temperature (deg C)" 
ylab <- "Max temperature (deg C)" 
xlim <- c(-5.5, 27.5)
ylim <- c(5.5, 42.5)


plot(tempb, cex=0.7, pch=16, col=pt.col(4, alpha=0.1), 
     asp=1, xaxs="i", yaxs="i", xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab) 



# ----------
lattice::xyplot(tmax ~ tmin, data = tempb, pch = 20, col = "black", alpha = 0.3)


