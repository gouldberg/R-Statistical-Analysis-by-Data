setwd("//media//kswada//MyFiles//R//pvq40")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PVQ40
# ------------------------------------------------------------------------------

data(PVQ40, package = "smacof")
# data(PVQ40agg, package = "smacof")


str(PVQ40)

attributes(PVQ40)


car::some(PVQ40)
# car::some(PVQ40agg)



# ------------------------------------------------------------------------------
# MDS on scores averaged by category
# ------------------------------------------------------------------------------

var_n <- c("SE", "CO", "TR", "BE", "UN", "SD", "ST", "HE", "AC", "PO")

var_n_select <- c("se1, se2", "co1, co2", "tr1, tr2", "be1, be2", "un1, un2, un3", "sd1, sd2", "st1, st2", "he1, he2", "ac1, ac2", "po1, po2")


# Take row means for each category of measure
for(i in 1:length(var_n)){
  eval(parse(text = paste0(var_n[i], " <- rowMeans(subset(PVQ40, select = c(", var_n_select[i], ")), na.rm = TRUE)")))
}

raw <- cbind(SE, CO, TR, BE, UN, SE, ST, HE, AC, PO)



# ----------
R <- cor(raw)

diss <- sim2diss(R)

res <- mds(diss, init = "torgerson", type = "ordinal")



# ----------
graphics.off()
par(mfrow=c(1,1))
plot(res)
polygon(res$conf, lty = 2)

erg    <- fitCircle(res$conf[,1], res$conf[,2])
draw.circle(erg$cx, erg$cy, radius = erg$radius, border = "blue", lty = 2)



# ------------------------------------------------------------------------------
# Unfolding
# ------------------------------------------------------------------------------

# Preferences into dissimilarities
( pref <- max(raw) - raw )



# ----------
# Initial configuration of the individuals:  random

nobs <- dim(raw)[1]

s1 <- matrix(runif(3 * nobs, min=0, max=1), nrow = nobs, ncol = 3)

s1



# ----------
# Initial configuration of ten basic personal values: TUV-theory based

tuv <- matrix(c(.50,-.87, .71,-.71, .87,-.50, .87,.50, .50,.87, -.50,.87,
                -.71,.71, -.87,.50, -.87,-.50, -.50,-.87) , nrow = 10, ncol = 2, byrow = TRUE)

# add column of zeros for 3d start
s2 <- cbind(tuv, matrix(0, nrow=10, ncol=1))


s2



# ----------
set.seed(33)

# Compute unfolding solution and rotate to pc plane of value points
result <- unfolding(delta=pref, ndim=3, itmax=6000, init=list(s1, s2))


par(mfrow = c(1,1))
plot(result)



# ----------
result

result$stress



# ------------------------------------------------------------------------------
# Check the validity of result
# ------------------------------------------------------------------------------

permtest(result)


# --> it does not work ...



# ------------------------------------------------------------------------------
# Unfolding
# ------------------------------------------------------------------------------

result$conf.col

result$conf.row


# ----------
# Projection to principal axes

# Find rotation matrix to principal axes
( e <- svd(result$conf.col) )

# Rotate value points (=X) to principal axes
( X <- result$conf.col %*% e$v )

# Rotate Y (=persons) to principal axes of X 
( Y <- result$conf.row %*% e$v )

# explained variance of X in values pc plane
( eV <- sum(X[, 1:2]^2) / sum(X^2) )



# ----------
# plot 3d unfolding solution
library(scatterplot3d)

lim1 <- c(-1, +1) 

graphics.off()
par(mfrow=c(1,1))
s3d <- scatterplot3d(X, type="h", xlab="Dimension 1", ylab="Dimension 2",
                     zlab="Dimension 3", xlim=lim1, ylim=lim1, zlim=lim1,  
                     cex.symbols=2,color="red", pch=21, bg="red", asp = 1)

text(s3d$xyz.convert(X), labels=rownames(X), pos = 2)

( X.lines <-  rbind(X, X[1, ]) )
s3d$points3d(x = X.lines[, 1], y = X.lines[, 2], z= X.lines[, 3], type = "l", col = "blue", lty = 2, lwd = 2) 

s3d$points3d(x = Y[, 1], y = Y[, 2], z = Y[, 3], pch = 21, bg = grey(0.1, alpha = .4),  
             col = grey(0.1, alpha = .6), xlab = "", ylab = "" ); 



# ------------------------------------------------------------------------------
# Plot by rgl  --> better presentation
# ------------------------------------------------------------------------------

library(rgl)

options(rgl.printRglwidget = TRUE) # Plot in regular R-Studio Viewer


plot3d(X, size=10, col = "red", xlab = "Dimension 1", ylab = "Dimension 2",
       zlab = "Dimension 3", xlim = lim1, ylim = lim1, zlim = lim1)

# settting the apparent ratios of the x,y,z axes of current bounding box
aspect3d("iso")

text3d(x = X[, 1], y = X[, 2], z= X[, 3], text=rownames(X), adj=1.2)

lines3d(x = X.lines[, 1], y = X.lines[, 2], z= X.lines[, 3],  col = "blue", lty = 2, lwd = 2) 

points3d(x = Y[, 1], y = Y[, 2], z= Y[, 3],  col = "grey", lty = 2, lwd = 2) 



# ------------------------------------------------------------------------------
# Plot stress per point (persons / values)
# ------------------------------------------------------------------------------

plot(result, plot.type = "stressplot")



