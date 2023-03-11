setwd("//media//kswada//MyFiles//R//gorf")

packages <- c("dplyr", "shapes")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

source(file = "//media//kswada//MyFiles//R_basics//functions_for_morphometrics.R")



# ------------------------------------------------------------------------------
# data:  gorf, panf, pongof
# ------------------------------------------------------------------------------
data(gorf.dat, package = "shapes")
data(panf.dat, package = "shapes")
data(pongof.dat, package = "shapes")

dim(gorf.dat)

dim(panf.dat)

dim(pongof.dat)



# ------------------------------------------------------------------------------
# Check configurations by sample
# ------------------------------------------------------------------------------

idx_gf <- sample(dim(gorf.dat)[3], size = 2, replace = FALSE)
idx_pf <- sample(dim(panf.dat)[3], size = 2, replace = FALSE)
idx_pgf <- sample(dim(pongof.dat)[3], size = 2, replace = FALSE)


par(mfrow=c(2,3), mar=c(2,2,2,2))
plot(gorf.dat[,,idx_gf[1]], axes = FALSE, xlab = "", ylab = "", main = paste("gorf", idx_gf[1]))
polygon(gorf.dat[,,idx_gf[1]])

plot(gorf.dat[,,idx_gf[2]], axes = FALSE, xlab = "", ylab = "", main = paste("gorf", idx_gf[2]))
polygon(gorf.dat[,,idx_gf[2]])

plot(panf.dat[,,idx_pf[1]], axes = FALSE, xlab = "", ylab = "", main = paste("panf", idx_pf[1]))
polygon(panf.dat[,,idx_pf[1]])

plot(panf.dat[,,idx_pf[2]], axes = FALSE, xlab = "", ylab = "", main = paste("panf", idx_pf[2]))
polygon(panf.dat[,,idx_pf[2]])

plot(pongof.dat[,,idx_pgf[1]], axes = FALSE, xlab = "", ylab = "", main = paste("pongf", idx_pgf[1]))
polygon(pongof.dat[,,idx_pgf[1]])

plot(pongof.dat[,,idx_pgf[2]], axes = FALSE, xlab = "", ylab = "", main = paste("pongf", idx_pgf[2]))
polygon(pongof.dat[,,idx_pgf[2]])



# ----------
# dataset must be first reorganized because landmarks are not labeled in the same order for each set.

gorf <- gorf.dat
panf <- panf.dat[c(5,1,2:4,6:8),,]
pongof <- pongof.dat[c(5,1,2:4,6:8),,]

# gorf <- gorf.dat[c(5,1,2:4,6:8),,]
# panf <- panf.dat
# pongof <- pongof.dat



# ------------------------------------------------------------------------------
# Projected to tangent space coordinates
# ------------------------------------------------------------------------------

APE <- array(c(panf, gorf, pongof), dim = c(8,2,80))
AP <- orp(pgpa(APE)$rotated)
m <- t(matrix(AP, 16, 80))
labels <- c(rep("P", 26), rep("G", 30), rep("O", 24))


# APE <- array(c(panf, gorf), dim = c(8,2,56))
# AP <- orp(pgpa(APE)$rotated)
# m <- t(matrix(AP, 16, 56))
# labels <- c(rep("P", 26), rep("G", 30))



# ------------------------------------------------------------------------------
# Hierarchical Clustering
# ------------------------------------------------------------------------------

graphics.off()

par(mar = c(0.5, 2, 1, 1))

layout(matrix(c(1,2), 2, 1))

plot(hclust(dist(m), method = "average"), main = "UPGMA", labels = labels, cex = 0.7)

plot(hclust(dist(m), method = "complete"), main = "COMPLETE", labels = labels, cex = 0.7)




# ------------------------------------------------------------------------------
# Estimate the number of groups within the data using a partitional clustering method and the elbow approach
# ------------------------------------------------------------------------------

library(cluster)

df <- dim(m)[1] - 1

SStot <- sum(diag(var(m))) * df

exp1 <- 0

for(i in 2:20){
  
  mod <- pam(dist(m), i)
  
  mod1 <- lm(m ~ as.factor(mod$clustering))
  
  exp1[i] <- sum(diag(var(mod1$fitted.values))) * df / SStot
}


par(mfrow = c(1,1), mar = c(2,2,2,2))
plot(1:20, exp1, ylab = "Explained variance", xlab = "Group number")
lines(1:20, exp1)



# ----------
pam(dist(m), 5)$clustering

