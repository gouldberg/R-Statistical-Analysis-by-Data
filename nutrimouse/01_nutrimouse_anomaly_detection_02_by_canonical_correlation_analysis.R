rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\05_anomaly_detection\\nutrimouse")



# ------------------------------------------------------------------------------
# data:  nutrimouse
#
#   - Two sets of variables were measured on 40 mice:
#       - gene:  expressions of 120 genes potentially involved in nutritional problems.
#       - lipid:  concentrations of 21 hepatic fatty acids.
#
#   - The 40 mice were distributed in a 2-factors experimental design (4 replicates):
#       - genotype:  Genotype (2-levels factor): wild-type and PPARalpha -/-
#       - diet:  Diet (5-levels factor): Oils used for experimental diets preparation were corn and colza oils (50/50)
#         for a reference diet (REF), hydrogenated coconut oil for a saturated fatty acid diet (COC), 
#         sunflower oil for an Omega6 fatty acid-rich diet (SUN), linseed oil for an Omega3-rich diet (LIN) and
#         corn/colza/enriched fish oils for the FISH diet (43/43/14).
# ------------------------------------------------------------------------------


# data(nutrimouse, package = "CCA")


gene <- read.table(file = "gene.txt", header = T, sep = "\t")

lipid <- read.table(file = "lipid.txt", header = T, sep = "\t")

diet <- read.table(file = "diet.txt", header = T, sep = "\t")

genotype <- read.table(file = "genotype.txt", header = T, sep = "\t")



# ----------
# 40 * 120
str(gene)


# 40 * 21
str(lipid)




# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------

# distribuion of 40 mice by each variable

graphics.off()

par(mfrow = c(1,1))

boxplot(gene)



# ----------
boxplot(lipid)




# ------------------------------------------------------------------------------
# Canonical Correlation Analysis
# ------------------------------------------------------------------------------


# here we use only gene[,1:10] and lipid[,1:9]

data <- as.matrix(cbind(gene[,1:10], lipid[,1:9]))




( R <- cor(data) )



# ----------
# decompose correlation matrix

xid <- 1:10


( r11 <- R[xid, xid] )

( r22 <- R[-(xid), -(xid)] )

( r12 <- R[xid, -(xid)] )

( r21 <- R[-(xid), xid] )




# ----------
# eigenvectors of matrices

( E1 <- solve(r11) %*% r12 %*% solve(r22) %*% r21 )


( E2 <- solve(r22) %*% r21 %*% solve(r11) %*% r12 )




# ----------
# eigenvalues and eigenvectors
# column 1 of eigenvectors is for 1st variate
# column 2 of eigenvectors is for 2nd variate

( e1 <- eigen(E1) )


( e2 <- eigen(E2) )




# ----------
# we take 2 vectors (values > 0.85)

e1$values


# in higher dimension, eigenvectors are represented by complex number ...
# so we use Re()

g1 <- as.matrix(data[,xid]) %*% Re(e1$vectors[,1])

g2 <- as.matrix(data[,xid]) %*% Re(e1$vectors[,2])

l1 <- as.matrix(data[,-(xid)]) %*% Re(e2$vectors[,1])

l2 <- as.matrix(data[,-(xid)]) %*% Re(e2$vectors[,2])




# ----------
# correlation of first variate for gene and lipid  = -0.76

cor(g1, l1)


# almost no corrleation of 2nd variate for gene and lipid = -0.05
cor(g2, l2)




# ----------
graphics.off()


par(mfrow = c(1,2))

plot(l1 ~ g1, main = "first variate: lipid vs gene")
abline(v = mean(g1), h = mean(l1), col = "darkgray", lty = 2)
text(g1, l1, label = 1:40, cex = 1.2)

plot(l2 ~ g2, main = "2nd variate: lipid ve gene")
abline(v = mean(g2), h = mean(l2), col = "darkgray", lty = 2)
text(g2, l2, label = 1:40, cex = 1.2)





# ----------
graphics.off()

par(mfrow = c(1,2))

plot(g2 ~ g1, main = "2nd vs 1st variate for gene of 40 mice")
abline(v = mean(g1), h = mean(g2), col = "darkgray", lty = 2)
text(g1, g2, label = 1:40, cex = 1.2)


plot(l2 ~ l1, main = "2nd vs 1st variate for lipid of 40 mice")
abline(v = mean(l1), h = mean(l2), col = "darkgray", lty = 2)
text(l1, l2, label = 1:40, cex = 1.2)





# ------------------------------------------------------------------------------
# anomaly detection by top 1 to 2 canonical covariate
# ------------------------------------------------------------------------------


X <- as.matrix(gene[,1:10])

Y <- as.matrix(lipid[,1:9])



# anomaly scores for each canonical covariates (1 to 2)

a <- matrix(nrow = nrow(X), ncol = 2, 0)




for(i in 1:2){
  
  f <- Re(X %*% e1$vectors[,i])
  
  g <- Re(Y %*% e2$vectors[,i])
  
  D <- data.frame(f = f, g = g)
  
  
  mod <- lm(g ~ f, data = D)
  
  sig2 <- sum(resid(mod)^2) / nrow(D)
  
  a[,i] <- c((g - resid(mod))^2 / sig2)

}



# ----------
fall <- Re(X %*% e1$vectors[,1:2])

gall <- Re(Y %*% e2$vectors[,1:2])

Dall <- data.frame(f = fall, g = gall)

head(Dall)




# ----------
graphics.off()

par(mfcol = c(3,2))

plot(Dall[,"f.1"], type = "h", main = "1st variate:  gene")
plot(Dall[,"g.1"], type = "h", main = "1st variate:  lipid")
plot(a[,1], type = "h", main = "1st variate anomaly")

plot(Dall[,"f.2"], type = "h", main = "2nd variate:  gene")
plot(Dall[,"g.2"], type = "h", main = "2nd variate:  lipid")
plot(a[,2], type = "h", main = "2nd variate anomaly")



# ----------
idx1 <- c(1, 2, 17, 19)

idx2 <- c(17, 31)


par(mfrow = c(2,1))

matplot(Y, type = "l", main = "lipid of 40 mice")
abline(v = idx1)

matplot(Y, type = "l", main = "lipid of 40 mice")
abline(v = idx2)




# ----------
# by variate

graphics.off()

par(mfrow = c(1,2))

idx <- unique(c(idx1))
colorcode <- rep(0, nrow(X))
colorcode[idx] <- 1

plot(l1 ~ g1, pch = 21, main = "1st variate: lipid vs gene", bg = colorcode)
abline(v = mean(g1), h = mean(l1), col = "darkgray", lty = 2)
text(g1+0.01, l1+0.01, label = 1:40, cex = 1.2)


idx <- unique(c(idx2))
colorcode <- rep(0, nrow(X))
colorcode[idx] <- 1

plot(l2 ~ g2, pch = 21, main = "2nd variate: lipid vs gene", bg = colorcode)
abline(v = mean(g2), h = mean(l2), col = "darkgray", lty = 2)
text(g2+0.01, l2+0.01, label = 1:40, cex = 1.2)




# ----------
# by X and Y

idx <- unique(c(idx1, idx2))
colorcode <- rep(0, nrow(X))
colorcode[idx] <- 1

graphics.off()

par(mfrow = c(1,2))

plot(g2 ~ g1, pch = 21, main = "gene of 40 mice: 2nd vs 1st variate", bg = colorcode)
abline(v = mean(g1), h = mean(g2), col = "darkgray", lty = 2)
text(g1+0.01, g2+0.01, label = 1:40, cex = 1.2)


plot(l2 ~ l1, pch = 21, main = "lipid of 40 mice: 2nd vs 1st variate", bg = colorcode)
abline(v = mean(l1), h = mean(l2), col = "darkgray", lty = 2)
text(l1+0.2, l2+0.2, label = 1:40, cex = 1.2)


