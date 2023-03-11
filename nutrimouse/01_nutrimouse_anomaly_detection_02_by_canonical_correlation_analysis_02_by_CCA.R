rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\ƒfƒXƒNƒgƒbƒv\\‹Zp—Í‹­‰»_“Œv‰ðÍ\\51_‰ðÍƒXƒNƒŠƒvƒg\\00_basics\\05_anomaly_detection\\nutrimouse")



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
# Correlation Matrix
# ------------------------------------------------------------------------------


X <- as.matrix(gene[,1:10])

Y <- as.matrix(lipid)


summary(X)

summary(Y)




# ----------
library(CCA)


correl <- matcor(X,Y)


correl$Xcor


correl$Ycor


# Correlation matrix ((p+q) * (p+q)) between X and Y variables
# 10 + 21 = 31
dim(correl$XYcor)



# ----------
img.matcor(correl)


img.matcor(correl,type = 2)




# ----------
# X = 4 (ACBP), Y = 2 (C16.0)  are highly correlated

Xt <- as.matrix(gene[,4])

Yt <- as.matrix(lipid[,2])


cor(Xt, Yt)




# X = 10 (ALDH3), Y = 2 (C16.0)  are highly correlated

Xt <- as.matrix(gene[,10])

Yt <- as.matrix(lipid[,2])


cor(Xt, Yt)





# ------------------------------------------------------------------------------
# Canonical Correlation Analysis
#    - The canonical correlation analysis seeks linear combinations of the fXf variables which are the most
#      correlated with linear combinations of the fYf variables.
#
#    - Let PX and PY be the projector onto the respective column-space of X and Y.
#      The eigenanalysis of PXPY provide the canonical correlations (square roots of the eigenvalues) and
#      the coefficients of linear combinations that define the canonical variates (eigen vectors).
# ------------------------------------------------------------------------------


X <- as.matrix(gene[,1:10])

Y <- as.matrix(lipid)




# ----------
# canonical correlation analysis
# highlight correlations between two data matrices.

# CCA() complete the cancor() function
# with supplemental numerical and graphical outputs
# and can handle missing values.


res.cc <- CCA::cc(X,Y)


names(res.cc)



# ----------
# plot canonical correlation (square roots of the eigenvalues)

graphics.off()

par(mfrow = c(1,1))

plot(res.cc$cor,type = "b")




# ----------
# variable representation
# note that ACBP, ALDH3 is correlated with C16.0 !!

plt.cc(res.cc, type = "v", var.label = TRUE)




# ----------
# individual representation together on dimension 1 and 2

# plt.cc(res.cc, type = "b", var.label = TRUE)

plt.cc(res.cc, d1 = 1, d2 = 2, type = "i", var.label = TRUE)


# graphics.off()
# par(mfrow = c(1,1))
# plot(g2 ~ g1, main = "2nd vs 1st variate for gene of 40 mice")
# abline(v = mean(g1), h = mean(g2), col = "darkgray", lty = 2)
# text(g1, g2, label = 1:40, cex = 1.2)



# ----------
plt.cc(res.cc, d1 = 3, d2 = 4, type = "i", var.label = TRUE)




# ------------------------------------------------------------------------------
# anomaly detection by top 1 to 4 canonical covariates
# ------------------------------------------------------------------------------


# select correlation >= 0.9  --> 4 canonical covariates  (1 to 4)
res.cc$cor[res.cc$cor >= 0.9]



# ----------
# anomaly scores for each canonical covariates (1 to 4)

a <- matrix(nrow = nrow(X), ncol = 4, 0)


for(i in 1:4){
  
  f <- X %*% res.cc$xcoef[,i]
  
  g <- Y %*% res.cc$ycoef[,i]
  
  D <- data.frame(f = f, g = g)
  
  
  mod <- lm(g ~ f, data = D)
  
  sig2 <- sum(resid(mod)^2) / nrow(D)
  
  a[,i] <- c((g - resid(mod))^2 / sig2)

}



# ----------
fall <- X %*% res.cc$xcoef[,1:4]

gall <- Y %*% res.cc$ycoef[,1:4]

Dall <- data.frame(f = fall, g = gall)

head(Dall)



# ----------
graphics.off()

par(mfcol = c(3,4))

plot(Dall[,"f.1"], type = "h", main = "1st")
plot(Dall[,"g.1"], type = "h")
plot(a[,1], type = "h")

plot(Dall[,"f.2"], type = "h", main = "2nd")
plot(Dall[,"g.2"], type = "h")
plot(a[,2], type = "h")

plot(Dall[,"f.3"], type = "h", main = "3rd")
plot(Dall[,"g.3"], type = "h")
plot(a[,3], type = "h")

plot(Dall[,"f.4"], type = "h", main = "4th")
plot(Dall[,"g.4"], type = "h")
plot(a[,4], type = "h")



# ----------
idx1 <- c(10, 17, 19)
idx2 <- c(17, 21, 31)
idx3 <- c(7, 16, 18, 31, 39)
idx4 <- c(9, 14, 39)

idx <- unique(c(idx1, idx2))

colorcode <- rep(0, 40)

colorcode[idx] <- 1

d1 <- res.cc$scores$xscores[,1]
d2 <- res.cc$scores$xscores[,2]


plot(d2 ~ d1, pch = 21, bg = colorcode)
abline(v = mean(d1), h = mean(d2), col = "darkgray", lty = 2)
text(d1+0.1, d2+0.1, label = 1:40, cex = 1.2)

