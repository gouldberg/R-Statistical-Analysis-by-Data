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
# Anomaly detection for the data assumed to follow multivariate normal distribution
#
#   - Mahalanobis-Taguchi Method  (MT method or MT system (MTS))
# ------------------------------------------------------------------------------



X <- cbind(gene[,1:10], lipid[,1:20])


mx <- colMeans(X)



Xc <- as.matrix(X - matrix(1, nrow(X), 1) %*% mx)


# compute covariance matrix
Sx <- t(Xc) %*% Xc / nrow(X)



# ----------
# anomaly score by 1 variable

a <- rowSums((Xc %*% solve(Sx)) * Xc ) / ncol(X)

plot(a, xlab = "index", ylab = "anomaly socre", type = "h", ylim = c(0, 1.5))


# threthold is set as 1.15
n <- nrow(X)
lines(0:n, rep(1.15, length(0:n)), col = "red", lty = 2)



# -->
# many anomalies ...




# ----------
# For 19th record, C20.3n.9 is main reason for being detected

xc_prime <- Xc[19, ]

SN1 <- 10 * log10(xc_prime ^ 2 / diag(Sx))

barplot(SN1)



