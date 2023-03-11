# setwd("//media//kswada//MyFiles//R//associated_press//")
setwd("//media//kswada//MyFiles//R//Bayesian_inference//associated_press//")

packages <- c("dplyr", "rstan")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Associated Press (AP)
#   - the partial sample from AP documents (from "topicmodels")
# ------------------------------------------------------------------------------

source("associated_press.R")

# number ot topics
K

# number of words
V

# number of documents
D

# data size
N


wordID
length(unique(wordID))


freq


docID


# ----------
data <- data.frame(docID = docID, wordID = wordID, freq = freq)
dat_mat <- data %>% tidyr::spread(., key = wordID, value = freq) %>% dplyr::select(-docID) %>% as.matrix()
dat_mat[is.na(dat_mat)] <- 0
rownames(dat_mat) <- paste0("doc_", 1:100)
colnames(dat_mat) <- paste0("wd", 1:4954)



# ------------------------------------------------------------------------------
# Principal Component Analysis Biplots
#   - singular value decomposition:  X = U * A^(1-alph) * A^alpha * V
#     A:  diagonal matrix with the singular values, lambda1^0.5, lambda2^0.5, ...
#     diagonal element of A^alpha is lambda1^(alpha/2), lambda2^(alpha/2), ...
#   - alpha is scaling factor
#       - alpha = 1:  row metric preserving, the plot approximates the Euclidean Distances among the persons in X
#       - alpha = 0:  column metric preserving, the plot approximates the covariance structure of the variables in X:
#                     the distances between the persons are determined by the Mahalanobis distance
#   - default choice in R's biplot is alpha = 1 (column metric preserving)
# ------------------------------------------------------------------------------


pca_or1 <- prcomp(dat_mat)

pca_or2 <- prcomp(dat_mat, scale = TRUE)


summary(pca_or1)

summary(pca_or2)



# ----------
# Biplot, with scale factor alpha = 1 (default):  row metric preserving
op <- par(mfrow = c(1,2))

biplot(pca_or1, pc.biplot = TRUE, cex = c(0.6, 0.8), col = c("gray", "coral1"), arrow.len = 0.05, 
       main = "Biplot (Unstandardized)", xlim = c(-4, 4), asp = 1, cex.axis = 0.8)

biplot(pca_or2, pc.biplot = TRUE, cex = c(0.6, 0.8), col = c("gray", "coral1"), arrow.len = 0.05, 
       main = "Biplot (Standardized)", asp = 1, cex.axis = 0.8)

layout(1)

par(op)




# ----------
# Biplot, with scale factor alpha = 1 (default):  row metric preserving
# Biplot, with scale factor alpha = 0:  column metric preserving
op <- par(mfrow = c(2,2), mar = c(4, 3, 4, 1))

biplot(pca_or1, pc.biplot = TRUE, cex = c(0.6, 0.8), col = c("gray", "coral1"), arrow.len = 0.05, 
       main = expression(paste("Biplot (Unstandardized, ", alpha, "=0)")), scale = 0, cex.axis = 0.8)
abline(h = 0, v = 0, col = "gray", lty = 2)

biplot(pca_or1, pc.biplot = TRUE, cex = c(0.6, 0.8), col = c("gray", "coral1"), arrow.len = 0.05, 
       main = expression(paste("Biplot (Unstandardized, ", alpha, "=1)")), scale = 1, cex.axis = 0.8)
abline(h = 0, v = 0, col = "gray", lty = 2)

biplot(pca_or2, pc.biplot = TRUE, cex = c(0.6, 0.8), col = c("gray", "coral1"), arrow.len = 0.05, 
       main = expression(paste("Biplot (Standardized, ", alpha, "=0)")), scale = 0, cex.axis = 0.8)
abline(h = 0, v = 0, col = "gray", lty = 2)

biplot(pca_or2, pc.biplot = TRUE, cex = c(0.6, 0.8), col = c("gray", "coral1"), arrow.len = 0.05, 
       main = expression(paste("Biplot (Standardized, ", alpha, "=1)")), scale = 1, cex.axis = 0.8)
abline(h = 0, v = 0, col = "gray", lty = 2)

par(op)



# -->
# Both left panels are now metric preserving (alpha = 0), meaning that they approximate the Euclidean distances among the participants.
# Both right panels are column metric preserving (alpha = 1), they approxiate the covariance (top right) and correlation (bottom right) structure,
# in addition to the Mahalanobis distances between persons.



# -->
# In the standardized version, the sd information is getting lost since all variables are scaled to sd = 1.
# In the bottom right panel, the variable vectors lengths are less than one.  (see more two axes on the right and the top)
# We could draw a unit circle; vectors close to this circle imply that these variables fit better.

