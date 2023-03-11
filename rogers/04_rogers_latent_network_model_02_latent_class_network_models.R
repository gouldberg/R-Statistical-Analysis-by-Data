setwd("//media//kswada//MyFiles//R//rogers")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Rogers
# ------------------------------------------------------------------------------

data("Rogers", package = "MPsychoR")

str(Rogers)

dim(Rogers)

car::some(Rogers)



# ------------------------------------------------------------------------------
# Latent Class Network Models
#  - Let a network algorithm try to detect corresponding node clusters.
#    Latent class network models integrate the idea of parametric clustering into network analysis.
#  - Formally, this model represents an extension of the eigenmodel approach, in addition to scaling,
#    it also estimates the class membership of each node in a probabilistic way.
# ------------------------------------------------------------------------------


# We fit a very simple model based on a dichotomized correlation matrix.
thresh <- 0.2

cormat01 <- ifelse(abs(cormat) > thresh, 1, 0)



# ----------
# We need to convert this adjacency matrix into a network object (undirected graph), using the network package
library(network)

cornet <- network(cormat01, matrix.type = "adjacency", directed = FALSE)

cornet



# ----------
library(latentnet)

set.seed(111)



# fix the number of dimensions to p = 2 (argument d) and vary the number of classes G from 1 to 3.
# We keep the MCMC settings at the funciton's default values.
# For each model we extract the BIC for goodness-of-fit assessment.
# Note that, similar to a GLM, latent class networks allow for different distributional family specifications.
# Since in our example we operate on a binary input matrix, we use the Bernoulli family (default)

fitLN1 <- ergmm(cornet ~ euclidean(d = 2, G = 1))

summary(fitLN1)$bic$Z



fitLN2 <- ergmm(cornet ~ euclidean(d = 2, G = 2))

summary(fitLN2)$bic$Z



fitLN3 <- ergmm(cornet ~ euclidean(d = 2, G = 3))

summary(fitLN3)$bic$Z



# -->
# according to the BIC,
# the two- and three-cluster solutions fit equally well.



# ------------------------------------------------------------------------------
# see if we have convergence in the MCMC
# ------------------------------------------------------------------------------

mcmc.diagnostics(fitLN2)



# ------------------------------------------------------------------------------
# 2-cluster solution:  plot the solution
# ------------------------------------------------------------------------------

# Let us focus on the 2-cluster solution.

plot(fitLN2)



# -->
# We see that dimension 2 discriminates between the set of OCD items and the set of depression items.
# The circles give the soft cluster boundary.
# These circles should not be interpreted in a deterministic way since, as in parametric clustering in general,
# the latent class network algorithm estimates probabilistic class membership.



# ------------------------------------------------------------------------------
# 2-cluster solution:  Examine the solution
# ------------------------------------------------------------------------------

# Extract the posterior probabilities of 3 selected symptoms

clusmemb2 <- fitLN2$mkl$mbc$Z.pZK

dimnames(clusmemb2) <- list(colnames(cormat01), paste("Cluster", 1:2))

clusmemb2[c("comptime", "suicide", "weightgain"), ]



# -->
# We see that comptime has high probabiliti to belong to cluster 1,
# whereas suicide and weightgain have high probabilities to belong to cluster 2.

# From the posterior probabilities of the remaining items and in conjunction with the network,
# we can conclude that cluster 1 is a tight OCD cluster, and cluster 2 a wider depression cluster.

# Therefore, the algorithm successfully associated the symptoms with the respective disorder (except for obdistress).



# ------------------------------------------------------------------------------
# 3-cluster solution:  plot the solution
# ------------------------------------------------------------------------------

# Let us focus on the 3-cluster solution.

plot(fitLN3)



# -->
# We see that the depression cluster from the 2 cluster solution is split up into 2 overlapping cluster: a wide, general depression cluster 
# and a depression subcluster (red), closer to the OCD cluster (blue)

# This plot also includes information on the posterior class membership by means of a pie chart representation of the nodes.
# For instance, obdistress has approximately equal probabilities to belong to cluster 1,2 or 3.



# ------------------------------------------------------------------------------
# 3-cluster solution:  Examine the solution
# ------------------------------------------------------------------------------

# Extract the posterior probabilities of 4 selected symptoms

clusmemb3 <- fitLN3$mkl$mbc$Z.pZK

dimnames(clusmemb3) <- list(colnames(cormat01), paste("Cluster", 1:3))

clusmemb3[c("comptime", "suicide", "weightgain", "obdistress"), ]




# ------------------------------------------------------------------------------
# 3-cluster solution:  alternative representation, posteriror densities of te clusters
# ------------------------------------------------------------------------------

plot(fitLN3, main = "Latent Class Network", cluster.col = c("coral", "cadetblue", "darkgoldenrod"), what = "density")



