setwd("//media//kswada//MyFiles//R//doubs")

packages <- c("dplyr", "vegan", "cluster", "dendextend", "pvclust", "labdsv", "RColorBrewer")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Doubs
# ------------------------------------------------------------------------------

load("./data/Doubs.RData")


dim(spe)
car::some(spe)


dim(env)
car::some(env)


dim(spa)
car::some(spa)


dim(fishtraits)
car::some(fishtraits)


dim(latlong)
car::some(latlong)



# ----------
# remove site 8, which have zero species
spe <- spe[-8,]
env <- env[-8,]
spa <- spa[-8,]
# latlong <- latlong[-8,]


# ----------
# data preparation
dfs <- env[, 1]
env2 <- env[, -1]
slo2 <- rep(".very_steep", nrow(env))
slo2[env$slo <= quantile(env$slo)[4]] <- ".steep"
slo2[env$slo <= quantile(env$slo)[3]] <- ".moderate"
slo2[env$slo <= quantile(env$slo)[2]] <- ".low"
slo2 <- factor(slo2, levels = c(".low", ".moderate", ".steep", ".very_steep"))
env3 <- env2
env3$slo <- slo2
envtopo <- env2[, c(1 : 3)]
envchem <- env2[, c(4 : 10)]


# ----------
# this is fictious design
ele.fac <- gl(3, 9, labels = c("high", "mid", "low"))
pH.fac <- as.factor(c(1, 2, 3, 2, 3, 1, 3, 2, 1, 2, 1, 3, 3, 2, 1, 1, 2, 3, 2, 1, 2, 3, 2, 1, 1, 3, 3))
ele.pH.helm <- model.matrix(~ ele.fac * pH.fac, contrasts = list(ele.fac = "contr.helmert", pH.fac = "contr.helmert"))[, -1]



# ------------------------------------------------------------------------------
# The Code It Yourself
#  - The code below is another exercise in coding matrix algebra in R
# ------------------------------------------------------------------------------
myRDA <- function(Y, X)
{
  
  # ----------------------------------------------
  ## 1. Preparation of the data
  
  Y.mat <- as.matrix(Y)
  Yc <- scale(Y.mat, scale = FALSE)
  
  X.mat <- as.matrix(X)
  Xcr <- scale(X.mat)
  
  # Dimensions
  n <- nrow(Y)
  p <- ncol(Y)
  m <- ncol(X)
  
  # ----------------------------------------------
  ## 2. Computation of the multivariate linear regression
  
  # Matrix of regression coefficients (eq. 11.9)
  B <- solve(t(Xcr) %*% Xcr) %*% t(Xcr) %*% Yc
  
  # Matrix of fitted values (eq. 11.10)
  Yhat <- Xcr %*% B
  
  # Matrix of residuals
  Yres <- Yc - Yhat
  
  
  # ----------------------------------------------
  ## 3. PCA on fitted values
  
  # Covariance matrix (eq. 11.12)
  S <- cov(Yhat)
  
  # Eigenvalue decomposition
  eigenS <- eigen(S)
  
  # How many canonical axes?
  kc <- length(which(eigenS$values > 0.00000001))
  
  # Eigenvalues of canonical axes
  ev <- eigenS$values[1 : kc]
  # Total variance (inertia) of the centred matrix Yc
  trace = sum(diag(cov(Yc)))
  
  # Orthonormal eigenvectors (contributions of response 
  # variables, scaling 1)
  U <- eigenS$vectors[, 1 : kc]
  row.names(U) <- colnames(Y)
  
  # Site scores (vegan's wa scores, scaling 1; eq.11.17)
  F <- Yc %*% U
  row.names(F) <- row.names(Y)
  
  # Site constraints (vegan's 'lc' scores, scaling 1; 
  # eq. 11.18)
  Z <- Yhat %*% U
  row.names(Z) <- row.names(Y)
  
  # Canonical coefficients (eq. 11.19)
  CC <- B %*% U
  row.names(CC) <- colnames(X)
  
  # Explanatory variables
  # Species-environment correlations
  corXZ <- cor(X, Z)
  
  # Diagonal matrix of weights
  D <- diag(sqrt(ev / trace))
  
  # Biplot scores of explanatory variables
  coordX <- corXZ %*% D    # Scaling 1
  coordX2 <- corXZ         # Scaling 2
  row.names(coordX) <- colnames(X)
  row.names(coordX2) <- colnames(X)
  
  # Scaling to sqrt of the relative eigenvalue
  # (for scaling 2)
  U2 <- U %*% diag(sqrt(ev))
  row.names(U2) <- colnames(Y)
  F2 <- F %*% diag(1/sqrt(ev))
  row.names(F2) <- row.names(Y)
  Z2 <- Z %*% diag(1/sqrt(ev))
  row.names(Z2) <- row.names(Y)
  
  # Unadjusted R2
  R2 <- sum(ev/trace)
  # Adjusted R2
  R2a <- 1 - ((n - 1)/(n - m - 1)) * (1 - R2)
  
  
  # ----------------------------------------------
  # 4. PCA on residuals
  # Write your own code as in Chapter 5. It could begin with : 
  #     eigenSres <- eigen(cov(Yres))
  #     evr <- eigenSres$values
  
  
  # ----------------------------------------------
  # 5. Output
  
  result <- 
    list(trace, R2, R2a, ev, CC, U, F, Z, coordX, 
         U2, F2, Z2, coordX2)
  names(result) <- 
    c("Total_variance", "R2", "R2adj", "Can_ev", 
      "Can_coeff", "Species_sc1", "wa_sc1", "lc_sc1", 
      "Biplot_sc1", "Species_sc2", "wa_sc2", "lc_sc2", 
      "Biplot_sc2") 
  
  result
}



# ----------
# Apply homemade function to the Doubs fish and environmental data
doubs.myRDA <- myRDA(spe.hel, env2)


names(doubs.myRDA)



# Retrieve adjusted R-square
doubs.myRDA$R2adj



