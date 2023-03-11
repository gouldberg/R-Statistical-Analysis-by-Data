setwd("//media//kswada//MyFiles//R//ew_eng")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  EW_eng
# ------------------------------------------------------------------------------

data(EW_eng, package = "smacof")


str(EW_eng)


EW_eng$east

EW_eng$west



# ------------------------------------------------------------------------------
# compute overall similarity measures
# ------------------------------------------------------------------------------

# the correlation of the coordinates of correspoinding points (after Procrustean fitting)
r <- cor(as.vector(res.west$conf), as.vector(fit2$Yhat))

r


# congruence coefficient on distances
c <- fit2$congcoef

c



# ------------------------------------------------------------------------------
# Benchmarking by random configurations
# ------------------------------------------------------------------------------

# Similarity of random MDS solutions
Procrustes.test <- function(n, m, nrep=500){
  
  set.seed(333); c <- vector();  r <- vector()
  X <- matrix(runif(n*m, -1, 1), nrow=n, ncol=m)
  X <- scale(X, scale=FALSE)

  for (i in 1:nrep){
    Y <- matrix(runif(n * m, -1, 1), nrow=n, ncol=m)
    fit <- Procrustes(X, Y)
    c[i] <- fit$congcoef
    r[i] <- cor(c(X), c(fit$Yhat))
  }
  
  cr <- list("c"=c, "r"=r)
}


# ----------
z <- Procrustes.test(13, 2) 

z99 <- quantile(z$c, .99)

r99 <- quantile(z$r, .99)

cat("c(99%)=", round(z99,2), " r(99%)=", round(r99,2), sep = '')


# -->
# Hence, the similarity of the observed configurations is much higher than can reasonably be expected by chance.


