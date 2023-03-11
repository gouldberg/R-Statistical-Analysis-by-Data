setwd("//media//kswada//MyFiles//R//squirrels")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Red Squirrels
# ------------------------------------------------------------------------------

SQ <- read.table(file = "RedSquirrels.txt", header = TRUE, dec = ".")


str(SQ)


# ----------
# remove large DBH
SQ2 <- subset(SQ, DBH < 0.6)



# ------------------------------------------------------------------------------
# Data exploration:  Inspect variance inflation factor before removing covariabtes
#   - Although pairplots and scatterplots show only 2-way relationships, but VIFs are capable of showing higher-dimension collinearity
#   - Each covariate is used in turn as a response variable with all other variables as covariates
#   - Various cut-off values have been proposed in the literature.
#     Montgomery and Peck (1992) used 5 or 10, Zuur et al. (2007) used 3.
#     If the relationship between the response variable and the covariates is weak, a moderate amount of collinearity means that
#     none of the covariates may be found significant.
#     On the other hand, if the relationships between response variable and covariates are strong, the model can cope with a moderate, or even high,
#     amount of collinearity. Hence the strength of the relationship between response variable and covariates should be taken into account when choosing
#     cut-off value for VIFs.
# ------------------------------------------------------------------------------

corvif <- function(dataz) {
  dataz <- as.data.frame(dataz)
  
  #vif part
  form    <- formula(paste("fooy ~ ",paste(strsplit(names(dataz)," "),collapse=" + ")))
  dataz   <- data.frame(fooy=1 + rnorm(nrow(dataz)) ,dataz)
  lm_mod  <- lm(form,dataz)
  
  cat("\n\nVariance inflation factors\n\n")
  print(myvif(lm_mod))
}


myvif <- function(mod) {
  v <- vcov(mod)
  assign <- attributes(model.matrix(mod))$assign
  if (names(coefficients(mod)[1]) == "(Intercept)") {
    v <- v[-1, -1]
    assign <- assign[-1]
  } else warning("No intercept: vifs may not be sensible.")
  terms <- labels(terms(mod))
  n.terms <- length(terms)
  if (n.terms < 2) stop("The model contains fewer than 2 terms")
  if (length(assign) > dim(v)[1] ) {
    diag(tmp_cor)<-0
    if (any(tmp_cor==1.0)){
      return("Sample size is too small, 100% collinearity is present")
    } else {
      return("Sample size is too small")
    }
  }
  R <- cov2cor(v)
  detR <- det(R)
  result <- matrix(0, n.terms, 3)
  rownames(result) <- terms
  colnames(result) <- c("GVIF", "Df", "GVIF^(1/2Df)")
  for (term in 1:n.terms) {
    subs <- which(assign == term)
    result[term, 1] <- det(as.matrix(R[subs, subs])) * det(as.matrix(R[-subs, -subs])) / detR
    result[term, 2] <- length(subs)
  }
  if (all(result[, 2] == 1)) {
    result <- data.frame(GVIF=result[, 1])
  } else {
    result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
  }
  invisible(result)
}



# Note that MyVar does not include response SqCones
MyVar <- c("Ntrees", "DBH", "TreeHeight", "CanopyCover")

corvif(SQ2[,MyVar])



# -->
# Largest VIF value is 2.85.
# Using a cut-off of 3 means that we can keep all 4 cavariates, but based on the pairplots, 
# we would prefer to drop at least one.
# Also, the number of observations in the dataset is one the low side for 4 covariates.
# We have 52 observations, and the general recommendation is to have at least 15 to 25 times as many observations as we have covariates.
# Flaherty et al. (2012) dropped DBH, which makes sense.
