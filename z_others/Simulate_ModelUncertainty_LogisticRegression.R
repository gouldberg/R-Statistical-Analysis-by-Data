# ------------------------------------------------------------------------------
# Simulate Model Uncertainty by several validation methods
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# Simulate Model Uncertainty by several validation methods
#
# IT TAKES TIME !!!!
# ------------------------------------------------------------------------------
set.seed(1)
n <- 200  # size of training sample
reps <- 200  # simulations
npop <- 50000  # size of validation gold standard sample

methods <- c("Boot 40", "Bood 200", "632a 40", "632a 200", "632b 40", "632b 200", "10-fold * 4", "4-fold * 10", "10-fold * 20", "4-fold * 50")
R <- expand.grid(sim = 1:reps,  p = c(15, 30),  method = methods)
R$Dxy <- R$Intercept <- R$Slope <- R$D <- R$D <- R$U <- R$Q <- R$repmeth <- R$B <- NA
R$n <- n

head(R)

# FUNCTION to do r overall reps of B resamples, averaging to get estimates similar to as if r*B resamples were done
val <- function(fit, method, B, r){
  contains <- function(m) length(grep(m, method)) > 0
  meth <- if(contains("Boot")) "boot" else
    if(contains("fold")) "crossvalidation" else
      if(contains("632")) ".632"
  z <- 0
  for(i in 1:r) z <- z + validate(fit, method=meth, B=B)[c("Dxy", "Intercept", "Slope", "D", "U", "Q"), "index.corrected"]
  return(z/r)
}


# set the pattern of number of variables
p_var <- c(15, 30)

for(p in p_var){
  cat(paste0("p: ", p, "\n"))
  Beta <- rep(.5, p)
  X <- matrix(runif(npop * p), nrow=npop) - 0.5
  LX <- matxv(X, Beta)
  Y <- ifelse(runif(npop) <= plogis(LX), 1, 0)
  
  for(j in 1:reps){
    cat(paste0("p: ", p, " -- j: ", j, "\n"))
    ## make training sample
    x <- matrix(runif(n * p), nrow=n) - 0.5
    L <- matxv(x, Beta)  # multiply matrix and vector
    y <- ifelse(runif(n) <= plogis(L), 1, 0)
    f <- lrm(y ~ x, x=TRUE, y = TRUE)
    beta <- f$coef
    forecast <- matxv(X, beta)
    # the "gold standard" external validations
    v <- val.prob(logit = forecast, y = Y, pl = FALSE)[c("Dxy", "Intercept", "Slope", "D", "U", "Q")]
    
    ## validate in population
    for(method in methods){
      cat(paste0("p: ", p, " -- j: ", j, " ---- method: ", method, "\n"))
      repmeth <- 1
      if(method %in% c("Boot 40", "632a 40", "632b 40"))
        B <- 40
      if(method %in% c("Boot 200", "632a 200", "632b 200"))
        B <- 200
      if(method == "10-fold * 4"){ B <- 10;  repmeth <- 4 }
      if(method == "4-fold * 10"){ B <- 4;  repmeth <- 10 }
      if(method == "10-fold * 20"){ B <- 10;  repmeth <- 20 }
      if(method == "4-fold * 50"){ B <- 4;  repmeth <- 50 }
      z <- val(f, method, B, repmeth)
      k <- which(R$sim == j & R$p == p & R$method == method)
      if(length(k) != 1) stop("Program logic error")
      R[k, names(z)] <- z - v
      R[k, c("B", "repmeth")] <- c(B=B, repmeth=repmeth)
    }
  }
}

statnames <- names(R)[6:11]
w <- reshape(R, direction="long", varying=list(statnames), v.names="x", timevar="stat", times=statnames)
w$p <- paste("p", w$p, sep="=")


# show the summary in multi-way dot chart
# Bootstrap nonparametric percentile 0.95 limits are included
s <- with(w, summarize(abs(x), llist(p, method, stat), smean.cl.boot, stat.name="mae"))
Dotplot(method ~ Cbind(mae, Lower, Upper) | stat * p, data = s, xlab = "Mean | error |")

s <- with(w, summarize(x^2, llist(p, method, stat), smean.cl.boot, stat.name="mse"))
Dotplot(method ~ Cbind(sqrt(mse), sqrt(Lower), sqrt(Upper)) | stat * p, data = s, xlab = expression(sqrt(MSE)))





