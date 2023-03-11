setwd("//media//kswada//MyFiles//R//cafes")

packages <- c("dplyr", "rethinking", "MASS", "ellipse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  cafes
#  mu(i) = a(i) + b(i) * A(i)
#    - mu(i):  wait time minutes at cafe
#    - A(i):  0/1 indicator for afternoon
# ------------------------------------------------------------------------------
#' Function to generate cafe data
#'
#' @param N - number of caffes
#' @param N_visits  - number of visits per caffe
#' @param a - mean of the distribution of average wait time across all cafe
#' @param b  - mean of the distribution of the average difference between morning and afternoon wait times (slope in the model)
#' @param sigma_a - deviance of the distribution of avg. wait times
#' @param sigma_b - deviance of the distribution of of the average difference between morning and afternoon wait times
#' @param rho - correlations between parameters a and b
#'
#' @return list with keys
#'       data - dataframe with N*N_visits rows and 3 columns: cafe(int, identifier),  afternoon(boolean encoded as {0,1}), wait(real)
#'       a_cafe - vector of true a
#'       b_cafe - vector of true b
#'       Mu
#'       Sigma

generate_caffe_data <- function(N_cafes, N_visits, a, b, sigma_a, sigma_b, rho) {
  Mu <- c( a , b )
  
  # Build covariance matrix from factorised representation
  sigmas <- c(sigma_a,sigma_b) # standard deviations
  Rho <- matrix( c(1,rho,rho,1) , nrow=2 ) # correlation matrix
  Sigma <- diag(sigmas) %*% Rho %*% diag(sigmas) # covariance matrix
  
  set.seed(5) # used to replicate examples
  vary_effects <- mvrnorm( N_cafes , Mu , Sigma )
  a_cafe <- vary_effects[,1]
  b_cafe <- vary_effects[,2]
  
  afternoon <- rep(0:1, N_cafes*N_visits/2)
  cafe_id <- rep( 1:N_cafes , each=N_visits )
  mu <- a_cafe[cafe_id] + b_cafe[cafe_id]*afternoon
  sigma <- 0.5 # std dev within cafes
  wait <- rnorm( N_visits*N_cafes , mu , sigma )
  d <- data.frame( cafe=cafe_id , afternoon=afternoon , wait=wait )
  list(
    data=d,
    a_cafe=a_cafe,
    b_cafe=b_cafe,
    Mu=Mu,
    Sigma=Sigma
  )
}


plot_true_cafe_data <- function(gen.cafe.data) {
  plot( gen.cafe.data$a_cafe , gen.cafe.data$b_cafe , col='red', pch=16,
        xlab="intercepts (a_cafe)" , ylab="slopes (b_cafe)", xlim=c(0,6), ylim=c(-2.5,0) )
  for ( l in c(0.1, 0.3, 0.5, 0.8, 0.99) ) {
    lines(ellipse(gen.cafe.data$Sigma, centre=gen.cafe.data$Mu, level=l), col=col.alpha("red", 0.7))
  }
}


plot_posterior_rho <- function(model, true_rho) {
  post <- extract.samples(model)
  rho_est <- mean( post$Rho[,1,2] )
  dens(post$Rho[,1,2])
  abline(v=true_rho, col='blue', lty=2)
  abline(v=rho_est, col='red', lty=2)
}



# ------------------------------------------------------------------------------
# Generate data
# ------------------------------------------------------------------------------

# generate data as in the chapter, but use 50 cafes instead of 20 to have better rho estimates
data.rho7 <- generate_caffe_data(50, 10, 3.5, -1, 1, 0.5, -0.7)



# ------------------------------------------------------------------------------
# Plot true data
# ------------------------------------------------------------------------------

plot_true_cafe_data(data.rho7)



# ------------------------------------------------------------------------------
# Model with multi-variate Gaussian priors(MVN)
# ------------------------------------------------------------------------------

mod.rho7 <- map2stan(
  alist(
    wait ~ dnorm( mu , sigma ),
    mu <- a_cafe[cafe] + b_cafe[cafe]*afternoon,
    c(a_cafe, b_cafe)[cafe] ~ dmvnorm2(c(a,b), sigma_cafe, Rho),
    a ~ dnorm(0,10),
    b ~ dnorm(0,10),
    sigma_cafe ~ dcauchy(0,2),
    sigma ~ dcauchy(0,2),
    Rho ~ dlkjcorr(2)
  ) ,
  data=data.rho7$data,
  iter=5000 , warmup=2000 , chains=2, cores = 10)




# ------------------------------------------------------------------------------
# Plot posterior rho
# ------------------------------------------------------------------------------

plot_posterior_rho(mod.rho7, -0.7)



