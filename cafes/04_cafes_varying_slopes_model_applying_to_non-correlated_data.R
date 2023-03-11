setwd("//media//kswada//MyFiles//R//cafes")

packages <- c("dplyr", "rethinking", "MASS", "ellipse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# generate data with correlation set to zero
# ------------------------------------------------------------------------------

data.rhoZero <- generate_caffe_data(50, 10, 3.5, -1, 1, 0.5, 0.0)


plot_true_cafe_data(data.rhoZero)



# ------------------------------------------------------------------------------
# estimation
# ------------------------------------------------------------------------------

mod.rhoZero <- map2stan(mod.rho7, data=data.rhoZero$data, iter=5000 , warmup=2000 , chains=2, cores = 10)



# ------------------------------------------------------------------------------
# posterior rho
# ------------------------------------------------------------------------------

plot_posterior_rho(mod.rhoZero, 0.0)



# Answer: posterior distribution of the correlation coefficient is centred around zero
#   It means, that even if we set up priors to encounter for correlation, but there is no correlation in the data, 
#   then the model successfully infer the absence of a relationship.
#   In other words, don't be afraid to use multivariate normal as a prior for intercepts and slopes. 
#   It doesn't hurt the inference if there is no correlation but it helps to find correlation if there is any according to data.
