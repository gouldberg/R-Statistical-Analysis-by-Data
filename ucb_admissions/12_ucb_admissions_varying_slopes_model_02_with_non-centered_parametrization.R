setwd("//media//kswada//MyFiles//R//ucb_admissions")

packages <- c("dplyr", "vcd", "MASS", "datasets")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  UCBAdmissions
# ------------------------------------------------------------------------------

data("UCBAdmissions", package = "datasets")


data <- UCBAdmissions


data


dimnames(data)


dim(data)




# ----------
# transform from matrix to aggregated data

library(tidyr)

d <- data.frame(data) %>% spread(., key = "Admit", value = "Freq") %>% dplyr::select(Dept, Gender, Admitted, Rejected) %>% mutate(applications = Admitted + Rejected)

colnames(d) <- c("dept", "applicant_gender", "admit", "reject", "applications")


d$male <- ifelse(d$applicant_gender == "Male", 1, 0)

d <- d %>% arrange(dept, applicant_gender)

d <- rapply(d, f = as.integer, classes = "numeric", how = "replace")



# ----------
str(d)

car::some(d)




# ------------------------------------------------------------------------------
# Centered parametrization and non-centered parametrization
# ------------------------------------------------------------------------------

# Multi-variate gaussian priors, centered parametrization
mod <- map2stan(
  alist(
    admit ~ dbinom( applications , p ),
    logit(p) <- a_dept[dept_id] + bm_dept[dept_id]*male,
    c(a_dept, bm_dept)[dept_id] ~ dmvnorm2( c(a,bm) , sigma_dept , Rho ),
    a ~ dnorm(0, 10),
    bm ~ dnorm(0, 1),
    sigma_dept ~ dcauchy(0, 2),
    Rho ~ dlkjcorr(2)
  ),
  data=d, warmup=1000, iter=5000, chains=4, cores=10 )


precis(mod, depth=2)

plot( precis(mod, pars=c("a_dept", "bm_dept"), depth=2) )




# ----------
# Non-centered parametrization
mod.nc <- map2stan(
  alist(
    admit ~ dbinom( applications , p ),
    logit(p) <- a_dept[dept_id] + bm_dept[dept_id]*male,
    c(a_dept, bm_dept)[dept_id] ~ dmvnormNC(sigma_dept , Rho ),
    #a ~ dnorm(0,10),
    sigma_dept ~ dcauchy(0, 2),
    Rho ~ dlkjcorr(2)
  ),
  data=d, warmup=1000, iter=5000, chains=4, cores=3 )


precis(mod.nc, depth=2)

plot( precis(mod.nc, pars=c("a_dept", "bm_dept"), depth=2) )



# ------------------------------------------------------------------------------
# model comparison
# ------------------------------------------------------------------------------
# models are almost identical in terms of the WAIC

(cmp <- compare(mod, mod.nc))

plot(cmp)



# ----------
plot(coeftab(mod, mod.nc))



# ----------
# extract n_eff values for each model
neff_c <- precis(mod, 2)@output 
neff_c['param'] <- rownames(neff_c)

neff_nc <- precis(mod.nc, 2)@output
neff_nc['param'] <- rownames(neff_nc)


params_df <- inner_join(dplyr::select(neff_c, Mean, param, n_eff), 
                        dplyr::select(neff_nc, Mean, param, n_eff), 
                        by=c('param'),  suffix=c('_centred','_noncentred'))

# plot distributions
boxplot( list( 'mod'=params_df$n_eff_centred , 'mod.non_centered'=params_df$n_eff_noncentred ) ,
         ylab="effective samples" , xlab="model" )


# on average, the non-centred model has larger effective samples values for a_dept and bm_dept
# but smaller values for deviance estimates (sigma_dept and Rho[1,2])



# ----------
t.test(params_df$n_eff_centred, params_df$n_eff_noncentred, paired = TRUE)
# but p-value of the difference is 0.2257 so from the perspective of significanse testing it is not possible to consider models as different

