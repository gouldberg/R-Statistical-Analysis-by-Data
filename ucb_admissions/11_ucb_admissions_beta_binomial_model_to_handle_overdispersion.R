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
#
# ------------------------------------------------------------------------------
mod_btb <- map(
  alist(
    admit ~ dbetabinom(applications, pbar, theta),
    logit(pbar) <- a,
    a ~ dnorm(0, 2),
    theta ~ dexp(1)
  ),
  data = d
)



# the scale paramter theta can be rather fickle, so it is better to use dexp instead of dcauchy and to speciy a reasonable start value.
# many values of theta will produce very similar predictions, so the half-cauchy prior is not always efficient.
mod_btb.stan <- map2stan(
  alist(
    admit ~ dbetabinom(applications, pbar, theta),
    logit(pbar) <- a,
    a ~ dnorm(0, 2),
    theta ~ dexp(1)
  ),
  data = d,
  constraints = list(theta = "lower=0"),
  start = list(theta = 3),
  iter = 4000, warmup = 1000, chains = 2, cores = 10
)



# ----------
precis(mod_btb, digits = 3)


pairs(mod_btb)



# ----------
# implied average probability of admission across departments
post <- extract.samples(mod_btb)

quantile(logistic(post$a), c(0.025, 0.5, 0.975))



# ----------
# draw posterior mean beta distribution
curve(dbeta2(x, mean(logistic(post$a)), mean(post$theta)), from = 0, to = 1, ylab = "Density", xlab = "probability admit", ylim = c(0, 3), lwd = 2)


# draw 100 beta distributions sampled from posterior
for(i in 1:100){
  p <- logistic(post$a[i])
  theta <- post$theta[i]
  curve(dbeta2(x, p, theta), add = TRUE, col = col.alpha("black", 0.2))
}


postcheck(mod_btb, na.rm = TRUE)
