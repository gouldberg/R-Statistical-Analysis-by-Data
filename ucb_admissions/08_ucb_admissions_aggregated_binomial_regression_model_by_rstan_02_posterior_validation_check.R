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
# binomial regression for aggregated data
#  - Whether these data contain evidence of gender bias in admissions?
# ------------------------------------------------------------------------------

library(rethinking)


mod1 <- map(
  alist(
    admit ~ dbinom(applications, p),
    logit(p) <- a + bm * male,
    a ~ dnorm(0, 10),
    bm ~ dnorm(0, 10)
  ),
  data = d
)



mod2 <- map(
  alist(
    admit ~ dbinom(applications, p),
    logit(p) <- a,
    a ~ dnorm(0, 10)
  ),
  data = d
)




# ----------
# WAIC comparison verifies that the male predictor variable improves expected out-of-sample deviance by a very large amount
# This comparison suggests that gender matters a lot.

compare(mod1, mod2)




# ----------
# Seems like being male is an advantage.
precis(mod1, corr = TRUE, digits=3)




# ----------
# the relative difference in admission odds as  1.84
exp(c(0.508, 0.61, 0.712))




# ------------------------------------------------------------------------------
# The difference in probability of admission
# ------------------------------------------------------------------------------

post <- extract.samples(mod1)


post



# ----------
p.admit.male <- logistic(post$a + post$bm)

p.admit.female <- logistic(post$a)



# ----------
( diff.admit <- p.admit.male - p.admit.female )


quantile(diff.admit, c(0.025, 0.5, 0.975))



# -->
# The difference in probability of admission is 14%, with a 95% interval from 11% to almost 17%



# ----------
dens(diff.admit)


