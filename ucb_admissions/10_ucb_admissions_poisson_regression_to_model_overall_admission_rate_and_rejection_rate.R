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
# binomial model of overall admission probability
# ------------------------------------------------------------------------------


m_binom <- map(
  alist(
    admit ~ dbinom(applications, p),
    logit(p) <- a,
    a ~ dnorm(0, 100)
  ), data = d
)



precis(m_binom, digits = 3)



# ----------
# the inferred binomial probability of admission, across the entire data set

logistic(coef(m_binom))




# ------------------------------------------------------------------------------
# Poisson model of overall admission rate and rejection rate
#
#  - This is multinomial in disguise as Poisson
#  - refactor multinomial likelihood into a series of Poisson likelihoods
# ------------------------------------------------------------------------------


# 'reject" is a reserved word
d$rej <- d$reject



# implied admission rate = lambda1 / (lambda1 + lambda2)

m_pois <- map2stan(
  alist(
    admit ~ dpois(lambda1),
    rej ~ dpois(lambda2),
    log(lambda1) <- a1,
    log(lambda2) <- a2,
    c(a1, a2) ~ dnorm(0, 100)
  ), data = d, chains = 3, cores = 10
)



precis(m_pois, digits = 3)




# ----------
# the inferred probability of admission, across the entire data set

k <- as.numeric(coef(m_pois))

exp(k[1]) / (exp(k[1]) + exp(k[2]))



# --> 
# This is the same inference as in the binomial model




# ----------
# the inferred binomial probability of admission, across the entire data set

logistic(coef(m_binom))

