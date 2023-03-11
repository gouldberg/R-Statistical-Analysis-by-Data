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
# binomial regression for aggregated data wiht unique intercept for each department
#  - What is the average difference in probability of admission between females and males within departments ?
# ------------------------------------------------------------------------------


d$dept_id <- coerce_index(d$dept)


mod3 <- map(
  alist(
    admit ~ dbinom(applications, p),
    logit(p) <- a[dept_id],
    a[dept_id] ~ dnorm(0, 10)
  ), data = d
)



# ----------
# model with male difference as well
mod4 <- map(
  alist(
    admit ~ dbinom(applications, p),
    logit(p) <- a[dept_id] + bm * male,
    a[dept_id] ~ dnorm(0, 10),
    bm ~ dnorm(0, 10)
  ), data = d
)



# ----------
compare(mod1, mod2, mod3, mod4)



# -->
# Now the model without male is ranked first.
# Still, the WAIC difference between mod3 and mod4 is tiny, both models get about half of the Akaike weight, almost tie.
# So there's modest support for some effect of gender, even if it is overfit a little.



# ----------
precis(mod3, depth=2)


precis(mod4, depth=2)



# -->
# The estimate for bm goes in the opposite direction now.
# On the proportional odds scale, the estimate becomes exp(-0.1) = 0.9.
# So a male in this sample has about 90% the odds of admission as a female, comparing within departments.




# ------------------------------------------------------------------------------
# Posterior validation check
# ------------------------------------------------------------------------------

# postcheck() : default posterior validation check function

postcheck(mod4, n = 1e4)



for(i in 1:6){
  x <- 1 + 2 * (i - 1)
  y1 <- d$admit[x] / d$applications[x]
  y2 <- d$admit[x+1] / d$applications[x+1]
  lines(c(x, x+1), c(y1, y2), col = rangi2, lwd = 2)
  text(x + 0.5, (y1 + y2) / 2 + 0.05, d$dept[x], cex = 0.8, col = rangi2)
}


# -->
# A through E, capture variation in overall admission rates among departments.
# This allows the model to compare male and female admission rates, controlling for heterogeneity across departments.



# ----------

pairs(mod4)



# ----------
# For comparison between quadratic approximation and MCMC
# stancode(mod4)
mod4.stan <- map2stan(mod4, chains = 2, iter = 2500, warmup = 500)
precis(mod4.stan, depth = 2)


