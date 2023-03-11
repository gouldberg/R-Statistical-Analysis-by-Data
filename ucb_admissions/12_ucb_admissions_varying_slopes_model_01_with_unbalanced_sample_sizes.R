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


d$male <- ifelse(d$applicant.gender == "Male", 1, 0)

d <- d %>% arrange(dept, applicant.gender)

d <- rapply(d, f = as.integer, classes = "numeric", how = "replace")



# ----------
str(d)

car::some(d)




# ------------------------------------------------------------------------------
# Varying intercept model
# ------------------------------------------------------------------------------

mod1 <- map2stan(
  alist(
    admit ~ dbinom(applications, p),
    logit(p) <- a_dept[dept_id] + bm * male,
    a_dept[dept_id] ~ dnorm(a, sigma_dept),
    a ~ dnorm(0, 10),
    bm ~ dnorm(0, 1),
    sigma_dept ~ dcauchy(0, 2)
  ),
  data = d,
  warmup = 500, iter = 4500, chains = 3, cores = 10
)


# note that the departments re ordered from those with the highest proportions accepted to the lowest.
precis(mod1, digits = 3, depth = 2)



# ----------
logistic(coef(mod1))


# -->
# Remember, the values above are the deviations from the global mean alpha, which in this case has posterior mean -0.59



# ------------------------------------------------------------------------------
# Varying effects of being male: unpooled model
# ------------------------------------------------------------------------------

mod2_0 <- map2stan(
  alist(
    admit ~ dbinom(applications, p),
    logit(p) <- a_dept[dept_id] + bm_dept[dept_id] * male,
    a_dept[dept_id] ~ dnorm(0, 10),
    bm_dept[dept_id] ~ dnorm(0, 1)
  ),
  data = d,
  warmup = 1000, iter = 5000, chains = 3, cores = 10
)


plot(mod2_0)


precis(mod2_0, digits = 3, depth = 2)



# ------------------------------------------------------------------------------
# Varying effects of being male: pooled model
#  - What if we allow the effect of an applicant's being male to vary in the same way we already allowed the overall rate of admission to vary ?
# ------------------------------------------------------------------------------

# One extra feature of varying slopes that will arise here is that since there is substantial imbalance in sample size across departments and the
# numbers of male and female applications they received,
# pooling will be stronger for those cases with fewer applications

mod2 <- map2stan(
  alist(
    admit ~ dbinom(applications, p),
    logit(p) <- a_dept[dept_id] + bm_dept[dept_id] * male,
    c(a_dept, bm_dept)[dept_id] ~ dmvnorm2(c(a, bm), sigma_dept, Rho),
    a ~ dnorm(0, 10),
    bm ~ dnorm(0, 1),
    sigma_dept ~ dcauchy(0, 2),
    Rho ~ dlkjcorr(2)
  ),
  data = d,
  warmup = 1000, iter = 5000, chains = 3, cores = 10
)


plot(mod2)


precis(mod2, digits = 3, depth = 2)



# ------------------------------------------------------------------------------
# model inspection
#  - marginal posterior distributions for the varying effects only
# ------------------------------------------------------------------------------
par(mfrow=c(1,1))
plot(precis(mod2, pars = c("a_dept", "bm_dept"), depth = 2))


# -->
# Notice that the intercepts range all over the place, while the slopes all cling close to zero.
# This reflects that the fact that departments varied a lot in overall admission rates, but they neigher discriminated much between male and female applicants
# nor varied much in how much they discriminated.

# But there are a few departments with slopes consistent with noticeable bias, departments 1 and 2 in particular.
# Department 1: slope estimate centered almost 1 log-odds below the mean, and does not have any mass on the other side of zero
# Department 2: highly uncertain slope, but that means it also incudes plausibly large effects.
#  --> Just because a marginal posterior overlaps zero does not mean we should think of it as zero.

# Notice also that these two departments have the largest intercepts.



# ------------------------------------------------------------------------------
# model inspection
#  - posterior correlation between intercepts and slopes
# ------------------------------------------------------------------------------
post <- extract.samples(mod2)


# prior and posterior distribution
R2 <- rlkjcorr(12000, K = 2, eta = 2)
dens(R2[,1,2], xlab ="correlation", col = "black", lty = 2, ylim = c(0, 1.2))
dens(post$Rho[,1,2], add=TRUE, col = "blue", lty = 1, lwd = 2)


# -->
# The posterior is concentrated on negative values



# ------------------------------------------------------------------------------
# model inspection
#  - shrinkage (adaptive regularization)  --> plot the posterior mean varying effects
# ------------------------------------------------------------------------------
# unpooled model
post_1 <- extract.samples(mod2_0)
a1 <- apply(post_1$a_dept, 2, mean)
b1 <- apply(post_1$bm_dept, 2, mean)
a1
b1


# compute posterior mean bivariate Gaussian
Mu_est_1 <- c(mean(post_1$a_dept), mean(post_1$bm_dept))
rho_est_1 <- 0
sa_est_1 <- sd(post_1$a_dept)
sb_est_1 <- sd(post_1$bm_dept)
cov_ab_1 <- sa_est_1 * sb_est_1 * rho_est_1
Sigma_est_1 <- matrix(c(sa_est_1^2, cov_ab_1, cov_ab_1, sb_est_1^2), ncol = 2)



# ----------
# adaptively pooled model (varying intercepts and slopes)
# extract posterior means of partially pooled estimates
post_2 <- extract.samples(mod2)
a2 <- apply(post_2$a_dept, 2, mean)
b2 <- apply(post_2$bm_dept, 2, mean)

# compute posterior mean bivariate Gaussian
Mu_est_2 <- c(mean(post_2$a_dept), mean(post_2$bm_dept))
rho_est_2 <- mean(post_2$Rho[,1,2])
sa_est_2 <- mean(post_2$sigma_dept[,1])
sb_est_2 <- mean(post_2$sigma_dept[,2])
cov_ab_2 <- sa_est_2 * sb_est_2 * rho_est_2
Sigma_est_2 <- matrix(c(sa_est_2^2, cov_ab_2, cov_ab_2, sb_est_2^2), ncol = 2)


#----------
# convert varying effects to admit ratio (oc: outcome)
a1_oc <- logistic((a1 + b1))
b1_oc <- (logistic(a1))
a2_oc <- (logistic(a2 + b1))
b2_oc <- (logistic(a2))

Mu_est_oc <- c(mean(a2_oc), mean(b2_oc))
rho_est_oc <- cor(a2_oc, b2_oc)
sa_est_oc <- sd(a2_oc)
sb_est_oc <- sd(b2_oc)
cov_ab_oc <- sa_est_oc * sb_est_oc * rho_est_oc
Sigma_est_oc <- matrix(c(sa_est_oc^2, cov_ab_oc, cov_ab_oc, sb_est_oc^2), ncol = 2)


# ----------
par(mfrow=c(1,2))
library(ellipse)

# plot both and connect lines
plot(a1, b1, xlab = "intercept(a_dept)", ylab = "slope(bm_dept)", pch = 16, col = rangi2, ylim = c(min(b1)-0.1, max(b1)+0.1), xlim=c(min(a1)-0.1, max(a1)+0.1))
points(a2, b2, pch = 1)
for(i in 1:6) lines(c(a1[i], a2[i]), c(b1[i], b2[i]))
for(l in c(0.1, 0.3, 0.5, 0.8, 0.99)) lines(ellipse(Sigma_est_2, centre=Mu_est_2, level=l), col=col.alpha("black", 0.2))


# -->
# blue points:  the unpooled estimates for each department
# open points: the posterior means from the varying effects model

# Notice the each open points is displaced from the blue towards the center of the contours, as a result of shrinkage in both dimensions
# Blue points farther from the center experience more shrinkage
# Notice too that shrinkage is not in direct lines towards the center.

plot(a1_oc, b1_oc, xlab = "admit ratio(Male)", ylab = "admit ratio(Female)", pch = 16, col = rangi2, ylim = c(min(a1_oc)-0.1, max(b1_oc)+0.1), xlim=c(min(a1_oc)-0.1, max(b1_oc)+0.1))
points(a2_oc, b2_oc, pch = 1)
for(i in 1:6) lines(c(a1_oc[i], a2_oc[i]), c(b1_oc[i], b2_oc[i]))
for(l in c(0.1, 0.3, 0.5, 0.8, 0.99)) lines(ellipse(Sigma_est_oc, centre=Mu_est_oc, level=l), col=col.alpha("black", 0.2))
abline(0, 1, lty = 2)

# diagonal dashed line: where morning wait is equal to afternoon wait.



# ------------------------------------------------------------------------------
# ignoring gender
# ------------------------------------------------------------------------------

mod3 <- map2stan(
  alist(
    admit ~ dbinom(applications, p),
    logit(p) <- a_dept[dept_id],
    a_dept[dept_id] ~ dnorm(a, sigma_dept),
    a ~ dnorm(0, 10),
    sigma_dept ~ dcauchy(0, 2)
  ),
  data = d, warmup = 500, iter = 4500, chains = 3, cores = 10
)


precis(mod3, digits = 3, depth = 2)



# ------------------------------------------------------------------------------
# model comparison
# ------------------------------------------------------------------------------
compare(mod1, mod2_0, mod2, mod3)

plot(precis(mod2, depth = 2))


# -->
# The model that ignores gender (mod3) earns tha same expected out-of-sample performance as the model that includes a constant effect of gender (mod1)
# The varying slopes model (mod2) dominates both.
# This is depite the fact that the average slope in mod3 is nearly zero.
# Not average, but the individual slopes is what matters.


