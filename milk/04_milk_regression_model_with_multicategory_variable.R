setwd("//media//kswada//MyFiles//R//milk")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  milk
# ------------------------------------------------------------------------------
data("milk", package = "rethinking")

d <- milk

dim(d)

str(d)



# ------------------------------------------------------------------------------
# Regression model with multicategory variable (clade)
# ------------------------------------------------------------------------------
unique(d$clade)

# library(caret)
# tmp <- predict(dummyVars(~ clade, data = d), d)

# dummy variable for referencing Ape
d$clade.NWM <- ifelse(d$clade == "New World Monkey", 1, 0)
d$clade.OWM <- ifelse(d$clade == "Old World Monkey", 1, 0)
d$clade.S <- ifelse(d$clade == "Strepsirrhine", 1, 0)


mod <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + b.NWM * clade.NWM + b.OWM * clade.OWM + b.S * clade.S,
    a ~ dnorm(0.6, 10),
    b.NWM ~ dnorm(0, 1),
    b.OWM ~ dnorm(0, 1),
    b.S ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = d)



precis(mod, corr = TRUE, digits = 3)


# -->
# The estimate a is the average milk energy for apes, and the estimates for the other categories are differences from apes.



# ------------------------------------------------------------------------------
# Estimated difference between the two monkey groups
#   - Once you get accustomed to manipulating estimates in this way, you can effectively reparameterize your model after you've already fit it to the data
# ------------------------------------------------------------------------------
# compare posterior distribution of kcal.per.g for each monkey
post <- extract.samples(mod)

mu.ape <- post$a
mu.NWM <- post$a + post$b.NWM
mu.OWM <- post$a + post$b.OWM
mu.S <- post$a + post$b.S

precis(data.frame(mu.ape, mu.NWM, mu.OWM, mu.S))


# ----------
diff.NWM.OWM <- mu.NWM - mu.OWM
quantile(diff.NWM.OWM, probs = c(0.025, 0.5, 0.975))


