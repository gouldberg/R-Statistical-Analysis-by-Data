setwd("//media//kswada//MyFiles//R//phdpubs")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PhdPubs data
# ------------------------------------------------------------------------------

data("PhdPubs", package = "vcdExtra")


dim(PhdPubs)

str(PhdPubs)


car::some(PhdPubs)



# ----------
PhdPubs <- within(PhdPubs, {
  female <- factor(female)
  married <- factor(married)
})



# ------------------------------------------------------------------------------
# The quasi-poisson model
# ------------------------------------------------------------------------------

mod.qpois <- glm(articles ~ ., data = PhdPubs, family = quasipoisson)

summary(mod.qpois)


plot(mod.qpois)

plot_resid(mod=mod.qpois, y = "articles")



# ------------------------------------------------------------------------------
# negative-binomiaol model
# ------------------------------------------------------------------------------

library(MASS)


# ----------
# estimate theta first
( nbin <- glm.nb(articles ~ ., data = PhdPubs) )

( theta <- nbin$theta )



# ----------
mod.nbin <- glm(articles ~., PhdPubs, family = negative.binomial(theta))

summary(mod.nbin)



# ----------
plot(allEffects(mod.nbin), band.colors = "blue", lwd=3, ylab = "Number of articles", main="", ylim=c(0, log(10)))

plot_resid(mod = mod.nbin, y = "articles")


