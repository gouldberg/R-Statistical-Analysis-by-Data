setwd("//media//kswada//MyFiles//R//lawn")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  lawn
# ------------------------------------------------------------------------------

data("lawn", package = "faraway")

str(lawn)


car::some(lawn)



# ------------------------------------------------------------------------------
# Test the significance of lowest level of the variance components
# ------------------------------------------------------------------------------


cmodn <- lmer(time ~ 1 + manufact + speed + (1 | machine), data = lawn)


# reduced model without the lowest level of the variance components
cmodnr <- lm(time ~ 1 + manufact + speed, data = lawn)



# ----------
lrstat <- numeric(1000)


for(i in 1:1000){
  rtime <- unlist(simulate(cmodnr))
  
  nmod <- lm(rtime ~ 1 + manufact + speed, data = lawn)
  
  amod <- lmer(rtime ~ 1 + manufact + speed + (1 | machine), data = lawn)
  
  lrstat[i] <- 2 * (logLik(amod) - logLik(nmod))
}



mean(lrstat > 2 * (logLik(cmodn) - logLik(cmodnr)))



# -->
# We do reject sigma(machine)^2 = 0.



