setwd("//media//kswada//MyFiles//R//jsp")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  jsp
# ------------------------------------------------------------------------------

data("jsp", package = "faraway")

str(jsp)

car::some(jsp)



# ----------
# We shall take as our response he math test score result from the final year and try to model this as a function of gender,
# social class and the Raven's test score from the first year which math be taken as a measure of ability when entering the school.

jspr <- jsp[jsp$year == 2,]


car::some(jspr)



# ------------------------------------------------------------------------------
# Test fixed effects by parametric bootstrap
# ------------------------------------------------------------------------------

# with gender
amod <- lmer(math ~ raven * social * gender + (1 | school) + (1 | school : class), data = jspr)


# without gender
nmod <- lmer(math ~ raven * social + (1 | school) + (1 | school : class), data = jspr)



# ----------
lrstat <- numeric(1000)



# ----------
# THIS TAKES TIME !!!:  1 minute
for(i in 1:1000){
  nmath <- unlist(simulate(nmod))
  # We use refit to speed up the computation
  nmodr <- refit(nmod, nmath)
  amodr <- refit(amod, nmath)
  lrstat[i] <- as.numeric(2 * (logLik(amodr) - logLik(nmodr)))
}



# ----------
( lrstat_ref <- as.numeric(2 * (logLik(amod) - logLik(nmod))) )



# ----------
hist(lrstat, freq = FALSE, ylim = c(0, 0.1), xlim = c(-90, 40))
lines(density(lrstat), col = "blue")
abline(v = lrstat_ref, col = "red")
box()


mean(lrstat > lrstat_ref)


