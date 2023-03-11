setwd("//media//kswada//MyFiles//R//berkeley_growth")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  berkeley growth
#   - The data from the Berkeley Growth Study (Tuddenham and Snyder, 1954).
# ------------------------------------------------------------------------------

data("growth", package = "fda")

str(growth)



# ----------
# 39 boys and 31 points
# a 31 by 39 numeric matrix giging the heights in centimeters of 39 boys at 31 ages
dim(growth$hgtm)


# 54 girls and 31 points
# a 31 by 54 numeric matrix giging the heights in centimeters of 54 girls at 31 ages
dim(growth$hgtf)



# ----------
head(growth$hgtm)


head(growth$hgtf)



# ----------
growth$age


# -->
# The ages are not equally spaced;
# There are four measurements while the child is one year old, annual measurements from two to eight years,
# followed by heights measured biannually



# ------------------------------------------------------------------------------
# Plot each of first 10 boys and girls
# ------------------------------------------------------------------------------

par(mfrow=c(1,1))
ylim = with(growth, range(hgtm, hgtf))

with(growth, matplot(age, hgtm[, 1:10], type='l', lty='dashed', ylab='height (cm)'))
with(growth, matlines(age, hgtf[, 1:10], lty='solid'))
legend('topleft', legend=c('girls', 'boys'), lty=c('solid', 'dashed'))


# -->
# This plot suggests that boys generally become taller than girls.



# ------------------------------------------------------------------------------
# smoothing first boys and girls
# ------------------------------------------------------------------------------

growthbasis = create.bspline.basis(breaks=growth$age, norder=6)

growfdPar = fdPar(growthbasis, 3, 10^(-0.5))


hgtffd = with(growth, smooth.basis(age,hgtf,growfdPar))

hgtmfd = with(growth, smooth.basis(age,hgtm,growfdPar))



# ----------
par(mfrow=c(2,1))
plot(hgtffd)
plot(hgtmfd)



# ------------------------------------------------------------------------------
# Functional t-Tests
#   - The test statistic that we use is the maximum value of the multivariate T-test.
#     To find a critical value of this statistic, we use a permuation test.
#   - We perform the following procedure:
#       1. Randomly shuffle the labels of the curves
#       2. Recalcualte the maximum of T(t) with the new labels.
# ------------------------------------------------------------------------------

# This executes a permutationtest and generates the graphics.
# It uses a default value of 200 random shuffles, which is more than adequate for such a large difference.
tres = tperm.fd(hgtffd$fd, hgtmfd$fd)



# -->
# It is appearent that there is little evidence for difference up to the age of around 12, about the middle of female growth spurt,
# at which point the boys rapidly become taller.
# We can conclude that the main reason why boys end up taller than girls is that they get an extra couple of years of growth on the average.