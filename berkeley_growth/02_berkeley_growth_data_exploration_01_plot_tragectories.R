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
# data exploration
# ------------------------------------------------------------------------------

# a sample of 10 functional obsevations 
idx_f <- sample(1:54, size = 10, replace = FALSE)

idx_m <- sample(1:39, size = 10, replace = FALSE)


par(mfrow = c(1,2))

with(growth, matplot(age, hgtf[, idx_f], pch=1:10, cex = 0.7, col=1:10,
                     xlab='Age (years)', ylab='Height (cm)',
                     ylim=c(60, 190) , main = "10 samples of girls"))


with(growth, matplot(age, hgtm[, idx_m], pch=1:10, cex = 0.7, col=1:10,
                     xlab='Age (years)', ylab='Height (cm)',
                     ylim=c(60, 190), main = "10 samples of boys"))



