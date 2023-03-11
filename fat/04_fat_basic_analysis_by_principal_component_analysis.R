setwd("//media//kswada//MyFiles//R//fat")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  fat
# ------------------------------------------------------------------------------

data(fat, package="faraway")

str(fat)



# ------------------------------------------------------------------------------
# check the correlation among predictors
# ------------------------------------------------------------------------------

par(mfrow=c(1,3))

plot(neck ~ knee, fat)
plot(chest ~ thigh, fat)
plot(hip ~ wrist, fat)


# -->
# we see that the body circumferences are strongly correlated.
# Although we may have many predictors, there may be less information that the number of predictors might suggest.



# ------------------------------------------------------------------------------
# principal component analysis
# ------------------------------------------------------------------------------
# we consider only the circumference measurements in the fat dataset and compute the principal components decomposition.
names(fat)

cfat <- fat[,9:18]

prfat <- prcomp(cfat)


# ----------
# rotation matrix
prfat$rot

# principal components
prfat$x



# ----------
summary(prfat)


# -->
# we see that the 1st principal component explains 86.7% of the variation in the data while the last few components account for very little of the variation.


# ----------
# we see that the chest, abdomen, hip and thigh measures dominate the first principal component.
round(prfat$rot[,1], 2)



# ----------
# we might prefer to scale the variables for size by converting to standard units
# after scaling, we see that the proportaion of variability explained by the first component drops to 70.2%
prfatc <- prcomp(cfat, scale = TRUE)

summary(prfatc)



# ----------
# the first principal component has very similar coefficients for all the variables.
# It is useful to interpret this as "overall size" since it is roughly proportional to a mean across all these (standardized) variables.
# One interpretation of this dominant first principal component is that body shapes in men are mostly proportional.
# Bigger men tend to be just larger all-round versions of smaller men.
round(prfat$rot[,1], 2)



# ----------
# The other principal components describe how the data vary in ways orthogonal to this first PC.
# The second principal components is roughly a contract between the body center measures of chest, abdomen, hip and thigh circumferences against
# the extremities of forearm, wrist, and ankle measures.
# This could be viewed as a relative measure of where the body is carrying its weight.
# This represents only 7.3% of the total variation so it is not substantial but is the largest component of variation after the first PC.
round(prfatc$rot[,2], 2)





