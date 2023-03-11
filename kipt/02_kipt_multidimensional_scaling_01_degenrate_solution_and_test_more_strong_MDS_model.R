setwd("//media//kswada//MyFiles//R//kipt")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  KIPT
#   - The inter-correlations of eight test items of the Kennedy Institute Phonics Test (KIPT), a test for reading skills.
# ------------------------------------------------------------------------------

data(KIPT, package = "smacof")


str(KIPT)


car::some(KIPT)



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

diss <- sim2diss(KIPT)



# ----------
# Without "eps" argument, mds() will use the default value eps = 1e-06 which causes it to stop earlier.
# We set the argument "eps" to an extra-small value here to make the program iterate on and on until it reaches such an exotically small raw Stress value
# if it can be reached in itmax = 3333 iterations.

fit1 <- mds(diss, type = "ordinal", eps = 1e-11)
# fit1 <- mds(diss, type = "ordinal")

fit2 <- mds(diss, type = "interval")

fit3 <- mds(diss, type = "ratio")



# ----------
fit1$stress

fit2$stress

fit3$stress



# ----------
par(mfrow = c(2,3))
plot(fit1, main="Ordinal MDS")
plot(fit2, main="Interval MDS")
plot(fit3, main="ratio MDS")

plot(fit1, plot.type="Shepard", main=paste0("Ordinal MDS (Stress1 = ", round(fit1$stress, 2), ")"))
plot(fit2, plot.type="Shepard", main=paste0("Interval MDS (Stress1 = ", round(fit2$stress, 2), ")"))
plot(fit3, plot.type="Shepard", main=paste0("Ratio MDS (Stress1 = ", round(fit3$stress, 2), ")"))


# -->
# For fit1, the Stress value is zero, so this MDS solution is formally perfect.
# Yet, the Shepard diagram of this solution reveals a peculiar relation of data and distances:
# Although the data scatter evenly over the interval from 0.44 to 0.94, they are not represented by distances with a
# similar distribution, but rather by two clearly distinct classes of distances so that the regression line makes just one big step.

# The large and the small distances, respectively, can be reordered arbitrarily as long as all similarities within the blocks remain greater than
# all between-block similarities. Any such reordering will have no effect on the Stress value.

# The reason for such a degenerate solution is that the data have a peculiar structure.
# They form three subgroups, with high within- and lo betwen-correlations.
# With ordinal MDS, such data can always be scalied with zero Stress.
# The problem becomes more likely if the number of variables is small (n <= 8)

# If the Shepard diagram suggests that the MDS solution is defenerate, then the natural next step for the user is testing a stronger MDS model,
# such as "interval" or "ratio".

