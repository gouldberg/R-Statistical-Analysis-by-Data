setwd("//media//kswada//MyFiles//R//mammalsleep")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  mammalsleep
# ------------------------------------------------------------------------------

data("mammalsleep", package = "faraway")


str(mammalsleep)


dim(mammalsleep)


car::some(mammalsleep)



# ----------
# Calculate dream / sleep
mammalsleep$pdr <- with(mammalsleep, dream / sleep)


head(mammalsleep)



# ------------------------------------------------------------------------------
# Data Exploration
# ------------------------------------------------------------------------------

# 62 animals
rownames(mammalsleep)



# ----------
summary(mammalsleep)


# -->
# There are many missing value records



# ----------
par(mfrow = c(1,1))

hist(mammalsleep$pdr)


# -->
# We notice that the proportion of time spent dreaming varies from zero up to almost half the time.



# ----------
psych::describe(mammalsleep)


Hmisc::describe(mammalsleep)




# -->
# body, brain, lifespan, and gestation requires log-transformation.  (skewness > 1.5)

