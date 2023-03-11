setwd("//media//kswada//MyFiles//R//gmo")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gmo
# ------------------------------------------------------------------------------

gmo <- read.table("gmo.csv", header = TRUE, sep = ";", dec = ".")

dim(gmo)

str(gmo)


car::some(gmo)



# ------------------------------------------------------------------------------
# summary or distributions
# ------------------------------------------------------------------------------

summary(gmo[,1:16])


Hmisc::describe(gmo[,1:16])



# -->
# Note the the number of categories are different by categorical variable (2 or 4 categories)

# It must be noted that:
# Those variables which have more categories have greater inertia, but this inertia is shared over a greater number of components.
# Therefore, the first dimensions will be made up of those variables with very few categories, and those with many.




# ------------------------------------------------------------------------------
# Recode rare categorical value
# ------------------------------------------------------------------------------

levels(gmo$Position.Al.H)[4] <- levels(gmo$Position.Al.H)[1]


levels(gmo$Position.Culture) <- c("Favourable", "Somewhat Against", "Totally opposed", "Favourable")


summary(gmo[,1:16])




# ------------------------------------------------------------------------------
# Summary of descriptive variables
# ------------------------------------------------------------------------------

summary(gmo[,17:21])

summary(gmo[,17:21], maxsum = Inf)


Hmisc::describe(gmo[,17:21])


