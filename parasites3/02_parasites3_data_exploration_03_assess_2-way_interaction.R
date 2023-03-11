setwd("//media//kswada//MyFiles//R//parasites3")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  parasites3
# ------------------------------------------------------------------------------

Par <- read.table(file = "Parasites3.txt", header = TRUE)


str(Par)

dim(Par)



# ----------
Par$Worms <- Par$Elytrophalloides_oatesi



# ------------------------------------------------------------------------------
# Data exploration:  Year and Area
# ------------------------------------------------------------------------------

# Sampling effort over time and space
with(Par, table(Year, Area))



# -->
# Year effect may actually be an are effect.
# San Jorge was sampled in 1998 and the other two zonesin 1999.



# ------------------------------------------------------------------------------
# Data exploration:  Length and Sex
# ------------------------------------------------------------------------------
boxplot(Length ~ Sex, 
        data = Par,
        xlab = "Sex",
        ylab = "Length")


# -->
# Another potential problem is collinearity between length and sex, but indicates that this is not a concern in these data.
