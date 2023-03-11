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
# Assess 3-way interactions
# ------------------------------------------------------------------------------

ylab.name = expression(paste("Prevalence of ",italic("E. oatesi"), sep=""))

xyplot(Worms ~ Length | Sex * Area,
       pch = 16,
       xlab = list(label = "Length", cex = 1.5),
       ylab = list(label = ylab.name, cex = 1.5),
       data = Par, 
       col=1)



# -->
# It appears that the data set is too small to include a 3-way interaction term and for a 2-way interaction term between length adn area.