setwd("//media//kswada//MyFiles//R//wheat")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wheat
#   - The presence of sprouted or diseased kernels in wheat can reduce the value of a wheat producer's entire crop.
#     It is important to identify these kernels after being harvested but prior to sale. To facilitate this identification process,
#     automated systems have been developed to separate healthy kernels from the rest.
#     Improving these systems requires better understanding of the measureable ways in which healthy kernels differ from kernels that
#     have sprouted prematurely or are infected with afungus ("Scrab").
#   - To this end, Martin et al. (1998) conducted a study examining numerous physical properties of kernels -- density, hardness, size, weight,
#     and moisture content -- measured on a sample of wheat kernels from two different classes of wheat, hard red winter (hrw) and soft red winter (srw).
#     Each kernel's condition was also classified as "Healthy", "Sprout", or "Scab" by human visual inspection.
#   - In the data provided by the authors of this paper, we have measurements from 275 wheat kernels. Our data is only a portion of the data
# ------------------------------------------------------------------------------

wheat <- read.csv(file = "//media//kswada//MyFiles//references//AnalysisOfCategoricalDataWithR//Chapter3//wheat.csv")

str(wheat)

car::some(wheat)




# ------------------------------------------------------------------------------
# basic analysis:  Parallel Coordinates Plot
# ------------------------------------------------------------------------------
# Reorder variables because class is binary (may distort plot)
# Create indicator variable for class
wheat2 <- data.frame(kernel = 1:nrow(wheat), wheat[,2:6],  
                   class.new = ifelse(test = wheat$class == "hrw", yes = 0, no = 1))

head(wheat2)



# ----------
# Colors by condition:
wheat.colors <- ifelse(test = wheat$type=="Healthy", yes = "black", no = ifelse(test = wheat$type=="Sprout", yes = "red", no = "green"))
# Line type by condition:
wheat.lty <- ifelse(test = wheat$type=="Healthy", yes = "solid", no = ifelse(test = wheat$type=="Sprout", yes = "longdash", no = "dotdash"))



# ----------
library(MASS)

parcoord(x = wheat2, col = wheat.colors, lty = wheat.lty)
legend(x = 6.15, y = 0.75, legend = c("Healthy", "Sprout", "Scab"), lty = c("solid", "longdash", "dotdash"), col=c("black", "red", "green"), cex=0.8, bty="n")



# ----------
# What observation has the largest weight
wheat[wheat$weight == min(wheat2$weight),]  # 269
order(wheat$size)


# -->
# We see that:
#  - Scab kernels generally have smaller density, size, and weight values
#  - Healthy kernels may have higher densities
#  - There is much overlap for healthy and sprout kernes, and
#  - The moisture content appears to be related to hard or soft red winter wheat class.




# ------------------------------------------------------------------------------
# basic analysis:  Parallel Coordinates Plot, highlighting observation #269
# ------------------------------------------------------------------------------
wheat[269,]  # scab
wheat.colors <- ifelse(test = wheat$type=="Healthy", yes = "black", no = ifelse(test = wheat$type=="Sprout", yes = "red", no = "green"))
wheat.colors[269] <- "purple"
line.width <- c(rep(x = 1, times = 268), 10, rep(x = 1, times = 6))
parcoord(x = wheat2, col = wheat.colors, lwd = line.width, lty = wheat.lty,  main = "Parallel coordinate plot for wheat data - highlight kernel 269")



# ------------------------------------------------------------------------------
# basic analysis:  Parallel Coordinates Plot, sort by wheat type
# ------------------------------------------------------------------------------
wheat.colors2 <- ifelse(test = wheat$type=="Healthy", yes = 1, no = ifelse(test = wheat$type=="Sprout", yes = 2, no = 3))
wheat3 <- data.frame(wheat.colors2, wheat2)     
parcoord(x = wheat3[order(wheat.colors2),], col = wheat.colors[order(wheat.colors2)],  main = "Parallel coordinate plot for wheat data - sort by Type")



# ----------
# Another way to do these plots with brushing (highlight parts of plot to change colors)
# library(iplots)  
# ipcp(wheat3[order(wheat.colors2),])

