# ------------------------------------------------------------------------------
# Data: MonkeysV2
#
# 2-way nested random effects and a crossed random effect model
#
# ---------------------
# The Sick et al. (2014) paper gives a description of the 
# experimental design. Here are the essential parts:
# 1. Two monkey groups (large vs small). Coded as GroupSize
# 2. There are multiple focal hours per day (coded as "FocalHour")
# 3. In a focal hour we look for a groomer (coded as FocalGroomer)
# 4. We also write down the id of the receiver monkey  (coded as Receiver)
# 5. Record the time since sunrise, and the rank difference between 
#    focal groomer and receiver

# Aim: RankDifference = function(Time, Relatedness, GroupSize)

# Sensible interactions:
#    Time and relatedness  (used in paper)
#    Based on our common sense: Relatedness and groupsize  ?
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "Hmisc", "lattice", "ggplot2", "MASS", "VGAM", "car", "lme4")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

rm(list=ls())


# ------------------------------------------------------------------------------
# Load Data and Basics
# ------------------------------------------------------------------------------
data <- read.csv(file = "./ZuurZero-InflatedModelswithR/Sicketal2014data.csv", header=TRUE, sep=";")
dim(data)
str(data)

data <- subset(data, subordinate.grooms == "yes")

Hmisc::describe(data)

is.na(data)
colSums(is.na(data))

car::some(data)

# ------------------------------------------------------------------------------
# Check linearlity by scatterplot
# ------------------------------------------------------------------------------
p <- ggplot(data = data, aes(x = Time , y = RankDifference))
p <- p + geom_point()  #add points
p <- p + xlab("Time") + ylab("Rank difference")
p <- p + theme(text = element_text(size=15))
p <- p + geom_smooth(method = "lm")  #Add a smoother
p <- p + facet_wrap(~ GroupSize)
p          



