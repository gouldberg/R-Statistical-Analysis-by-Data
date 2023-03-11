setwd("//media//kswada//MyFiles//R//work_women")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  work_women
# ------------------------------------------------------------------------------

work <- read.table("work_women.csv", header = TRUE, row.names = 1, sep = ";")

str(work)

dim(work)


colnames(work) <- c("stay.at.home", "part.time.work", "full.time.work", "totally.agree", "mostly.agree", "mostly.disagree", "totally.disagree")

names(work)

work



# ----------
# we divide into two tables
( work1 <- work[,1:3] )

( work2 <- work[,4:7] )



# ------------------------------------------------------------------------------
# Check the questionnaires and the margins
# ------------------------------------------------------------------------------

# Question 1:
# Out of the following three options, which best corresponds to your image of the ideal family:

# Response to Question 1 (in row)
# 1. both parents work equally:  a family where both parents each have a profession which interests them equally and
# where housework and childcare are equally shared.
# 2. husband works more:  a fmily where the mother has aprofession, but which is less time-consuming than that of the father, and
# where she carries out the majority of the housework and childcare.
# 3. only the husband works:  a family where only the husband works and where the wife stays at home.


# Question 2:
# When identifying a difference between the period when children are very young and when they start school, what do you think is the best activity for a mother

# Response to Question 2 (in column 1 to 3)
# The most suitable activity for a mother when the children go to school, "Stay at home", "Part-time work" or "Full-time work"


# Question 3:
# What do you think of the followin opinion:  Women who do not work feel like they're cut off from the world ?

# Response to Question 3 (in column 4 to 7)
# "totally agree", "somewhat agree", "somewhat disagree", "totally disagree"


addmargins(as.matrix(work1))

addmargins(as.matrix(work2))



# ------------------------------------------------------------------------------
# spline plot:  a stacked barchart of the row percentages
# ------------------------------------------------------------------------------

spineplot(t(as.matrix(work1)))


# -->
# Note that there is some linear associations between current 


spineplot(t(as.matrix(work2)))

