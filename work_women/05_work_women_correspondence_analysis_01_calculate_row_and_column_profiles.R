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
# Row Proriles
# ------------------------------------------------------------------------------

ddr <- rbind(work1, apply(work1, 2, sum))

rownames(ddr)[4] <- "Mean Profile"

ddr

round(addmargins(prop.table(as.matrix(ddr), margin = 1), margin = 2), 3)



# -->
# Women who answered "only husband works" responded:
# "Stay at home" more often than the average population (average row profile):  26.5% compared with 16.5%
# "Full-time work" less often than the average population:  10.4% compared with 18.4%

# Similarly, women who answered "Both work equally" responded:
# "Stay at home" less often than the average population (average row profile):  5.0% compared with 16.5%
# "Full-time work" more often than the average population:  40.6% compared with 18.4%



# ------------------------------------------------------------------------------
# Column Proriles
# ------------------------------------------------------------------------------

ddc <- cbind(work1, apply(work1, 1, sum))

colnames(ddc)[4] <- "Mean Profile"

ddc

round(addmargins(prop.table(as.matrix(ddc), margin = 2), margin = 1), 3)



# -->
# Women who answered "stay at home" responded:
# Only the husband works more often than the average population (average column profile):  84.9% compared with 52.7%
# Both parents work equally less often than the average population:  4.6% compared with 15.1%

# Similarly, women who answered full-time work responded:
# Only the husband works less often than the average population (29.7% compared with 52.7%)
# Both parents work equally more often than the average population:  33.4% compared with 15.1%


# -->
# The category par-time work is extremely close to the centre of graviity, thus indicating a profile near to the average profile

# The women who answered part-time work cannot be distinguished from the rest of the population (in terms of their responses to question 1).
# This answer is not informative.



