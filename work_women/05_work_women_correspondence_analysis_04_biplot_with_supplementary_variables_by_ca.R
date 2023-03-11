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
# Check the association between the responses of Q1 and Q3
# ------------------------------------------------------------------------------

chisq.test(as.matrix(work2))


# -->
# relationship between responses of Q1 and Q3 is highly significant


assocstats(as.matrix(work2))

assocstats(as.matrix(work1))


# -->
# but moderate in intensity
# in particular, it is less intense  than the relationship between Q1 and Q2  (in terms for Cramer's V)



# ------------------------------------------------------------------------------
# Correspondence Analysisis Biplot with supplementary variables
# ------------------------------------------------------------------------------

library(ca)

fit_ca <- ca(work, supcol = 4:ncol(work))


graphics.off()
par(mfrow = c(1,1))
plot(fit_ca, arrows = c(FALSE, TRUE))


# ----------
fit_ca


summary(fit_ca)



# ----------
# principal inertias
fit_ca$sv^2


