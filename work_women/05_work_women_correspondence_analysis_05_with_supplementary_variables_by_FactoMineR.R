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
# Simple Correspondence Analysis by FactoMineR  with supplementary variables
# ------------------------------------------------------------------------------

# In correspondence analysis, we provide the matrix as supplementary variables (as "col.sup" not "qunati.sup")

res.ca2 <- CA(work, col.sup = 4:ncol(work))

summary(res.ca2)



# -->
# The cloud of categories for response of Q3 is more tightly clustered around the origin than those of the other two variables.
# Here, we reencounter the fact that the relationship between the responses to Q1 and Q3 is less intense than that between the responses to Q1 and Q2.

# The category "totally agree" is further from the origin of the axes than "totally disagree";
# It would therefore seem to be more typical of a favourable attitude towards women's work than totally disagree is typical of an unfavarouble attitude.



# ----------
# compare with ca  --> same (except for arrows)

par(mfrow=c(1,2))

plot(fit_ca, arrows = c(FALSE, TRUE))
plot(res.ca2)



