setwd("//media//kswada//MyFiles//R//work_women")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  work_women
#  - Women's attitudes to women's work in France in 1970 from questionnaires completed by 1724 women.
#  - This data represents a turning point in history in sociological terms.
#    The end of the 1960s also marked the end of a number of feminist struggles, particularly regarding women's access to paid work.
#    (In France, women could not work without their husband's consent before 1965.)
# ------------------------------------------------------------------------------

work <- read.table("work_women.csv", header = TRUE, row.names = 1, sep = ";")

str(work)

dim(work)


colnames(work) <- c("stay.at.home", "part.time.work", "full.time.work", "totally.agree", "mostly.agree", "mostly.disagree", "totally.disagree")

names(work)

work



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

summary(work)
