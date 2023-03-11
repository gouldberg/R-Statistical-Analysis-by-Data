setwd("//media//kswada//MyFiles//R//chicken2")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  chicken2
# ------------------------------------------------------------------------------

chicken <- read.table("chicken.csv", header = TRUE, sep = ";", dec = ".", row.names = 1)

str(chicken)

dim(chicken)


car::some(chicken)



# ----------
chicken <- as.data.frame(t(chicken))

diet <- as.factor(c(rep("N", 6), rep("F16", 5), rep("F16R5", 8), rep("F16R16", 9), rep("F48", 6), rep("F48R24", 9)))

chicken <- cbind.data.frame(diet, chicken)

colnames(chicken)[1] <- "Diet"


dim(chicken)

str(chicken)



# ------------------------------------------------------------------------------
# distribution of average and standard deviation
# ------------------------------------------------------------------------------

mean(chicken[,2], na.rm = TRUE)


hist(apply(chicken[,-1], 2, FUN = mean, na.rm = TRUE))

hist(apply(chicken[,-1], 2, FUN = sd, na.rm = TRUE))

