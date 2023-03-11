setwd("//media//kswada//MyFiles//R//chicken2")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  chicken2
#   - Data related to 43 chickens having undergone one of the six following diet conditions:
#       - normal diet (N), fasting for 16 hours (F16), fasting for 16 hours then refed for 5 hours (F16R5),
#         fasting for 16 hours then refed for 16 hours (F16R16), fasting for 48 hours (F48), and
#         fasting for 48 hours then refed for 24 hours (F48R24)
#   - At the end of the diet, the genes were analysed using DNA chips, and the expression of 7407 genes retained for all the chickens.
#     A biologist selected the most pertinent genes, since at the beginning, more than 20,000 genes were identified by DNA chips.
#     The data were then preprocessed in a standard manner for DNA chips (normalisation, eliminating the chip effect, etc.)
#   - The data table to be analysed is a rectangular array with far fewer individuals than variables: 43 rows (chicken) and 7407 columns (genes)
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
# basics
# ------------------------------------------------------------------------------
