setwd("//media//kswada//MyFiles//R//death")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  death
#   - 
# ------------------------------------------------------------------------------

death <- read.table("death.csv", header = TRUE, row.names = 1, sep = ";")

str(death)

dim(death)

head(death)


colnames(death) <- c("0_1", "1_4", "5_14", "15_24", "25_34", "35_44", "45_54", "55_64", "65_74", "75_84", "85_94", "95+")



# ------------------------------------------------------------------------------
# Inertia
# ------------------------------------------------------------------------------

res.ca = CA(death, row.sup = 66:nrow(death), graph = FALSE)

summary(res.ca, nb.dec = 4)



# ----------
# Total inertia (= Phi^2)
sum(res.ca$eig[,1])
chisq.test(death[1:65,])$statistic / sum(death[1:65,])



# ----------
# Cramer's V = sqrt(Phi^2 / min(r-1, c-1))
sqrt( ( chisq.test(death[1:65,])$statistic / sum(death[1:65,]) ) / min(12-1, 65-1) )



# ----------
# chart of eigenvalues
par(mfrow = c(1,1))
barplot(res.ca$eig[,1], main = "Eigenvalues", names.arg = 1:nrow(res.ca$eig))



# -->
# THe observed X^2 has a value of 1,080,254, and the associated p-value is extremely close to zero.
# Although the test hypotheses are not validated (many of the cells have theoretical sample sizes of less than 5),
# the p-value is so low that the significance cannot be questioned.
# Total inertia is equal to Phi^2 = 1.0213, and the intensity of the relationship, measured using Cramer's V, is high = 0.305
# (Cramer's V = 1 would indicate that exclusive association between ech age group and a group of causes of mortality)

# The sequence of eigenvalues identifies 3 dimensions.
# These 3 dimensions account for 92.6% of the total inertia and therefore effectively summarise overall vriablitity (whithin a 12 - 1 = 11 dimensional space).
# We can therefore focus our interpretation on these first three dimensions.


