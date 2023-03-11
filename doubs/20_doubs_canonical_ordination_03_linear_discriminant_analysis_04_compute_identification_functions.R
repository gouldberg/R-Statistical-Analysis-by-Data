setwd("//media//kswada//MyFiles//R//doubs")

packages <- c("dplyr", "vegan", "cluster", "dendextend", "pvclust", "labdsv", "RColorBrewer")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Doubs
# ------------------------------------------------------------------------------

load("./data/Doubs.RData")


dim(spe)
car::some(spe)


dim(env)
car::some(env)


dim(spa)
car::some(spa)


dim(fishtraits)
car::some(fishtraits)


dim(latlong)
car::some(latlong)



# ----------
# remove site 8, which have zero species
spe <- spe[-8,]
env <- env[-8,]
spa <- spa[-8,]
# latlong <- latlong[-8,]


# ----------
# data preparation
dfs <- env[, 1]
env2 <- env[, -1]
slo2 <- rep(".very_steep", nrow(env))
slo2[env$slo <= quantile(env$slo)[4]] <- ".steep"
slo2[env$slo <= quantile(env$slo)[3]] <- ".moderate"
slo2[env$slo <= quantile(env$slo)[2]] <- ".low"
slo2 <- factor(slo2, levels = c(".low", ".moderate", ".steep", ".very_steep"))
env3 <- env2
env3$slo <- slo2
envtopo <- env2[, c(1 : 3)]
envchem <- env2[, c(4 : 10)]



# ----------
# Log transform ele and bod
env.pars3 <- cbind(log(env2$ele), env2$oxy, log(env2$bod))
colnames(env.pars3) <- c("ele.ln", "oxy", "bod.ln") 
rownames(env.pars3) <- rownames(env2)



# ------------------------------------------------------------------------------
# Computation of LDA - identification functions (on unstandardized variables)
# ------------------------------------------------------------------------------

# Alternate coding without formula interface:  spe.lda <- lda(env.pars3.df, gr)

library(MASS)

env.pars3.df <- as.data.frame(env.pars3)


( spe.lda <- lda(gr ~ ele.ln + oxy + bod.ln, data = env.pars3.df) )


summary(spe.lda)



# -->
# TECHNICAL NOTE:  Running the function with unstandardized explanatory variables will allow the classsification of new objects
# while producing the exact same discrimination as a run with standardized variables.



# ------------------------------------------------------------------------------
# group means for the 3 variables
# ------------------------------------------------------------------------------

spe.lda$means



# ------------------------------------------------------------------------------
# Extract the unstandardized identification functions (matrix C, eq. 11.33 in Legendre and Legendre 2012)
# ------------------------------------------------------------------------------

# coefficients of linear dicriminants
( C <- spe.lda$scaling )




# ------------------------------------------------------------------------------
# Classification of tow new objects (identification)
# ------------------------------------------------------------------------------

# Classification of two new objects (identification)
# A new object is created with two sites: 
#     (1) ln(ele) = 6.8, oxygen = 9 and ln(bod) = 0.8 
# and (2) ln(ele) = 5.5, oxygen = 10 and ln(bod) = 1.0

newo <- data.frame(c(6.8, 5.5), c(9, 10), c(0.8, 1))

colnames(newo) <- colnames(env.pars3)

newo



# ----------
( predict.new <- predict(spe.lda, newdata = newo) )



# ----------
predict.new$posterior



# -->
# Posterior probabilities of new objects (rows) to belong to groups 1-4 (columns)
# The 1st object (row 1) has the highest probability (0.88) to belong to group 2 and the second object to group 3


