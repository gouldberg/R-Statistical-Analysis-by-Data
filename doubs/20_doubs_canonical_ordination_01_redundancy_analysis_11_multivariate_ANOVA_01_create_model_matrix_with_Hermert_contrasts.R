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



# ------------------------------------------------------------------------------
# Two-way balanced factorial design
#   - This is fictious balanced two-way ANOVA design !!!!
# ------------------------------------------------------------------------------

# Creation of a factor 'elevation' (3 levels, 9 sites each)
ele.fac <- gl(3, 9, labels = c("high", "mid", "low"))


# Creation of a factor mimicking 'pH'
pH.fac <- as.factor(c(1, 2, 3, 2, 3, 1, 3, 2, 1, 2, 1, 3, 3, 2, 1, 1, 2, 3, 2, 1, 2, 3, 2, 1, 1, 3, 3))



# two-way factorial design balanced
table(ele.fac, pH.fac)



# ------------------------------------------------------------------------------
# Create model.matrix with Helmert contrasts for factors and interactions
# ------------------------------------------------------------------------------

# Creation of Helmert contrasts for the factors and the interaction
ele.pH.helm <- model.matrix(~ ele.fac * pH.fac, contrasts = list(ele.fac = "contr.helmert", pH.fac = "contr.helmert"))[, -1]

ele.pH.helm

colnames(ele.pH.helm)



# ----------
# no interaction
ele.pH.helm2 <- model.matrix(~ ele.fac + pH.fac, contrasts = list(ele.fac = "contr.helmert", pH.fac = "contr.helmert"))[, -1]

colnames(ele.pH.helm2)



# ----------
# Check property 1 of Helmert contrasts : all variables sum to 0
apply(ele.pH.helm, 2, sum)



# ----------
# Check property 2 of Helmert contrasts: their crossproducts must be 0 within and between groups (factors and interaction)
crossprod(ele.pH.helm)
