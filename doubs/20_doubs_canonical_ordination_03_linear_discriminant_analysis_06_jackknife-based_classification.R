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
# LDA with jackknife-based classification (i.e., leave-one-out cross-validation)
# ------------------------------------------------------------------------------

( spe.lda.jac <- lda(gr ~ ele.ln + oxy + bod.ln, data = env.pars3.sc, CV = TRUE) )


summary(spe.lda.jac)



# ----------
# Numbers and proportions of correct classification
spe.jac.class <- spe.lda.jac$class

spe.jac.table <- table(gr, spe.jac.class)



# ----------
# Classification success
diag(prop.table(spe.jac.table, 1))

diag(prop.table(spe.table2, 1))



# -->
# This is no as good as the result in spe.table2.
# However, spe.table2 shows an a posteriori classification of the objects that have been used in the computations, it is too optimistic.
