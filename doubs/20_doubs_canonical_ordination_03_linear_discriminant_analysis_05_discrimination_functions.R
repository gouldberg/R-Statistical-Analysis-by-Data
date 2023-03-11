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
# Computation of LDA - discrimination funcions (on standardized variables)
# ------------------------------------------------------------------------------

# scaling
env.pars3.sc <- as.data.frame(scale(env.pars3.df))



# ----------
spe.lda2 <- lda(gr ~ ., data = env.pars3.sc)



# ----------
# group means
spe.lda2$means



# ----------
# Extract the classification functions
( C2 <- spe.lda2$scaling )



# ----------
# Compute the canonical eigenvalues
spe.lda2$svd^2



# ----------
# Position the objects in the space of the canonical variates
# alternative way :  Fp2 <- as.matrix(env.pars3.sc) %*% C2
( Fp2 <- predict(spe.lda2)$x )



# Classification of the objects
( spe.class2 <- predict(spe.lda2)$class )



# ----------
# Posterior probabilities of the objects to belong to the groups
# (rounded for easier interpretation)
( spe.post2 <- round(predict(spe.lda2)$posterior, 2) )



# ----------
# Contingency table of prior versus predicted classifications
( spe.table2 <- table(gr, spe.class2) )



# Proportion of correct classification (classification success)
diag(prop.table(spe.table2, 1))




# ------------------------------------------------------------------------------
# plot LDA results
# ------------------------------------------------------------------------------

# Some trial and error is needed to adjust the lengths of the arrows with argument mul.coef.

source("./functions/plot.lda.R")

plot.lda(lda.out = spe.lda2, groups = gr, plot.sites = 2, plot.centroids = 1, mul.coef = 2.35)



