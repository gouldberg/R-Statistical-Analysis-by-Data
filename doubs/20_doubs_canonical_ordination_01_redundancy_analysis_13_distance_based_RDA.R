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
# this is fictious design
ele.fac <- gl(3, 9, labels = c("high", "mid", "low"))
pH.fac <- as.factor(c(1, 2, 3, 2, 3, 1, 3, 2, 1, 2, 1, 3, 3, 2, 1, 1, 2, 3, 2, 1, 2, 3, 2, 1, 1, 3, 3))
ele.pH.helm <- model.matrix(~ ele.fac * pH.fac, contrasts = list(ele.fac = "contr.helmert", pH.fac = "contr.helmert"))[, -1]



# ------------------------------------------------------------------------------
# Distance-based redundancy analysis (db-RDA)
#   - Legendre and Anderson (1999) proposed the method of distance-based redundancy analysis.
#     They showed that RDA could be used as a form of ANOVA that was applicable to community composition data
#     if these were transformed in some appropriate way, which went through the calculation of a dissimilarity matrix of the user's choice.
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# preparation:  'ele' factor with factor pH
# ------------------------------------------------------------------------------

# Rename columns of matrix of Helmert contrasts (for convenience)
colnames(ele.pH.helm) <- c("ele1", "ele2", "pH1", "pH2", "ele1pH1", "ele1pH2", "ele2pH1", "ele2pH2" )



# Create the matrix of covariables. MUST be of class matrix, NOT data.frame
covariables <- ele.pH.helm[, 3:8]



# ------------------------------------------------------------------------------
# Compute dissimilarity response matrix:  square-root percentage difference
# ------------------------------------------------------------------------------

# vegan’s vegdist()
spe.bray27 <- vegdist(spe[1:27, ], "bray")

# … or with function dist.ldc() of adespatial
# spe.bray27 <- dist.ldc(spe[1:27, ], "percentdiff")



# ------------------------------------------------------------------------------
# Distance-based redundancy analysis (db-RDA)
# 1. dbrda() on the square-rooted dissimilarity matrix
# ------------------------------------------------------------------------------

bray.ele.dbrda <-  dbrda(sqrt(spe.bray27) ~ ele.pH.helm[, 1:2] + Condition(covariables))


anova(bray.ele.dbrda, permutations = how(nperm = 999)) 



# ------------------------------------------------------------------------------
# Distance-based redundancy analysis (db-RDA)
# 2. capscale() with raw (site by species) data
# ------------------------------------------------------------------------------

# Rename factor (cosmetic for plot)
ele.fac. <- ele.fac

bray.env.cap <- capscale(spe[1:27, ] ~ ele.fac. + Condition(covariables), data = as.data.frame(ele.pH.helm), distance = "bray", add = "lingoes", comm = spe[1:27, ])


anova(bray.env.cap, permutations = how(nperm = 999))



# ------------------------------------------------------------------------------
# Plot with "wa" scores to see dispersion of sites around the factor levels
# ------------------------------------------------------------------------------

triplot.rda(bray.env.cap, site.sc = "wa", scaling = 1)


# -->
# The results of the two analyses are slightly different because
# (1) the test is not performed in the same manner
# (2) the correction to make the response matrix Euclidean is not the same.



# ------------------------------------------------------------------------------
# Alternative ways of computing db-RDA
# ------------------------------------------------------------------------------

# 1. capscale() with raw (site by species) data

# Alternate coding with explicit covariables coming from same object as the constraining variables : 
bray.env.capscale <- capscale(spe[1:27, ] ~ ele1 + ele2 + Condition(pH1 + pH2 + ele1pH1 + ele1pH2 + ele2pH1 + ele2pH2), data = as.data.frame(ele.pH.helm), 
                              distance = "bray", add = "cailliez", comm = spe[1:27, ])


anova(bray.env.capscale, permutations = how(nperm = 999))



# ----------
# 2. PCoA with Lingoes (1971) correction:  Explicit steps
spe.bray27.lin <- pcoa(spe.bray27, correction = "lingoes") 



# ----------
spe.bray27.lingoes <- spe.bray27.lin$vectors.cor 


# Test of the factor ele. Factor pH and interaction, Helmert-coded, form the matrix of covariables
spe.L.ele.dbrda <- rda(spe.bray27.lingoes, ele.pH.helm[, 1:2], covariables) 


anova(spe.L.ele.dbrda, permutations = how(nperm = 999))



# ----------
# Same by staying in {vegan} and using wcmdscale() : 
spe.lingoes2 <- wcmdscale(spe.bray27, add = "lingoes") 

anova(rda(spe.lingoes2 ~ ele.pH.helm[, 1:2] + Condition(covariables)))

