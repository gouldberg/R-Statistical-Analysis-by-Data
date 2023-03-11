# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
rm(list=ls())
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "vegan", "adespatial")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Doubs
# ------------------------------------------------------------------------------
load("./RefData/NumericalEcologyWithR/NEwR2-Data/Doubs.RData")

car::some(spe)
car::some(env)
car::some(spa)
car::some(fishtraits)
car::some(latlong)


# remove site 8, which have zero species
spe <- spe[-8,]
env <- env[-8,]
spa <- spa[-8,]
# latlong <- latlong[-8,]



# ------------------------------------------------------------------------------
# Preparation
# ------------------------------------------------------------------------------
# Set aside the variable 'dfs' (distance from the source) for later use
dfs <- env[, 1]


# Remove the 'dfs' variable from the env data frame
env2 <- env[, -1]


# ------------------------------------------------
# Recode the slope variable (slo) into a factor (qualitative) variable to show how these are handled in the ordinations
slo2 <- rep(".very_steep", nrow(env))
slo2[env$slo <= quantile(env$slo)[4]] <- ".steep"
slo2[env$slo <= quantile(env$slo)[3]] <- ".moderate"
slo2[env$slo <= quantile(env$slo)[2]] <- ".low"
slo2 <- factor(slo2, levels = c(".low", ".moderate", ".steep", ".very_steep"))
table(slo2)



# ------------------------------------------------
# Create an env3 data frame with slope as a qualitative variable
env3 <- env2
env3$slo <- slo2



# ------------------------------------------------
# Create two subsets of explanatory variables
# Physiography (upstream-downstream gradient)
envtopo <- env2[, c(1 : 3)]
names(envtopo)
# Water quality
envchem <- env2[, c(4 : 10)]
names(envchem)



# ------------------------------------------------------------------------------
# Redundancy analysis (RDA)
#
#  - NOTE that rda(Y, X, W) syntax doew not allow qualitative variables of class "factor" to be included in the explanatory and covariable matrices.
#    Therefore, in all but the simpelst applications, it is better to use the formula syntax such as rda(Y ~ var1 + factorA)
# ------------------------------------------------------------------------------
# Hellinger-transform the species dataset
spe.hel <- decostand(spe, "hellinger")



# RDA of the Hellinger-transformed fish species data, constrained by all the environmental variables contained in env3
( spe.rda <- rda(spe.hel ~ ., env3) )



# Scaling 2 (default)
# Note that 1st residual eigenvalue (PC1) is larger than the last canonical eigenvalue,
# meaning that the 1st residua structure (axis) of the data has more variance than some of the structures that can be explained by the explanatory variables in X
# The first two canonical axes explain together 56.1% of the total variance of the response dasta, the 1st axis alone explaning 45.4%, but these values are unadjusted.
# Since the adjusted R^2 of the RDA^2 is 0.5224,
# the proportions of acumulated constrained eigenvalues show that the 1st axis alone explaines = 0.5224 * 0.6243 = 32.6% variance.
# and the 1st two axes together = 0.5224 * 0.7712 = 40.3% variance.  --> We can be CONFIDENT that the major trends have been well modelled in this analysis.
summary(spe.rda, axes = 0)
( R2adj <- RsquareAdj(spe.rda)$adj.r.squared )


# Canonical coefficients from the rda object
coef(spe.rda)


# ----------------------------------------------
# adjusted R^2 = 1 - (n-1)/(n-m-1) * (1 - R^2)
# n:  the number of objects = nrow(spe) = 29,  m: the number of degrees of freedom of the model = the rank of the explanatory matrix = ncol(env3) + 3 - 1 = 12
# (the variable slo is qualitative with 4 factors)
# As a rule of thumb, this adjustment may be overly conservative when m > n/2

# Unadjusted R^2 retrieved from the rda object
( R2 <- RsquareAdj(spe.rda)$r.squared )

# Adjusted R^2 retrieved from the rda object
( R2adj <- RsquareAdj(spe.rda)$adj.r.squared )


# ----------------------------------------------
# Triplots of the rda results (lc scores)
# 1.    sp:  species
# 2-1.  lc:  fitted site scores (linear combinations of explanatory variables)
# 2-2.  wa:  site scores in the species space (weighted averages in CCA or weighted sums in RDA)  --> default
# 3.    cn:  constriants (the explanatory variables)

# The fitted site scores (lc) are strictly orthogonal linear combinations of the explanatory variables, representing clearly and exclusively what can be modelled
# using the situations because the "true" ordination diagram of RDA is the ordination of the Y-hat matrix of fitted values.
# On the other hand, wa (the site scores that are weighted sums of species) appear more robust to noise in the environmental variables.

# Site scores as linear combinations of the environmental variables
# the scores are multipled by 0.92 so that the arrows do not cover the names of the variables
# The bottom and lef-hand scales are for th eobjects and the response variables, the top and right-hand scales are for the explanatory varibles

# Oxygen (oxy), elevation (ele), nitrates (nit) and discharge (dis), as well as slope (mainly the level slo.very_steep) play an important role
# in the dispersion of the sites along the 1st axis.
graphics.off();  par(mfrow = c(1, 1));
plot(spe.rda, scaling = 1, display = c("sp", "lc", "cn"), main = "Triplot RDA spe.hel ~ env3 - scaling 1 - lc scores")
spe.sc1 <- scores(spe.rda, choices = 1:2, scaling = 1, display = "sp")
arrows(0, 0, spe.sc1[, 1] * 0.92, spe.sc1[, 2] * 0.92, length = 0, lty = 1, col = "red")
text(-0.75, 0.7, "a", cex = 1.5)


# Scaling 2
# Three groups of fish species correlated with different sets of explanatory variables:
# the brown trout (Satr), Eurasian minnow (Phph) and stone loach (Babl) are found in the 1st half of the sites, and are correlated with high oxygen content and slope as well as high elevation.
# The bleak (Alal), roach (Ruru) and European chub (Sgce), on the opposite, are related to sites 23, 24, 25 characterized by high phosphates (pho), ammonium (amm) and biological oxygen demand (bod) levels.
# Most other species are bunched together away from these extremes. They show mostly shorter projections, indicating that they are either present over most portions of the river or related to
# intermediate ecologicasl conditions.
plot(spe.rda, display = c("sp", "lc", "cn"), main = "Triplot RDA spe.hel ~ env3 - scaling 2 - lc scores")
spe.sc2 <- scores(spe.rda, choices = 1:2, display = "sp")
arrows(0, 0, spe.sc2[, 1] * 0.92, spe.sc2[, 2] * 0.92, length = 0, lty = 1, col = "red")
text(-0.82, 0.55, "b", cex = 1.5)



# ----------------------------------------------
# Triplots of the rda results (wa scores):  Site scores as weighted averages (vegan's default)
# Scaling 1 :  distance triplot
plot(spe.rda,  scaling = 1,  main = "Triplot RDA spe.hel ~ env3 - scaling 1 - wa scores")
arrows(0, 0, spe.sc1[, 1] * 0.92,  spe.sc1[, 2] * 0.92,  length = 0,  lty = 1,  col = "red")


# Scaling 2 (default) :  correlation triplot
plot(spe.rda, main = "Triplot RDA spe.hel ~ env3 - scaling 2 - wa scores")
arrows(0, 0, spe.sc2[, 1] * 0.92, spe.sc2[, 2] * 0.92, length = 0, lty = 1, col = "red")


# ----------------------------------------------
# Select species with goodness-of-fit at least 0.6 in the ordination plane formed by axes 1 and 2
# Triplots with homemade function triplot.rda(), scalings 1 and 2
source("./RefData/NumericalEcologyWithR/NEwR2-Functions/triplot.rda.R")
spe.good <- goodness(spe.rda)
sel.sp <- which(spe.good[, 2] >= 0.6)
par(mfrow = c(2, 1))
triplot.rda(spe.rda, site.sc = "lc", scaling = 1, cex.char2 = 0.7, pos.env = 3, pos.centr = 1, mult.arrow = 1.1, mar.percent = 0.05, select.spe = sel.sp)
text(-0.92, 0.72, "a", cex = 2)
triplot.rda(spe.rda, site.sc = "lc", scaling = 2, cex.char2 = 0.7, pos.env = 3, pos.centr = 1, mult.arrow = 1.1, mar.percent = 0.05, select.spe = sel.sp)
text(-2.82, 2, "b", cex = 2)



# ------------------------------------------------------------------------------
# Redundancy analysis (RDA):  Permutation Tests of RDA Results
#
#  - Due to widespread problems of non-normal distributions in ecologicasl data, classical parametric tests are often not appropriate in this field.
# ------------------------------------------------------------------------------
# Global test of the RDA result
anova(spe.rda, permutations = how(nperm = 999))


# Tests of all canonical axes
anova(spe.rda, by = "axis", permutations = how(nperm = 999))


# Apply Kaiser-Guttman criterion to residual axes
# The eigenvalues are compared with the mean of the residual eigenvalues only
spe.rda$CA$eig[spe.rda$CA$eig > mean(spe.rda$CA$eig)]



# ------------------------------------------------------------------------------
# Redundancy analysis (RDA):
# - Partial RDA: whether water chemistry significantly explains the fish species patterns when the effect of the topographic gradient is held constant
# ------------------------------------------------------------------------------
# Simple syntax; X and W may be in separate tables of quantitative variables
# Partitioning of variance:
#  - Conditioned:  the amount of variance that has been explained by the covariables and removed.
#  - Constrained:  the amount of variance uniquely explained by the explanatory variables.
#  - Unconstrained:  the residual variance.
# Eigenvalues, and their contribution to the variance after removing the contributions of conditioning variables
#  - These values and proportions are partial in the sense that the effects of the covariables have been removed. 
#  - The sum of all these eigenvalues corresponds therefore to the sum of the constrained and nconstrained (residual) variances, excluding the conditioned (i.e. removed) variance.
( spechem.physio <- rda(spe.hel, envchem, envtopo) )
summary(spechem.physio, axes = 0)


# Formula interface; X and W variables must be in the same data frame
# The results of the two analyses are identical.
(spechem.physio2 <- 
    rda(spe.hel ~ pH + har + pho + nit + amm + oxy + bod 
        + Condition(ele + slo + dis), data = env2))


# Test of the partial RDA, using the results with the formula interface to allow the tests of the axes to be run
anova(spechem.physio2, permutations = how(nperm = 999))
anova(spechem.physio2, permutations = how(nperm = 999), by = "axis")


# ----------------------------------------------
# Partial RDA triplots (with fitted site scores) with function triplot.rda
# Scaling 1
# The sites are not as cleanly ordered by their succession along the river. 
# This indicates that the chemical variables that are important for the fishes do not necessarily follow that order and 
# that the fish community responds significantly to these chemical constraints irrespective of their locations along the river.
# The hardness (har) and nitrates (nit) are less important to explain the fish community structure.
# These two variables are well correlated with the positions of the sites along the river, and therefore their apparent effect on the fish community may have been spurious and 
# has been removed by the analysis, which controlled for the effect of the physiographic variables.
par(mfrow = c(2, 1))
triplot.rda(spechem.physio, site.sc = "lc", scaling = 1, cex.char2 = 0.8, pos.env = 3, mar.percent = 0)
text(-0.58, 0.64, "a", cex = 2)


# Scaling 2
triplot.rda(spechem.physio, site.sc = "lc", scaling = 2, cex.char2 = 0.8, pos.env = 3, mult.spe = 1.1, mar.percent = 0.04)
text(-3.34, 3.64, "b", cex = 2)


# Alternative plot using plot.cca ---------------------------------
# Partial RDA triplots (with fitted site scores) with function triplot.rda
# Scaling 1
plot(spechem.physio, scaling = 1, display = c("sp", "lc", "cn"), main = "Triplot RDA spe.hel ~ chem | Topo - scaling 1 - lc scores")
spe3.sc <- scores(spechem.physio, choices = 1:2, scaling = 1, display = "sp")
arrows(0, 0, spe3.sc[, 1] * 0.92, spe3.sc[, 2] * 0.92, length = 0, lty = 1, col = "red")

# Scaling 2
plot(spechem.physio, display = c("sp", "lc", "cn"), main = "Triplot RDA spe.hel ~ chem | Topo - scaling 2 - lc scores")
spe4.sc <- scores(spechem.physio, choices = 1:2, display = "sp")
arrows(0, 0, spe4.sc[, 1] * 0.88, spe4.sc[, 2] * 0.88, length = 0, lty = 1, col = "red")



# ------------------------------------------------------------------------------
# Redundancy analysis (RDA):  Selection of Explanatory Variables
#  - Seach for parsimony
#  - Seach for possible strong linear dependencies (correlations) among the explanatory variables in the RDA model, which could render the regression coeffs
#   of the explanatory variables in the model unstable.
#
#
# In RDA, forward selection is the method most often applied because it works even in cases where the number of explanatory variables is larger than (n-1).
# ------------------------------------------------------------------------------
# ----------------------------------------------
# Variance inflation factors (VIF) in two RDAs
# Ideally, VIFs above 10 should be at least examined, and avoied if possible.
# But variables with high VIFs should generally not be manually removed before the application of a procedure of selection of variables,
# since both may contribute significantly.

# For a matrix X containing quantitative variables only, cif <- diag(solve(cor(X)))

# First RDA of this Chapter: all environmental variables except dfs
vif.cca(spe.rda)

# Partial RDA – physiographic variables only
vif.cca(spechem.physio) 



# ----------------------------------------------
# Forward selection of explanatory variables:  forward.sel()
# forward.sel() requires a response data matrix and an explanatory data matrix which, unfortunately, must contain quantitative variables only.
# Factors must be recoded in the form of dummy variables.

# RDA with all explanatory variables except dfs (only quantitative variables)
spe.rda.all <- rda(spe.hel ~ ., data = env2)

# Global adjusted R^2
( R2a.all <- RsquareAdj(spe.rda.all)$adj.r.squared )

# Forward selection using forward.sel()
adespatial::forward.sel(spe.hel, env2, adjR2thresh = R2a.all)



# ----------------------------------------------
# Forward selection using vegan's ordistep()
# This function allows the use of factors, and allows forward, backward and stepwise (a combination) selection.
# It can be applied to RDA, CCA and db-RDA.
# This is based on AIC-like criterion, but according to Oksanen, it may not be completely trustwarthy, and furthermore, experience shows that it tends to be very liberal.
mod0 <- rda(spe.hel ~ 1, data = env2)
step.forward <- vegan::ordistep(mod0, scope = formula(spe.rda.all), direction = "forward", permutations = how(nperm = 499))
RsquareAdj(step.forward)



# ----------------------------------------------
# Backward elimination using vegan's ordistep()
step.backward <- vegan::ordistep(spe.rda.all, permutations = how(nperm = 499))
# With redundant argument direction = "backward":
# step.backward <- vegan::ordistep(spe.rda.all, direction = "backward", permutations = how(nperm = 499)
RsquareAdj(step.backward)



# ----------------------------------------------
# Forward selection using vegan's ordiR2step() using a double stopping criterion (Blanchet et al. 2008a) and object env containing only quantitative variables.
step2.forward <- vegan::ordiR2step(mod0, scope = formula(spe.rda.all), direction = "forward", R2scope = TRUE, permutations = how(nperm = 199))
RsquareAdj(step2.forward)


# Forward selection using vegan's ordiR2step() using a double stopping criterion (Blanchet et al. 2008a) and object env3 containing a factor.
mod00 <- rda(spe.hel ~ 1, data = env3)
spe.rda2.all <- rda(spe.hel ~ ., data = env3)
step3.forward <- vegan::ordiR2step(mod00, scope = formula(spe.rda2.all), direction = "forward", permutations = how(nperm = 199))
RsquareAdj(step3.forward)


# Note that the adjusted R^2 of the complete model is smaller than that of the complete RDA with only quantitative variables.
# Some information has been lost when transforming the quantitative slo variable into a factor with 4 levels.



# ----------------------------------------------
# Partial forward selection with variable slo held constant
mod0p <- rda(spe.hel ~ Condition(slo), data = env2)
mod1p <- rda(spe.hel ~ . + Condition(slo), data = env2)
step.p.forward <- ordiR2step(mod0p,  scope = formula(mod1p), direction = "forward", permutations = how(nperm = 199))



# ----------------------------------------------
# Parsimonious RDA
(spe.rda.pars <- rda(spe.hel ~ ele + oxy + bod, data = env2))
anova(spe.rda.pars, permutations = how(nperm = 999))
anova(spe.rda.pars, permutations = how(nperm = 999), by = "axis")
(R2a.pars <- RsquareAdj(spe.rda.pars)$adj.r.squared)


# Compare the variance inflation factors
vif.cca(spe.rda.all)
vif.cca(spe.rda.pars)



# ----------------------------------------------
# Triplots of the parsimonious RDA (with fitted site scores)
# Since there is now a third significant canonical axis, you could plot other combinations, axes 1 and 3, 2 and 3
# This triplot indeed presents the same structures as the one produced with all explanatory variables.
# The sites and species show the same relationships.
par(mfrow = c(2, 1))
# Scaling 1
triplot.rda(spe.rda.pars, site.sc = "lc", scaling = 1, cex.char2 = 0.8, pos.env = 2, mult.spe = 0.9, mult.arrow = 0.92, mar.percent = 0.01)
# Scaling 2
triplot.rda(spe.rda.pars, site.sc = "lc", scaling = 2, cex.char2 = 0.8, pos.env = 2, mult.spe = 1.1, mar.percent = -0.02)




# Alternate code using plot.cca -----------------------------------
# Triplots of the parsimonious RDA (with fitted site scores)
par(mfrow = c(1, 2))
# Scaling 1
plot(spe.rda.pars, scaling = 1, display = c("sp", "lc", "cn"), main = "Triplot RDA spe.hel ~ ele+oxy+bod - scaling 1 - lc scores")
spe4.sc <- scores(spe.rda.pars, choices = 1:2, scaling = 1, display = "sp")
arrows(0, 0, spe4.sc[, 1] * 0.92, spe4.sc[, 2] * 0.92, length = 0, lty = 1, col = "red")
# Scaling 2
plot(spe.rda.pars, display = c("sp", "lc", "cn"), main = "Triplot RDA spe.hel ~ ele+oxy+bod - scaling 2 - lc scores")
spe5.sc <- scores(spe.rda.pars, choices = 1:2, display = "sp")
arrows(0, 0, spe5.sc[, 1] * 0.9, spe5.sc[, 2] * 0.9, length = 0, lty = 1, col = "red")



# ------------------------------------------------------------------------------
# Environmental reconstruction (calibration, bioindication) with RDA
#  - Projecting new sites in an RDA to estimate the values of explanatory variables
# ------------------------------------------------------------------------------
# New (fictitious) objects with fish abundances
# Variables(species) must match those in the original data set in name, number and order
# New site 1 is made from rounded means of species in sites 1 to 15
site1.new <- round(apply(spe[1:15, ], 2, mean))
# New site 2 is made from rounded means of species in sites 16 - 29
site2.new <- round(apply(spe[16:29, ], 2, mean))
( obj.new <- t(cbind(site1.new, site2.new)) )


# Hellinger transformation of the new sites
obj.new.hel <- decostand(obj.new, "hel")


# ----------------------------------------------
# Calibration
calibrate(spe.rda.pars, obj.new.hel)


# Compare with real values at sites 7 to 9 and 22 to 24: 
env2[7:9, c(1, 9, 10)]
env2[22:24, c(1, 9, 10)]



# ------------------------------------------------------------------------------
# Variation partitioning with two sets of explanatory variables
# ------------------------------------------------------------------------------
# Explanation of fraction labels (two, three and four explanatory matrices) with optional colours
par(mfrow = c(1, 3), mar = c(1, 1, 1, 1))
showvarparts(2, bg = c("red", "blue"))
showvarparts(3, bg = c("red", "blue", "yellow"))
showvarparts(4, bg = c("red", "blue", "yellow", "green"))


# ----------------------------------------------
# 1. Variation partitioning with all explanatory variables (except dfs)
( spe.part.all <- varpart(spe.hel, envchem, envtopo) )


# Plot of the partitioning results (correct values of the adjusted R squares)  (but the sizes of the circles in the Venn diagram are not to scale)
# The unique contribution of the chemical variables (0.241) is more than twice as large as that of physiography (0.112).
# The variation explained jointly by the 2 sets is also large (0.233)
# This indicates that the chemical and physiographic variables are intercorrelated.
# This is a good reason to make an effort towards parsimony, and to combine variation partitioning with forward selection.
graphics.off();  par(mfrow=c(1,1))
plot(spe.part.all, digits = 2, bg = c("red", "blue"))



# ----------------------------------------------
# 2. Variation partitioning after forward selection of explanatory variables
# Separate forward selection in each subset of environmental variables
spe.chem <- rda(spe.hel, envchem)
R2a.all.chem <- RsquareAdj(spe.chem)$adj.r.squared
forward.sel(spe.hel, envchem, adjR2thresh = R2a.all.chem, nperm = 9999)

spe.topo <- rda(spe.hel, envtopo)
R2a.all.topo <- RsquareAdj(spe.topo)$adj.r.squared
forward.sel(spe.hel, envtopo, adjR2thresh = R2a.all.topo, nperm = 9999)


# Parsimonious subsets of explanatory variables, based on forward selections
names(envchem)
envchem.pars <- envchem[, c(4, 6, 7)]
names(envtopo)
envtopo.pars <- envtopo[, c(1, 2)]


# Variation partitioning
# 
( spe.part <- varpart(spe.hel, envchem.pars, envtopo.pars) )
plot(spe.part, digits = 2, bg = c("red", "blue"), Xnames = c("Chemistry", "Physiography"), id.size = 0.7)



# ----------------------------------------------
# Tests of all testable fractions
# Test of fraction [a+b]
anova(rda(spe.hel, envchem.pars), permutations = how(nperm = 999))
# Test of fraction [b+c]
anova(rda(spe.hel, envtopo.pars), permutations = how(nperm = 999))
# Test of fraction [a+b+c]
env.pars <- cbind(envchem.pars, envtopo.pars)
anova(rda(spe.hel, env.pars), permutations = how(nperm = 999))
# Test of fraction [a]
anova(rda(spe.hel, envchem.pars, envtopo.pars), permutations = how(nperm = 999))
# Test of fraction [c]
anova(rda(spe.hel, envtopo.pars, envchem.pars), permutations = how(nperm = 999))


# ----------------------------------------------
# 3. Variation partitioning without the 'nit' variable
envchem.pars2 <- envchem[, c(6, 7)]
( spe.part2 <- varpart(spe.hel, envchem.pars2, envtopo.pars) )
plot(spe.part2, digits = 2)



# Two-way MANOVA by RDA
# Creation of a factor 'elevation' (3 levels, 9 sites each)
ele.fac <- gl(3, 9, labels = c("high", "mid", "low"))
# Creation of a factor mimicking 'pH'
pH.fac <- as.factor(c(1, 2, 3, 2, 3, 1, 3, 2, 1, 2, 1, 3, 3, 2, 1, 1, 2, 3, 2, 1, 2, 3, 2, 1, 1, 3, 3))

# Is the two-way factorial design balanced?
table(ele.fac, pH.fac)

# Creation of Helmert contrasts for the factors and the interaction
ele.pH.helm <- model.matrix(~ ele.fac * pH.fac, contrasts = list(ele.fac = "contr.helmert", pH.fac = "contr.helmert"))[, -1]
ele.pH.helm
ele.pH.helm2 <- model.matrix(~ ele.fac + pH.fac, contrasts = list(ele.fac = "contr.helmert", pH.fac = "contr.helmert"))[, -1]
colnames(ele.pH.helm2)


# Check property 1 of Helmert contrasts : all variables sum to 0
apply(ele.pH.helm, 2, sum)
# Check property 2 of Helmert contrasts: their crossproducts 
# must be 0 within and between groups (factors and interaction)
crossprod(ele.pH.helm)

# Verify multivariate homogeneity of within-group covariance
# matrices using the betadisper() function (vegan package)
# implementing Marti Anderson's testing method (Anderson 2006)

# To avoid the rist of heterogeneity of variances with respect to 
# one factor because of the dispersion in the other (in case of 
# interaction), creation of a factor crossing the two factors, i.e. 
# defining the cell-by-cell attribution of the data
cell.fac <- gl(9, 3) 

spe.hel.d1 <- dist(spe.hel[1:27, ])

# Test of homogeneity of within-cell dispersions
(spe.hel.cell.MHV <- betadisper(spe.hel.d1, cell.fac))
anova(spe.hel.cell.MHV)     # Parametric test (not recommended here)
permutest(spe.hel.cell.MHV)

# Alternatively, test homogeneity of dispersions within each factor. 
# These tests ore more robust with this small example because there are now 9 observations per group instead of 3. 
# Factor "elevation"
(spe.hel.ele.MHV <- betadisper(spe.hel.d1, ele.fac))
anova(spe.hel.ele.MHV)     # Parametric test (not recommended here)
permutest(spe.hel.ele.MHV) # Permutation test


# Factor "pH"
(spe.hel.pH.MHV <- betadisper(spe.hel.d1, pH.fac))
anova(spe.hel.pH.MHV)
permutest(spe.hel.pH.MHV) # Permutation test



# Step-by-step procedure using function rda()
# Test the interaction first. Factors ele and pH (columns 1-4)  
# are assembled to form the matrix of covariables for the test.
interaction.rda <- rda(spe.hel[1:27, ], ele.pH.helm[, 5:8], ele.pH.helm[, 1:4])
anova(interaction.rda, permutations = how(nperm = 999))


# Test the main factor ele. The factor pH and the interaction
# are assembled to form the matrix of covariables. 
factor.ele.rda <- rda(spe.hel[1:27, ], ele.pH.helm[, 1:2], ele.pH.helm[, 3:8])
anova(factor.ele.rda, permutations = how(nperm = 999), strata = pH.fac)


# Test the main factor pH. The factor ele and the interaction
# are assembled to form the matrix of covariables. 
factor.pH.rda <- rda(spe.hel[1:27, ], ele.pH.helm[, 3:4], ele.pH.helm[, c(1:2, 5:8)]) 
anova(factor.pH.rda, permutations = how(nperm = 999), strata = ele.fac)


# RDA with the significant factor ele
ele.rda.out <- rda(spe.hel[1:27, ]~ ., as.data.frame(ele.fac))
# Triplot with "wa" sites related to factor centroids, and species arrows
plot(ele.rda.out, scaling = 1, display = "wa", main = "Multivariate ANOVA, factor elevation - scaling 1 - wa scores")
ordispider(ele.rda.out, ele.fac, scaling = 1, label = TRUE, col = "blue")
spe.sc1 <- scores(ele.rda.out, scaling = 1, display = "species")
arrows(0, 0, spe.sc1[, 1] * 0.3, spe.sc1[, 2] * 0.3, length = 0.1, angle = 10, col = "red")
text(spe.sc1[, 1] * 0.3, spe.sc1[, 2] * 0.3, labels = rownames(spe.sc1), pos = 4, cex = 0.8, col = "red")


# Permutational MANOVA using adonis2()
adonis2(spe.hel[1:27, ] ~ ele.fac * pH.fac, method = "euc", by = "term")



# ------------------------------------------------------------------------------
# Nonlinear relationships in RDA
#  - RDA with a single second degree explanatory variable
#
# In Doubs data, there are several species that are found mostly in the central section of the river.
# Their relationship with the variable "distance from the source" (dfs) is therefore unimodal: absence first, then presence, then absence again.
# ------------------------------------------------------------------------------
# ----------------------------------------------
# RDA with a single second degree explanatory variable
# Create a matrix of dfs and its orthogonal second degree term using function poly()
( dfs.df <- poly(dfs, 2) )
colnames(dfs.df) <- c("dfs", "dfs2")


# Verify that the polynomial terms are orthogonal
cor(dfs.df)


# Find out if both variables are significant
forward.sel(spe.hel, dfs.df)


# RDA and test
spe.dfs.rda <- rda(spe.hel ~ ., as.data.frame(dfs.df))
anova(spe.dfs.rda)


# Triplot using "lc" (model) site scores and scaling 2 (interested primarily in the relationships among the species)
# Note that species having their optimum around mid-river will point to the opposite direction from the dfs-squared variable dfs2
# Species with arrows pointing in the same direction as the dfs2 variable may be more present at both ends of the river than in the middle.
# (no species shows this particular distribution)
# Arrows of species that are more present at one end of the river point in the direction of the dfs variable or opposite to it.
graphics.off();  par(mfrow = c(1,1));
triplot.rda(spe.dfs.rda, site.sc = "lc", scaling = 2, plot.sites = FALSE, pos.env = 1, mult.arrow = 0.9, move.origin = c(-0.25, 0), mar.percent = 0)


# If you want a triplot showing the sites in a configuration closer to the data, replace "lc" by "wa"
triplot.rda(spe.dfs.rda, site.sc = "wa", scaling = 2, plot.sites = FALSE, pos.env = 1, mult.arrow = 0.9, move.origin = c(-0.25, 0), mar.percent = 0)


# Alternate code using plot.cca
plot(spe.dfs.rda, scaling = 2, display = c("sp", "lc", "cn"), main = "Triplot RDA spe ~ dfs+dfs2 - scaling 2 - lc scores")
spe6.sc <- scores(spe.dfs.rda, choices = 1:2, scaling = 2, display = "sp")
arrows(0, 0, spe6.sc[, 1] * 0.9, spe6.sc[, 2] * 0.9, length = 0, lty = 1, col = "red")


# Maps of four fish species
# The brown trout (Satr) is most strongly linked to the upper half of the river, its vector is opposed to dfs and orthogonal to dfs2.
# Grayling (Thth) is characteristic of the central part of the river, its vector on the triplot is opposed to that of dfs2.
# The bleak (Alal) is abundant in the lower half of the river
# Finally, the tench (Titi) is present in 3 different zones along the river, which results in vector pointing halfway between the dfs and dfs2 vectors.
par(mfrow = c(2, 2))
plot(spa, asp = 1, col = "brown", cex = spe$Satr, xlab = "x (km)", ylab = "y (km)", main = "Brown trout")
lines(spa, col = "light blue")
plot(spa, asp = 1, col = "brown", cex = spe$Thth, xlab = "x (km)", ylab = "y (km)", main = "Grayling")
lines(spa, col = "light blue")
plot(spa, asp = 1, col = "brown", cex = spe$Alal, xlab = "x (km)", ylab = "y (km)", main = "Bleak")
lines(spa, col = "light blue")
plot(spa, asp = 1, col = "brown", cex = spe$Titi, xlab = "x (km)", ylab = "y (km)", main = "Tench")
lines(spa, col = "light blue")



# ----------------------------------------------
# Polynomial RDA, second degree, with forward selection among all environmental variables
# polyvars() takes only the variables and their respective (orthogonal) quadratic terms.
source("./RefData/NumericalEcologyWithR/NEwR2-Functions/polyvars.R")
env.square <- polyvars(env2, degr = 2)
names(env.square)
spe.envsq.rda <- rda(spe.hel ~ ., env.square)
R2ad <- RsquareAdj(spe.envsq.rda)$adj.r.squared
spe.envsq.fwd <- forward.sel(spe.hel, env.square, adjR2thresh = R2ad)
spe.envsq.fwd

envsquare.red <- env.square[, sort(spe.envsq.fwd$order)]
(spe.envsq.fwd.rda <- rda(spe.hel ~., envsquare.red))
RsquareAdj(spe.envsq.fwd.rda)
summary(spe.envsq.fwd.rda)



# Triplot using lc (model) site scores and scaling 2
# The polinomnial RDA is far less parsimonious, having retained 9 explanatory variables.
# But the adjusted R^2 is now 0.,7530, an important increase.
# Indeed, the linear and quadratic terms of 4 variables have been retained, those of ele, oxy, slo and amm.
# This means that among the species that respond to these variables, some of them do it in a linear way (modelled by the first-degree term) and
# others by showing a unimodal response (modelled by the second-degree term).
#
# Arrows of the tench (Titi) and squared oxygen content (oxy.2) point in opposite directions. The tench tends to be more abundant in places where oxygen concentration is intermediate.
graphics.off();  par(mfrow = c(1,1));
triplot.rda(spe.envsq.fwd.rda, site.sc = "lc", scaling = 2, plot.sites = FALSE, pos.env = 1, mult.arrow = 0.9, mult.spe = 0.9, mar.percent = 0)



# ------------------------------------------------------------------------------
# Distance-based redundancy analysis (db-RDA)
#  - square-rooted percentage difference response matrix and 'ele' factor with factor pH and interaction as covariables
# ------------------------------------------------------------------------------
# Rename columns of matrix of Helmert contrasts (for convenience)
colnames(ele.pH.helm) <- c("ele1", "ele2", "pH1", "pH2", "ele1pH1", "ele1pH2", "ele2pH1", "ele2pH2" )


# Create the matrix of covariables. MUST be of class matrix, NOT data.frame
covariables <- ele.pH.helm[, 3:8]


# Compute the dissimilarity response matrix with vegan’s vegdist()
spe.bray27 <- vegdist(spe[1:27, ], "bray")
# … or with function dist.ldc() of adespatial
# spe.bray27 <- dist.ldc(spe[1:27, ], "percentdiff")


# 1. dbrda() on the square-rooted dissimilarity matrix
bray.ele.dbrda <-  dbrda(sqrt(spe.bray27) ~ ele.pH.helm[, 1:2] + Condition(covariables))
anova(bray.ele.dbrda, permutations = how(nperm = 999)) 


# 2. capscale() with raw (site by species) data
# Rename factor (cosmetic for plot)
ele.fac. <- ele.fac
bray.env.cap <- capscale(spe[1:27, ] ~ ele.fac. + Condition(covariables), data = as.data.frame(ele.pH.helm), distance = "bray", add = "lingoes", comm = spe[1:27, ])
anova(bray.env.cap, permutations = how(nperm = 999))


# Plot with "wa" scores to see dispersion of sites around the factor levels
triplot.rda(bray.env.cap, site.sc = "wa", scaling = 1)


# The results of the two analyses are slightly different because (1) the test is not performed in the same manner and (2) the correction to make the response matrix Euclidean is not the same.



# ----------------------------------------------
# Alternative ways of computing db-RDA
# 1. capscale() with raw (site by species) data
# Alternate coding with explicit covariables coming from same object as the constraining variables : 
bray.env.capscale <- capscale(spe[1:27, ] ~ ele1 + ele2 + Condition(pH1 + pH2 + ele1pH1 + ele1pH2 + ele2pH1 + ele2pH2), data = as.data.frame(ele.pH.helm), 
                              distance = "bray", add = "cailliez", comm = spe[1:27, ])
anova(bray.env.capscale, permutations = how(nperm = 999))


# 2. PCoA with Lingoes (1971) correction:  Explicit steps
spe.bray27.lin <- pcoa(spe.bray27, correction = "lingoes") 
spe.bray27.lingoes <- spe.bray27.lin$vectors.cor 
# Test of the factor ele. Factor pH and interaction, Helmert-coded, form the matrix of covariables
spe.L.ele.dbrda <- rda(spe.bray27.lingoes, ele.pH.helm[, 1:2], covariables) 
anova(spe.L.ele.dbrda, permutations = how(nperm = 999))


# Same by staying in {vegan} and using wcmdscale() : 
spe.lingoes2 <- wcmdscale(spe.bray27, add = "lingoes") 
anova(rda(spe.lingoes2 ~ ele.pH.helm[, 1:2] + Condition(covariables)))



# ------------------------------------------------------------------------------
# The Code It Yourself
#  - The code below is another exercise in coding matrix algebra in R
# ------------------------------------------------------------------------------
myRDA <- function(Y, X)
{
  
  # ----------------------------------------------
  ## 1. Preparation of the data
  
  Y.mat <- as.matrix(Y)
  Yc <- scale(Y.mat, scale = FALSE)
  
  X.mat <- as.matrix(X)
  Xcr <- scale(X.mat)
  
  # Dimensions
  n <- nrow(Y)
  p <- ncol(Y)
  m <- ncol(X)
  
  # ----------------------------------------------
  ## 2. Computation of the multivariate linear regression
  
  # Matrix of regression coefficients (eq. 11.9)
  B <- solve(t(Xcr) %*% Xcr) %*% t(Xcr) %*% Yc
  
  # Matrix of fitted values (eq. 11.10)
  Yhat <- Xcr %*% B
  
  # Matrix of residuals
  Yres <- Yc - Yhat
  
  
  # ----------------------------------------------
  ## 3. PCA on fitted values
  
  # Covariance matrix (eq. 11.12)
  S <- cov(Yhat)
  
  # Eigenvalue decomposition
  eigenS <- eigen(S)
  
  # How many canonical axes?
  kc <- length(which(eigenS$values > 0.00000001))
  
  # Eigenvalues of canonical axes
  ev <- eigenS$values[1 : kc]
  # Total variance (inertia) of the centred matrix Yc
  trace = sum(diag(cov(Yc)))
  
  # Orthonormal eigenvectors (contributions of response 
  # variables, scaling 1)
  U <- eigenS$vectors[, 1 : kc]
  row.names(U) <- colnames(Y)
  
  # Site scores (vegan's wa scores, scaling 1; eq.11.17)
  F <- Yc %*% U
  row.names(F) <- row.names(Y)
  
  # Site constraints (vegan's 'lc' scores, scaling 1; 
  # eq. 11.18)
  Z <- Yhat %*% U
  row.names(Z) <- row.names(Y)
  
  # Canonical coefficients (eq. 11.19)
  CC <- B %*% U
  row.names(CC) <- colnames(X)
  
  # Explanatory variables
  # Species-environment correlations
  corXZ <- cor(X, Z)
  
  # Diagonal matrix of weights
  D <- diag(sqrt(ev / trace))
  
  # Biplot scores of explanatory variables
  coordX <- corXZ %*% D    # Scaling 1
  coordX2 <- corXZ         # Scaling 2
  row.names(coordX) <- colnames(X)
  row.names(coordX2) <- colnames(X)
  
  # Scaling to sqrt of the relative eigenvalue
  # (for scaling 2)
  U2 <- U %*% diag(sqrt(ev))
  row.names(U2) <- colnames(Y)
  F2 <- F %*% diag(1/sqrt(ev))
  row.names(F2) <- row.names(Y)
  Z2 <- Z %*% diag(1/sqrt(ev))
  row.names(Z2) <- row.names(Y)
  
  # Unadjusted R2
  R2 <- sum(ev/trace)
  # Adjusted R2
  R2a <- 1 - ((n - 1)/(n - m - 1)) * (1 - R2)
  
  
  # ----------------------------------------------
  # 4. PCA on residuals
  # Write your own code as in Chapter 5. It could begin with : 
  #     eigenSres <- eigen(cov(Yres))
  #     evr <- eigenSres$values
  
  
  # ----------------------------------------------
  # 5. Output
  
  result <- 
    list(trace, R2, R2a, ev, CC, U, F, Z, coordX, 
         U2, F2, Z2, coordX2)
  names(result) <- 
    c("Total_variance", "R2", "R2adj", "Can_ev", 
      "Can_coeff", "Species_sc1", "wa_sc1", "lc_sc1", 
      "Biplot_sc1", "Species_sc2", "wa_sc2", "lc_sc2", 
      "Biplot_sc2") 
  
  result
}


# Apply homemade function to the Doubs fish and environmental data
doubs.myRDA <- myRDA(spe.hel, env2)
summary(doubs.myRDA)


# Retrieve adjusted R-square
doubs.myRDA$R2adj



