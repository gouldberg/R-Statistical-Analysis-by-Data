# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
rm(list=ls())
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "vegan", "cocorresp", "ade4")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  aravo
#  - The data come from 75, 5 * 5 m plots located in the South-Western Alps at about 2700 m elevation
#  - Consists in the 3 matrices
#     - community composition:  82 species, abundance scale from 0 to 5
#     - traits:  8 quantitative variables
#     - environment:  4 quantitative and 2 categorical variables
# ------------------------------------------------------------------------------
# Extract the data (available in ade4)
data(aravo)
dim(aravo$spe)
dim(aravo$traits)
dim(aravo$env)


car::some(aravo$spe)
car::some(aravo$traits)
car::some(aravo$env)



# ------------------------------------------------------------------------------
# RLQ and fourth-corner analyses:  Relating species traits and environment
#  - Functional ecology is the discipline devoted to the study of the relationship between species traits and the environment.
#    This type of study poses methodological challenges to researchers because to test hypotheses in this context, one must evaluate the relationships between species traits
#    and environmental characteristics, as mediated by the species presence-absence or abundance data.
#
#  - RLQ method:  ordination method allowint the visualization of the joint structure resulting from the 3 data tables, but with a single global test
#  - fourth-corner method:  consists in a series of statistical tests of individual trait-environment relationships, without consideration of the covariation among traits or environmental variables
#      and no output about the sites and the species.
#
#  - Fourth-corner method's permutation models:  Model 1 to 6
#      - Model 1:  environmental control over individual species
#                  Permute the data within each column of matrix L (or A) independently, and this destroys the links between L and Q (A and B) as well as between L and R (or A and C)
#                  The H0 stats that individuals of a species are randomly distributed with respect the site conditions.
#      ...
# ------------------------------------------------------------------------------
# Preliminary analyses: CA on the species data, PCA(Hill-Smith analysis, handling quantitative and categorical) on environment data and PCA of traits data.
( afcL.aravo <- dudi.coa(aravo$spe, scannf = FALSE) )
( acpR.aravo <- dudi.hillsmith(aravo$env, row.w = afcL.aravo$lw,scannf = FALSE) )
( acpQ.aravo <- dudi.pca(aravo$traits, row.w = afcL.aravo$cw, scannf = FALSE) )



# RLQ analysis:  compute separate ordinations of the 3 data set.
rlq.aravo <- rlq(dudiR = acpR.aravo, dudiL = afcL.aravo, dudiQ = acpQ.aravo, scannf = FALSE)


# compute on the basis of the 3 ordinations
graphics.off();  par(mfrow=c(1,1));
plot(rlq.aravo)


# Traits by environment crossed table
rlq.aravo$tab


# Global test  --> model 6 (combination of model 2 and 4) yielded a combined p-value = 0.001
# hence the null hypothesis is rejected, withi means that both links L - Q and R - L are significant.
randtest(rlq.aravo, nrepet = 999, modeltype = 6)


# Since the plots are crowded, one can plot them one by one in large graphical windows.
# Site(L) scores, Species(Q) scores, Environmental variables, Species traits.
graphics.off();  par(mfrow=c(2,2));
s.label(rlq.aravo$lR, boxes = FALSE, sub = "a", possub = "topleft")
s.label(rlq.aravo$lQ, boxes = FALSE, sub = "b", possub = "topleft")
s.arrow(rlq.aravo$l1, sub = "c", possub = "topleft")
s.arrow(rlq.aravo$c1, sub = "d", possub = "topleft")


# Left part (negative) of "b" (Species(Q) scores) :
#   - identified species (Poa supina, Alchemilla pentaphyllea, or Taraxacum alpinum)
# "d" (Species traits):
#   - with higher specific leaf area (SLA) and mass-based leaf nitrogen content (NMass), lower height, and a reduced seed mass
# "a" (Site(L) scores) and "c" (Environmental variables)
#   - these species were mostly found in late-melting habitas

# "d" (Species trains):
#   - The right part of the axis highlights traint attributes (upright and thick leaves) associated with convex landforms, physically disturbed and mostly early-melting sites.
# "b" (Species(Q) scores):
#   - Corresponding species are Sempervivum, montanum, Androsace adfinis, or Lloydia serotina.

# "c" (Environmental variables):
#   - Second axis outlined zoogenic disturbed sites located in concave slopes.
# "d" (Species traits):
#   - These habitats were characterized by large leaved species
# "b" (Species(Q) scores):
#   - Corresponding species are Cirsium acaule, Geum montanum, Alchemilla vulgaris



# Fourth-corner analysis (takes time with 49999 permutations!):  TAKES TIME !!!  (around 5 min)
# modeltype = 6 based on global test
fourth.aravo <- ade4::fourthcorner(tabR = aravo$env, tabL = aravo$spe, tabQ = aravo$traits, modeltype = 6, p.adjust.method.G = "none",  p.adjust.method.D = "none",  nrepet = 49999)
fourth.aravo


# Correction for multiple testing, here using FDR
fourth.aravo.adj <- ade4::p.adjust.4thcorner(fourth.aravo, p.adjust.method.G = "fdr",  p.adjust.method.D = "fdr",  p.adjust.D = "global") 


# Plot significant associations
# At the alpha = 0.05 level, significatn positive associations are represented by red cells and negative ones by blue cells
# SLA (specific leaf area) and N_mass (mass-based leaf nitrogen content) are positively associated with Snow (mean snow melt date) and Form.5 (concave microtopography)
# These traits are likely to favour species that tolerate a longer period of snow cover
#  - a higher nitrogen content, partly due to nitrogen storage in snowpacks and partly to the protective effect of snow on soil temperature and water content,
#   warrants larger reserves, and a larger leaf area allows a larger rate of photosynthesis once the plant is eventually exposed to the sun.
# Conversely, these two traits are negatively associated with PhysD (physical disturbance due to cryoturbation), which tends to occur more often in areas without snow and
#  therefore more exposed to large temperature oscillations.
par(mfrow=c(1,1))
plot(fourth.aravo.adj, alpha = 0.05, stat = "D2")



# Biplot combining RLQ and fourth-corner results
# blue lines:  negative associations
# red lines: positive associations
# Snow and Form.5 show up clearly as a tight group of associations that take place in concave-up sites where snow takes time to melt.
plot(fourth.aravo.adj, x.rlq = rlq.aravo, alpha = 0.05, stat = "D2", type = "biplot")


# ------------------------------------------------------------------------------
# data:  Doubs
# RLQ and fourth-corner analyses (Doubs data)
# ------------------------------------------------------------------------------
load("./RefData/NumericalEcologyWithR/NEwR2-Data/Doubs.RData")

car::some(fishtraits)

summary(fishtraits)
rownames(fishtraits)
names(spe)
names(fishtraits)
tra <- fishtraits[ , 6:15]
tra


# Remove the 'dfs' variable from the env data frame
env2 <- env[, -1]

# Recode the slope variable (slo) into a factor (qualitative) variable to show how these are handled in the ordinations
slo2 <- rep(".very_steep", nrow(env))
slo2[env$slo <= quantile(env$slo)[4]] <- ".steep"
slo2[env$slo <= quantile(env$slo)[3]] <- ".moderate"
slo2[env$slo <= quantile(env$slo)[2]] <- ".low"
slo2 <- factor(slo2, levels = c(".low", ".moderate", ".steep", ".very_steep"))
table(slo2)

# Create an env3 data frame with slope as a qualitative variable
env3 <- env2
env3$slo <- slo2


# ----------------------------------------------------
# Preliminary analyses: CA, Hill-Smith and PCA
afcL.doubs <- dudi.coa(spe, scannf = FALSE)
acpR.doubs <- dudi.hillsmith(env3, row.w = afcL.doubs$lw, scannf = FALSE)
acpQ.doubs <- dudi.pca(tra, row.w = afcL.doubs$cw, scannf = FALSE)


# RLQ analysis
rlq.doubs <- rlq(dudiR = acpR.doubs, dudiL = afcL.doubs, dudiQ = acpQ.doubs, scannf = FALSE)
plot(rlq.doubs)


# Traits by environment crossed table
rlq.doubs$tab


# Global test  --> model 6 (combination of model 2 and 4) yielded a combined p-value = 0.001
# hence the null hypothesis is rejected, withi means that both links L - Q and R - L are significant.
randtest(rlq.doubs, nrepet = 999, modeltype = 6)


# Since the plots are crowded, one can plot them one by one in large graphical windows.
# Site(L) scores, Species(Q) scores, Environmental variables, Species traits.
graphics.off();  par(mfrow=c(2,2));
s.label(rlq.doubs$lR, boxes = FALSE, sub = "a", possub = "topleft")
s.label(rlq.doubs$lQ, boxes = FALSE, sub = "b", possub = "topleft")
s.arrow(rlq.doubs$l1, sub = "c", possub = "topleft")
s.arrow(rlq.doubs$c1, sub = "d", possub = "topleft")


# Fourth-corner analysis (takes time with 49999 permutations!)
?fourthcorner

fourth.doubs2 <- fourthcorner(tabR = env3, tabL = spe, tabQ = tra, modeltype = 2, p.adjust.method.G = "fdr", p.adjust.method.D = "fdr", nrepet = 49999)
fourth.doubs2
summary(fourth.doubs2)


fourth.doubs <- fourthcorner(tabR = env2, tabL = spe, tabQ = tra, modeltype = 6, p.adjust.method.G = "none",  p.adjust.method.D = "none", nrepet = 49999)

# Correction for multiple testing, here using FDR
fourth.doubs.adj <- p.adjust.4thcorner(fourth.doubs, p.adjust.method.G = "fdr",  p.adjust.method.D = "fdr", p.adjust.D = "global") 

fourth.doubs.adj
summary(fourth.doubs.adj)

# Plot
par(mfrow=c(2,2))
plot(fourth.doubs.adj, alpha = 0.05, stat = "D2")
plot(fourth.doubs2, stat = "D2")
plot(fourth.doubs2, stat = "G")


# Biplot combining RLQ and fourth-corner results
par(mfrow=c(1,2))
plot(fourth.doubs.adj, x.rlq = rlq.doubs, alpha = 0.05, stat = "D2", type = "biplot")
plot(fourth.doubs2, x.rlq = rlq.doubs, alpha = 0.05, stat = "D2", type = "biplot")
