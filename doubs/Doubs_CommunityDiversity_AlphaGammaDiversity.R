# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
rm(list=ls())
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "vegan", "vegetarian", "ggplot2", "adespatial", "ade4", "FD", "taxize")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  Doubs
# ------------------------------------------------------------------------------
load("./RefData/NumericalEcologyWithR/NEwR2-Data/Doubs.RData")


# Remove empty site 8
spe <- spe[-8, ]
env <- env[-8, ]
spa <- spa[-8, ]



# ------------------------------------------------------------------------------
# Source additional function
# ------------------------------------------------------------------------------
source("./RefData/NumericalEcologyWithR/NEwR2-Functions/panelutils.R")
source("./RefData/NumericalEcologyWithR/NEwR2-Functions/Rao.R")



# ------------------------------------------------------------------------------
# Alpha species diversity
#  - Compute alpha diversity indices of the fish communities
# ------------------------------------------------------------------------------
# number of species by site (= N0)
( q <- rowSums(spe > 0) )


# total abundance of all species by site
( N <- apply(spe, 1, sum) )


# the relative frequency by site
p <- data.frame()
for(i in 1:nrow(spe)){
  p0 <- spe[i,] / N[i]
  p <- rbind(p, p0)
}
colnames(p) <- colnames(spe)


# --------------------------------------------------
# Renyi's entropies and the corresponding diversity numbers:  a = 0

# Diversity: Species richness (number of species)
N0 <- rowSums(spe > 0)
( N0 <- vegan::specnumber(spe) )


# Entropy
( H0 <- log(N0, base = 2) )



# --------------------------------------------------
# Renyi's entropies and the corresponding diversity numbers:  a = 1

# Entropy: Shannon entropy (Hill numbers, base e and 2)
# Computed on the basis of the relative frequencies of the states (species) using the Shannon equation
# For any number of individuals, H takes its maximum when all species are represented by equal abundances
( H1 <- vegan::diversity(spe) )
( H1b2 <- vegan::diversity(spe, base = 2) )


# Diversity: Shannon diversity (base e and 2)
( N1 <- exp(H1) )
( N1b2 <- 2^Hb2 )



# --------------------------------------------------
# Renyi's entropies and the corresponding diversity numbers:  a = 2

# Entropy
( H2 <- - log(apply(p^2, 1, sum)) )


# Diversity: Simpson diversity
( N2 <- diversity(spe, "inv") )


( gamma <- apply(p^2, 1, sum) )
N2 <- 1 / gamma



# --------------------------------------------------
# Evenness

# Pielou evenness
# Despite its poor performance, due to its plong recognized strong dependence on species richness, Pielou's evenness is still the most widely used evenness index in the ecological literature.
( Hmax <- log(N0) )
( J <- H1 / Hmax )


# Shannon's evenness (Hill's ratio)
( E10 <- N1 / N0 )


# Simpson's evenness (Hill's ratio) 
( E20 <- N2 / N0 )



# --------------------------------------------------
( div <- data.frame(N0, H1, Hb2, N1, N1b2, N2, E10, E20, J) )


# Correlations among diversity indices
cor(div)


# Contrary to Hill's ratios, Pielou evenness is biased because it is systematically positively correlated with species richness.
pairs(div[-1, ], lower.panel = panel.smooth, upper.panel = panel.cor, diag.panel = panel.hist, main = "Pearson Correlation Matrix")



# ------------------------------------------------------------------------------
# Alpha species diversity
#  - Examine the site with max and min diversity
# ------------------------------------------------------------------------------
# Renyi's entropies and the corresponding diversity numbers:  a = 0
sort(N0)


# Site 29 is maximum in diversity and Site 1 is maximum in diversity
graphics.off();  par(mfrow=c(2,1));
barplot(unlist(spe[which.max(N0),]))
barplot(unlist(spe[which.min(N0),]))



# --------------------------------------------------
# Compare diversities and evenness based on Renyi's generalization a = 0,1,2
( div_a0 <- sort(N0) )
( div_a1 <- sort(N1) )
( div_a2 <- sort(N2) )

( sort(J) )
( sort(E10) )
( sort(E20) )



# --------------------------------------------------
# The diversity of site 1 is lowest (=1) since it has only 1 species, but evenness is largest
graphics.off();  par(mfrow=c(1,1));
barplot(unlist(spe["1",]))


# Site 2 and site 23 has same number of spieces (a = 0), but in a = 1,2, the diversity of site 2 is larger than that of site 23
# Also the evenness of site 2 is larger than that of site 23
# The diversity of site 2 is low (2nd or 3rd), but the evenness is second largest followed by site 1
graphics.off();  par(mfrow=c(2,1));
barplot(unlist(spe["23",]))
barplot(unlist(spe["2",]))



# --------------------------------------------------
# Plot distribution of abundances by diversity increasing order:  a = 0
graphics.off();  par(mfrow=c(4,4));
for(i in 1:16){
  name_site <- names(div_a0[i])
  div <- unname(div_a0[i])
  abN <- rowSums(spe[name_site,] > 0)
  ave <- round(rowSums(spe[name_site,]) / abN, digits = 2)
  barplot(sort(unlist(spe[name_site,]), decreasing=T), main = paste0("Site ", name_site, "  Div:", round(div, digits=2), "  AbN:", abN, "  ave:", ave), ylim = c(0,10), las = 1)
}

for(i in 17:27){
  name_site <- names(div_a0[i])
  div <- unname(div_a0[i])
  abN <- rowSums(spe[name_site,] > 0)
  ave <- round(rowSums(spe[name_site,]) / abN, digits = 2)
  barplot(sort(unlist(spe[name_site,]), decreasing=T), main = paste0("Site ", name_site, "  Div:", round(div, digits=2), "  AbN:", abN, "  ave:", ave), ylim = c(0,10), las = 1)
}



# Plot distribution of abundances by diversity increasing order:  a = 1
# Site 20 has same number of abundance (22) as Site 28 and average is lower than that of Site 28, but larger diversity (19.93 vs. 19.81)
graphics.off();  par(mfrow=c(4,4));
for(i in 1:16){
  name_site <- names(div_a1[i])
  div <- unname(div_a1[i])
  abN <- rowSums(spe[name_site,] > 0)
  ave <- round(rowSums(spe[name_site,]) / abN, digits = 2)
  barplot(sort(unlist(spe[name_site,]), decreasing=T), main = paste0("Site ", name_site, "  Div:", round(div, digits=2), "  AbN:", abN, "  ave:", ave), ylim = c(0,10), las = 1)
}

for(i in 17:27){
  name_site <- names(div_a1[i])
  div <- unname(div_a1[i])
  abN <- rowSums(spe[name_site,] > 0)
  ave <- round(rowSums(spe[name_site,]) / abN, digits = 2)
  barplot(sort(unlist(spe[name_site,]), decreasing=T), main = paste0("Site ", name_site, "  Div:", round(div, digits=2), "  AbN:", abN, "  ave:", ave), ylim = c(0,10), las = 1)
}


# Plot distribution of abundances by diversity increasing order:  a = 2
# This time, Site 20 has lower diversity than that of Site 28 (18.23 vs. 18.56)
graphics.off();  par(mfrow=c(4,4));
for(i in 1:16){
  name_site <- names(div_a2[i])
  div <- unname(div_a2[i])
  abN <- rowSums(spe[name_site,] > 0)
  ave <- round(rowSums(spe[name_site,]) / abN, digits = 2)
  barplot(sort(unlist(spe[name_site,]), decreasing=T), main = paste0("Site ", name_site, "  Div:", round(div, digits=2), "  AbN:", abN, "  ave:", ave), ylim = c(0,10), las = 1)
}

for(i in 17:27){
  name_site <- names(div_a2[i])
  div <- unname(div_a2[i])
  abN <- rowSums(spe[name_site,] > 0)
  ave <- round(rowSums(spe[name_site,]) / abN, digits = 2)
  barplot(sort(unlist(spe[name_site,]), decreasing=T), main = paste0("Site ", name_site, "  Div:", round(div, digits=2), "  AbN:", abN, "  ave:", ave), ylim = c(0,10), las = 1)
}



# ------------------------------------------------------------------------------
# Beta diversity:  Measured by a single number
# ------------------------------------------------------------------------------
# Gamma richness and expected species pool
( gobs <- ncol(spe) )
( gthe <- vegan::specpool(spe) )



# --------------------------------------------------
# Multiplicative partitioning of Hill numbers (Jost 2006, 2007)

# Mean alpha species richness
vegetarian::d(spe, lev = "alpha", q = 0)


# Mean alpha Shannon diversity
vegetarian::d(spe, lev = "alpha", q = 1)


# Mean alpha Simpson diversity
vegetarian::d(spe, lev = "alpha", q = 2, boot = TRUE)



# --------------------------------------------------
# Multiplicative beta species richness
vegetarian::d(spe, lev = "beta", q = 0)


# Multiplicative beta Shannon diversity
vegetarian::d(spe, lev = "beta", q = 1)


# Multiplicative beta Simpson diversity
vegetarian::d(spe, lev = "beta", q = 2, boot = TRUE)



# --------------------------------------------------
# Gamma species richness
vegetarian::d(spe, lev = "gamma", q = 0)


# Gamma Shannon diversity
vegetarian::d(spe, lev = "gamma", q = 1)


# Gamma Simpson diversity
vegetarian::d(spe, lev = "gamma", q = 2, boot = TRUE)



# --------------------------------------------------
# Plot multiplicative beta diversity vs order
# Increases from about 2 to 7 when increasing the order , i.e., when giving more and more importance to evenness component against the richness component of species diversity.
mbeta <- data.frame(order = 0:20, beta = NA, se = NA)
for (i in 1:nrow(mbeta)) {
  out <- d(spe, lev = "beta", q = mbeta$order[i], boot = TRUE)
  mbeta$beta[i] <- out$D.Value
  mbeta$se[i] <- out$StdErr
}
mbeta 
ggplot(mbeta, aes(order, beta)) + geom_point() + geom_line() +
  geom_errorbar(aes(order, beta, ymin = beta - se, ymax = beta + se), width = 0.2) + labs(y = "Multiplicative beta diversity", x = "Order of the diversity measure")



# --------------------------------------------------
# MacArthur's homogeneity measure (MacArthur 1965)
hom <- data.frame(order = 0:20, homogeneity = NA, se = NA)
for (i in 1:nrow(hom)) {
  out <- vegetarian::M.homog(spe, q = hom$order[i], std = TRUE, boot = TRUE)
  hom$homogeneity[i] <- out$M
  hom$se[i] <- out$StdErr
}

hom

ggplot(hom, aes(order, homogeneity)) + geom_point() + geom_line() +
  geom_errorbar(aes(order, homogeneity, ymin = homogeneity - se, ymax = homogeneity + se), width = 0.2) + labs(y = "MacArthur's homogeneity measure", x = "Order of the diversity measure")



# ------------------------------------------------------------------------------
# Beta diversity as the variance of the community composition table
# SCBD (Species Contributions to Beta Diversity) and LCBD (Local Contributions to Beta Diversity) indices 
# ------------------------------------------------------------------------------
# Computation using beta.div {adespatial} on Hellinger-transformed species data
( spe.beta <- adespatial::beta.div(spe, method = "hellinger", nperm = 9999) )
summary(spe.beta)


# SSTotal and BDTotal
# These values differ slightly from those of legendre and DeCaceres (2013) because we used a different distance coefficient.
spe.beta$beta


# 5 species have an SCBD higher than the mean SCBD
#  - the brown trout (Satr),  Eurasian minnow (Phph),  bleak (Alal),  stone loach (Babl),  and (to a lesser extent) roach (Ruru)
spe.beta$SCBD[spe.beta$SCBD >= mean(spe.beta$SCBD)]


# Plot of the species with large SCBD
graphics.off(); par(mfrow = c(2,3));
plot(spa, asp = 1, cex.axis = 0.8, col = "brown", cex = spe$Satr, main = "Brown trout", xlab = "x coordinate (km)", ylab = "y coordinate (km)")
lines(spa, col = "light blue")
plot(spa, asp = 1, cex.axis = 0.8, col = "brown", cex = spe$Phph, main = "Eurasian minnow", xlab = "x coordinate (km)", ylab = "y coordinate (km)")
lines(spa, col = "light blue")
plot(spa, asp = 1, cex.axis = 0.8, col = "brown", cex = spe$Babl, main = "Stone loach", xlab = "x coordinate (km)", ylab = "y coordinate (km)")
lines(spa, col = "light blue")
plot(spa, asp = 1, cex.axis = 0.8, col = "brown", cex = spe$Ruru, main = "Roach", xlab = "x coordinate (km)", ylab = "y coordinate (km)")
lines(spa, col = "light blue")
plot(spa, asp = 1, cex.axis = 0.8, col = "brown", cex = spe$Alal, main = "Bleak", xlab = "x coordinate (km)", ylab = "y coordinate (km)")
lines(spa, col = "light blue")



# --------------------------------------------------
# LCBD values
spe.beta$LCBD


# p-values
spe.beta$p.LCBD


# Holm correction
p.adjust(spe.beta$p.LCBD, "holm")


# Sites with significant Holm-corrected LCBD value
# The permutation tests show that the LCBD values of sites 1 and 23 are significant after a Holm correction for 29 simultaneous tests.
# Both sites stand out because they harbour very few species, which makes them different from most of the other sites.
# Any departure from the overall species abundance pattern increases the LCBD value.
#  - Site 1:  a pristine site at the ehad of the river, harbouring a single species (the brown trout (Satr))
#  - Site 23-25 suffered from urban pollution and were in need of rehabilitation
row.names(spe[which(p.adjust(spe.beta$p.LCBD, "holm") <= 0.05),])


# Plot the LCBD values on the river map
par(mfrow=c(1,1))
plot(spa, asp = 1, cex.axis = 0.8, pch = 21, col = "white", bg = "brown", cex = spe.beta$LCBD * 70, main = "LCBD values", xlab = "x coordinate (km)", ylab = "y coordinate (km)")
lines(spa, col = "light blue")
text(85, 11, "***", cex = 1.2, col = "red")
text(80, 92, "***", cex = 1.2, col = "red")



# ------------------------------------------------------------------------------
# Beta diversity:  Partitioning beta diversity into replacement, richness difference and nestedness components
#  - Species replacement
#      - what is observed along ecological gradients where some species are found on a limited range of the gradient according to their ecological optima and tolerance.
#        Some species are observed at one end of the gradient, then disappear and are replaced by others, and so on.
#        The causes of these patterns include environmental forcing, but also, potentially, competition and histrical events.
#  - Richness difference
#      - caused by local species disappearances, differences in the local abiotic conditions leading to different numbers of ecological niches,
#        or other ecological processes leading to communities with higher or lower numbers of species
#  - Nestedness:
#      - a special case of richness difference where the species at poorer sites are a strict subset of the species present at richer sites.
# ------------------------------------------------------------------------------
# Jaccard-based Podani indices (presence-absence data)
( fish.pod.j <- adespatial::beta.div.comp(spe, coef = "J", quant = FALSE) )


# Display summary statistics
#  - BDtotal = Repl + RichDif
#  - repl:  total replacement diversity
#  - RichDif:  total richness diversity (or nestedness)
fish.pod.j$part

# --> the total richness diversity accounts for the larger proportion (71.6%) of BDtotal



# --------------------------------------------------
# Extraction of the richness difference matrix
( fish.rich <- as.matrix(fish.pod.j$rich) )


# Plot of the richness difference with respect to site 30
#  - the last site (site 30) acts as a reference for all plots since the fish necessarily colonized the river from its lower part.
fish.rich.30 <- fish.rich[29, ][-29]
site.names <- seq(1, 29)[-8]
plot(site.names, fish.rich.30, type = "n", xaxp = c(1, 29, 28), main = "Doubs fish data: richness difference with respect to site 30",  xlab = "Site number", ylab = "Richness difference")
lines(site.names, fish.rich.30, pch = 24, col = "red")
points(site.names, fish.rich.30, pch = 24, cex = 1.5, col = "white", bg = "red")
text(3, 0.85, "Upstream", cex = 1, col = "red")
text(27, 0.85, "Downstream", cex = 1, col = "red")


# We study Podani's Jaccard-based richness difference.
# Therefore, if the various species did reach different points along the river, richness difference is expected to increase upstream from site 30.
# The plot shows that upstream from site 30 there is littele richness difference up to site 17, with the notable exception of sites 23, 24, and 25.
# In the mid 20th century, these 3 sites were heavily polutted (urban pollution).
# Upstream from site 17, richness difference steadily increases to site 9, the drops at sites 6,5,4, then increases again up to site 1, where it is the highest.



# --------------------------------------------------
# Extraction of the replacement matrix
fish.repl <- as.matrix(fish.pod.j$repl)


# Extraction of the Jaccard dissimilarity Dj matrix
fish.jac <- as.matrix(fish.pod.j$D)


# Plot of the Jaccard, replacement and richness difference indices between nearest neighbours
# Contrary to Legendre's example, however, we will plot the values between neigbouring sites, not with respect to site 30, to emphasize local (pairwise) features.
# Jaccard dissimilarity D = (1 - S7), at each pair of sites Repl + RichDif = D

# First, extract the subdiagonals of the square dissimilarity matrices
fish.repl.neigh <- diag(fish.repl[-1, ]) # Replacement
fish.rich.neigh <- diag(fish.rich[-1, ]) # Richness difference
fish.jac.neigh <- diag(fish.jac[-1, ]) # Jaccard Dj index

absc <- c(2:7, 9:30) # Abscissa
label.pairs <- c("1-2", "2-3", "3-4", "4-5", "5-6", "6-7", " ", "7-9", "9-10", "10-11", "11-12", "12-13", "13-14", "14-15", 
                 "15-16", "16-17", "17-18", "18-19", "19-20", "20-21", "21-22", "22-23", "23-24", "24-25", "25-26", "26-27", "27-28", "28-29", "29-30")
plot(absc, fish.jac.neigh, type = "n", xaxt = "n", main = "Replacement - Richness difference - Jaccard - nearest neighbours",  xlab = "Site pairs", ylab = "Podani's indices")
axis(side = 1, 2:30, labels = label.pairs, las = 2, cex.axis = 0.9)
lines(absc, fish.jac.neigh, col = "black")
points(absc, fish.jac.neigh, pch = 21, cex = 2, col = "black", bg = "black")
lines(absc, fish.repl.neigh, col = "blue")
points(absc, fish.repl.neigh, pch = 22, cex = 2, col = "white", bg = "blue")
lines(absc, fish.rich.neigh, col = "red")
points(absc, fish.rich.neigh, pch = 24, cex = 2, col = "white", bg = "red")
legend("top", c("Jaccard D", "Replacement", "Richness difference"), pch = c(16, 15, 17), col = c("black", "blue", "red"))


# --> One can observe that from site 1 to 7, the dissimilarities are due exclusively to richness differences.
# Species replacement occurs mainly between neighbouring sites in the intermediate section of the river.
# Large richness difference between sites 22-23, 23-24, and 25-26, due to the drop in species richness in sites 23,24, and 25.
# Interestingly, in all but 3 pairs of sites (9-10, 14-15 and 15-16), either richness difference or replacement accounts for the whole dissimilarity value.



# --------------------------------------------------
# Triangular plots
# Jaccard
fish.pod.J <- adespatial::beta.div.comp(spe, coef = "J", quant = FALSE)


# Sorensen
fish.pod.S <- adespatial::beta.div.comp(spe, coef = "S", quant = FALSE)


# Ruzicka
fish.pod.qJ <- adespatial::beta.div.comp(spe, coef = "J", quant = TRUE)


# Percentage difference
fish.pod.qS <- adespatial::beta.div.comp(spe, coef = "S", quant = TRUE)


# Data frames for the triangular plots
fish.pod.J.3 <- cbind((1 - fish.pod.J$D), fish.pod.J$repl, fish.pod.J$rich)
colnames(fish.pod.J.3) <- c("Similarity", "Repl", "RichDiff")
fish.pod.S.3 <- cbind((1 - fish.pod.S$D), fish.pod.S$repl, fish.pod.S$rich)
colnames(fish.pod.S.3) <- c("Similarity", "Repl", "RichDiff")
fish.pod.qJ.3 <- cbind((1 - fish.pod.qJ$D), fish.pod.qJ$repl, fish.pod.qJ$rich)
colnames(fish.pod.qJ.3) <- c("Similarity", "Repl", "AbDiff")
fish.pod.qS.3 <- cbind((1 - fish.pod.qS$D), fish.pod.qS$repl, fish.pod.qS$rich)
colnames(fish.pod.qS.3) <- c("Similarity", "Repl", "AbDiff")



# Triangular plots of all pairs of sites, based on the Podani indices of richness or abundance difference, replacement, and corresponding "similarities".
# The mean values of the indices, as well as the position of a "mean pair of sites", are represented by larger black dots.
par(mfrow = c(2, 2))
ade4::triangle.plot(as.data.frame(fish.pod.J.3[, c(3, 1, 2)]), show = FALSE, labeltriangle = FALSE, addmean = TRUE)
text(-0.45, 0.5, "RichDiff", cex = 1.5)
text(0.4, 0.5, "Repl", cex = 1.5)
text(0, -0.6, "Jaccard similarity", cex = 1.5)
ade4::triangle.plot(as.data.frame(fish.pod.S.3[, c(3, 1, 2)]), show = FALSE, labeltriangle = FALSE, addmean = TRUE)
text(-0.45, 0.5, "RichDiff", cex = 1.5)
text(0.4, 0.5, "Repl", cex = 1.5)
text(0, -0.6, "Sørensen similarity", cex = 1.5)

ade4::triangle.plot(as.data.frame(fish.pod.qJ.3[, c(3, 1, 2)]), show = FALSE, labeltriangle = FALSE, addmean = TRUE)
text(-0.45, 0.5, "AbDiff", cex = 1.5)
text(0.4, 0.5, "Repl", cex = 1.5)
text(0, -0.6, "S = 1 – Ružička D", cex = 1.5)
ade4::triangle.plot(as.data.frame(fish.pod.qS.3[, c(3, 1, 2)]), show = FALSE, labeltriangle = FALSE, addmean = TRUE)
text(-0.45, 0.5, "AbDiff", cex = 1.5)
text(0.4, 0.5, "Repl", cex = 1.5)
text(0, -0.6, "S = 1 – Percentage difference", cex = 1.5)



# Display values of the mean points in the triangular plots
colMeans(fish.pod.J.3[, c(3, 1, 2)])
colMeans(fish.pod.S.3[, c(3, 1, 2)])
colMeans(fish.pod.qJ.3[, c(3, 1, 2)])
colMeans(fish.pod.qS.3[, c(3, 1, 2)])



# ------------------------------------------------------------------------------
# Beta diversity:  Partitioning beta diversity into replacement, richness difference and nestedness components
# Explaining replacement and richness difference by means of db-RDA
#  - Is there a link between replacement or richness difference and the environmental variables ?
# ------------------------------------------------------------------------------
# Canonical ordination:  replacement and richness difference matrices are submitted together with the environmental variables to distance-based RDA

# Replacement
# Very weak relationship of the replacement matrix with teh environmental variables (R^2adj = 0.038, p = 0.007)
repl.dbrda <- vegan::dbrda(fish.repl ~ ., data = env, add = "cailliez")
anova(repl.dbrda)
RsquareAdj(repl.dbrda)


# Richness difference
# Well linked to environmental variables (R^2adj = 0.168, p = 0.001)
rich.dbrda <- vegan::dbrda(fish.rich ~ ., data = env, add = "cailliez")
anova(rich.dbrda)
RsquareAdj(rich.dbrda)


# Biplot of the 2 first axes confirms that large richness differences are mainly found among the higher elevation sites,
# and small richness difference among the sites in the lower part of the river, which are those where the discharge is the largest and the nitrogen content is also large.
graphics.off();  par(mfrow=c(1,1));
plot(rich.dbrda, scaling = 1, display = c("lc", "cn"), main = "Richness difference explained by environmental variables")




# ------------------------------------------------------------------------------
# Functional and phylogenetic diversity
# Alpha functional diversity
# ------------------------------------------------------------------------------
summary(fishtraits)
rownames(fishtraits)
names(spe)
names(fishtraits)


# 4 quantitative variables and six binary vairables
# Quantitaive functional traits are:
#  - BodyLength:  the average total body length of the adult fish
#  - BodyLengthMax:  the maximum total body length of the adult fish
#  - ShapeFactor:  the ratio of total body length to maximum body height
#  - TrophicLevel:  the relative positions of the species in the trophic chains

tra <- fishtraits[ , 6:15]
tra


# Distance-based functional diversity indices
# g  # cut the dendrogram using the number of groups as criterion
# 10 # choose the number of functional groups
res <- FD::dbFD(tra, as.matrix(spe), asym.bin = 5:10, stand.FRic = TRUE, clust.type = "ward.D", CWM.type = "all", calc.FGR = TRUE)

res



# --------------------------------------------------
# Variation in functional richness (top left), evenness (top right), divergence, dispersion
# Rao quadratic entropy and functional group richness
par(mfrow = c(3, 2))
plot(spa, asp = 1, pch = 21, cex.axis = 0.8, col = "white", bg = "brown", cex = res$FRic * 5, main = "Functional richness", xlab = "x coordinate (km)", ylab = "y coordinate (km)")
lines(spa, col = "light blue")
plot(spa, asp = 1, pch = 21, cex.axis = 0.8, col = "white", bg = "brown", cex = res$FEve * 6, main = "Functional evenness", xlab = "x coordinate (km)", ylab = "y coordinate (km)")
lines(spa, col = "light blue")
plot(spa, asp = 1, pch = 21, cex.axis = 0.8, col = "white", bg = "brown", cex = res$FDiv * 5, main = "Functional divergence", xlab = "x coordinate (km)", ylab = "y coordinate (km)")
lines(spa, col = "light blue")
plot(spa, asp = 1, pch = 21, cex.axis = 0.8, col = "white", bg = "brown", cex = res$FDis * 1.5, main = "Functional dispersion", xlab = "x coordinate (km)", ylab = "y coordinate (km)")
lines(spa, col = "light blue")
plot(spa, asp = 1, pch = 21, cex.axis = 0.8, col = "white", bg = "brown", cex = res$RaoQ / 2, main = "Rao quadratic entropy", xlab = "x coordinate (km)", ylab = "y coordinate (km)")
lines(spa, col = "light blue")
plot(spa, asp = 1, pch = 21, cex.axis = 0.8, col = "white", bg = "brown", cex = res$FGR / 2, main = "Functional group richness", xlab = "x coordinate (km)", ylab = "y coordinate (km)")
lines(spa, col = "light blue")



# --------------------------------------------------
# Add these indices to the div data frame
div$FRic <- res$FRic
div$FEve <- res$FEve
div$FDiv <- res$FDiv
div$FDis <- res$FDis
div$RaoQ <- res$RaoQ
div$FGR <- res$FGR
div



# --------------------------------------------------
# Community-weighted mean trait values (CWMs)
FD::functcomp(tra, as.matrix(spe), CWM.type = "all")

par(mfrow = c(2, 2))
plot(spa, asp = 1, pch = 21, col = "brown", bg = "orange", cex = (res$CWM$BodyLength - 100) / 40, main = "CWM Body Length", xlab = "x coordinate (km)", ylab = "y coordinate (km)")
lines(spa, col = "light blue")
plot(spa, asp = 1, pch = 21, col = "brown", bg = "orange", cex = res$CWM$ShapeFactor, main = "CWM Shape Factor", xlab = "x coordinate (km)", ylab = "y coordinate (km)")
lines(spa, col = "light blue")
plot(spa, asp = 1, pch = 21, col = "brown", bg = "orange", cex = res$CWM$TrophicLevel, main = "CWM Trophic Level", xlab = "x coordinate (km)", ylab = "y coordinate (km)")
lines(spa, col = "light blue")



# ------------------------------------------------------------------------------
# Functional and phylogenetic diversity
# Beta Taxonomic, Phylogenetic and functional diversity
# ------------------------------------------------------------------------------
# Distance matrix based on a simplified phylogenetic classification
# Retreive hierarchical classification from species list
splist <- as.character(fishtraits$LatinName)

# Database ncbi - less reliable than GBIF
# spcla <- classification(splist, db = "ncbi")
spcla <- taxize::classification(splist, db = "gbif")


# ------------------------------------------------------------
# WARNING: depending on the taxonomic status of the species, the function classification() may stop and ask the user to choose between two (or more) names.
# However, if this script is copied and pasted as a whole, then NAs will be produced and some species will be deleted.
# This will induce errors in the next steps of the analysis. To avoid that, run the "spcal <- classification..." only and make your choice. The function will then proceed.

# Compute the distance matrix and the phylogenetic tree
tr <- taxize::class2tree(spcla)
tr$classification
tr$distmat


# Convert the tree to a cophenetic matrix constrained between 0 and 1
phylo.d <- cophenetic(tr$phylo) / 100


# Replace full species names by name codes
rownames(phylo.d) <- names(spe)
colnames(phylo.d) <- names(spe)


# Functional dissimilarity matrix (Gower dissimilarity)
trait.d <- gowdis(tra, asym.bin = 5:10)


# Plot the tree and the dendrogram 
trait.gw <- hclust(trait.d, "ward.D2")
par(mfrow = c(1, 2))
plot(tr)
text(0.1, 27.5, "a", cex = 1.8)
plot(trait.gw, hang = -1, main = "")
text(26, 1.045, "b", cex = 1.8)


# Additive partitioning of TD, FD and PD (de Bello et al. 2010)
spe.rao <- Rao(sample = t(spe),
  dfunc = trait.d, # optional functional dissimilarity matrix
  dphyl = phylo.d, # optional phylogenetic distance matrix
  weight = FALSE,
  Jost = TRUE,
  structure = NULL
)
names(spe.rao)


# Species diversity (Simpson)
names(spe.rao$TD)
# Mean alpha Simpson diversity
spe.rao$TD$Mean_Alpha
# Gamma Simpson diversity
spe.rao$TD$Gamma
# Additive beta Simpson diversity
spe.rao$TD$Beta_add
spe.rao$TD$Gamma - spe.rao$TD$Mean_Alpha
# Beta diversity expressed as percentage of gamma
spe.rao$TD$Beta_prop
spe.rao$TD$Beta_add / spe.rao$TD$Gamma
# Multiplicative beta Simpson diversity
spe.rao$TD$Gamma / spe.rao$TD$Mean_Alpha


# Phylogenetic diversity (Rao)
names(spe.rao$PD)
# Mean alpha Rao phylogenetic diversity
spe.rao$PD$Mean_Alpha
# Gamma Rao phylogenetic diversity
spe.rao$PD$Gamma
# Additive beta Rao phylogenetic diversity
spe.rao$PD$Beta_add
spe.rao$PD$Gamma - spe.rao$PD$Mean_Alpha
# Beta phylogenetic diversity expressed as percentage of gamma
spe.rao$PD$Beta_prop
spe.rao$PD$Beta_add / spe.rao$PD$Gamma
# Multiplicative beta Rao phylogenetic diversity
spe.rao$PD$Gamma / spe.rao$PD$Mean_Alpha


# Functional diversity (Rao)
names(spe.rao$FD)
# Mean alpha Rao functional diversity
spe.rao$FD$Mean_Alpha
# Gamma Rao functional diversity
spe.rao$FD$Gamma
# Additive beta Rao functional diversity
spe.rao$FD$Beta_add
spe.rao$FD$Gamma - spe.rao$FD$Mean_Alpha
# Beta functional diversity expressed as percentage of gamma
spe.rao$FD$Beta_prop
spe.rao$FD$Beta_add / spe.rao$FD$Gamma
# Multiplicative beta Rao functional diversity
spe.rao$FD$Gamma / spe.rao$FD$Mean_Alpha


# Variation of alpha TD, PD and FD along the Doubs river
spe.rao$TD$Alpha
spe.rao$PD$Alpha
spe.rao$FD$Alpha

par(mfrow = c(2, 2))
plot(spa, asp = 1, cex.axis = 0.8, pch = 21, col = "white", bg = "brown", cex = spe.rao$TD$Alpha / 4, main = "Taxonomic diversity", xlab = "x coordinate (km)", ylab = "y coordinate (km)")
lines(spa, col = "light blue")
plot(spa, asp = 1, cex.axis = 0.8, pch = 21, col = "white", bg = "brown", cex = spe.rao$PD$Alpha * 1.5, main = "Phylogenetic diversity", xlab = "x coordinate (km)", ylab = "y coordinate (km)")
lines(spa, col = "light blue")
plot(spa, asp = 1, cex.axis = 0.8, pch = 21, col = "white", bg = "brown", cex = spe.rao$FD$Alpha * 2.5, main = "Functional diversity", xlab = "x coordinate (km)", ylab = "y coordinate (km)")
lines(spa, col = "light blue")


# Add Rao-based diversity indices to the div data frame
div$alphaTD <- spe.rao$TD$Alpha
div$alphaPD <- spe.rao$PD$Alpha
div$alphaFD <- spe.rao$FD$Alpha


# Save the div data frame as a CSV file
# write.csv(div, file = "diversity.csv", quote = FALSE)


# Save the species classification as a CSV file
# write.csv(tr$classification, file = "classification.csv", quote = FALSE)


