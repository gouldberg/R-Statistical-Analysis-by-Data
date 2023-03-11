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
source("./functions/panelutils.R")
source("./functions/Rao.R")



# ------------------------------------------------------------------------------
# relative frequency by site
# ------------------------------------------------------------------------------


# number of species by site (= N0)
( q <- rowSums(spe > 0) )

# this is same
q <- specnumber(spe)



# ---------
# total abundance of all species by site
( n <- apply(spe, 1, sum) )



# ---------
# the relative frequency by site
p <- data.frame()

for(i in 1:nrow(spe)){
  p0 <- spe[i,] / n[i]
  p <- rbind(p, p0)
}

colnames(p) <- colnames(spe)


p[1,]

p[2,]




# ------------------------------------------------------------------------------
# Shannon's index (Shannon's entropy) by site
# Shannon's diversity
# Shannon's evenness
# ------------------------------------------------------------------------------

# Shannon's index (Shannon's entropy) by site
H <- sapply(1:nrow(spe), function(x){
  tmp <- p[x,]
  tmp <- tmp[tmp > 0]
  -sum(tmp * log(tmp))
})


H



# This is same
H <- diversity(spe)


# base 2
Hb2 <- diversity(spe, base = 2)



# ----------
# Shannon's diversity
( N1 <- exp(H) )
( N1b2 <- exp(Hb2) )



# ----------
# Shannon's evenness = diversity / species richness
( E10 <- N1 / q )




# ------------------------------------------------------------------------------
# Pielou evenness J by site
# ------------------------------------------------------------------------------

( Hmax <- log(q) )


( J <- H / Hmax )



# -->
# Because Shannon entropy is zero when species richness is one, Pielou evenness J cannot be calculated for site #1
# Pielou evenness is biased because it is systematically positively correlated with species richness



# ------------------------------------------------------------------------------
# Simpson's concentration index:  lambda
# ------------------------------------------------------------------------------

# This gives the probability that two randomly chosen organisms belong to the same species

lambda <- sapply(1:nrow(spe), function(x){
  tmp <- spe[x,]
  bunbo <- sum(tmp) * (sum(tmp) - 1)
  tmp <- tmp[tmp > 0]
  bunshi <- sum(tmp * (tmp - 1))
  bunshi / bunbo
})


lambda



# if species richness is large, lambda is close to:
lambda <- apply(p^2, 1, sum)




# ------------------------------------------------------------------------------
# Simpson's diversity  (inverse Simpson index)
# Simpson's evenness
# ------------------------------------------------------------------------------

( N2 <- diversity(spe, "inv") )


# this is same
1 / lambda



# ----------
# Simpson's evenness = diversity / species richness
E20 <- N2 / q




# ------------------------------------------------------------------------------
# All diversity indices
# ------------------------------------------------------------------------------


( div <- data.frame(q, H, Hb2, N1, N1b2, N2, E10, E20, J) )



# only most widely used quantities
# q: species richness
# H: Shannon entropy (base e)
# N2: Simpson diversity (inverse Simpson index)
( div_most <- data.frame(q, H, N2) )



# ------------------------------------------------------------------------------
# Correlation among diversity indices
# ------------------------------------------------------------------------------

# Correlations among diversity indices
cor(div)

cor(div_most)



# ----------
# Contrary to Hill's ratios, Pielou evenness is biased because it is systematically positively correlated with species richness.
pairs(div[-1, ], lower.panel = panel.smooth, upper.panel = panel.cor, diag.panel = panel.hist, main = "Pearson Correlation Matrix")


pairs(div_most[-1, ], lower.panel = panel.smooth, upper.panel = panel.cor, diag.panel = panel.hist, main = "Pearson Correlation Matrix")




# ------------------------------------------------------------------------------
# plot q, H, N2 on map
# q: species richness
# H: Shannon entropy (base e)
# N2: Simpson diversity (inverse Simpson index)
# ------------------------------------------------------------------------------

par(mfrow = c(2,2))

plot(spa, asp = 1, cex.axis = 0.8, pch = 21, col = "white", bg = "blue", 
     cex = sqrt(div_most$q / max(div_most$q)) * 5,
                main = "species richness", 
                xlab = "x coordinate (km)", ylab = "y coordinate (km)")

lines(spa, col = "light blue")



plot(spa, asp = 1, cex.axis = 0.8, pch = 21, col = "white", bg = "brown", 
     cex = sqrt(div_most$H / max(div_most$H)) * 5,
     main = "Shannon Entropy", 
     xlab = "x coordinate (km)", ylab = "y coordinate (km)")

lines(spa, col = "light blue")


plot(spa, asp = 1, cex.axis = 0.8, pch = 21, col = "white", bg = "orange", 
     cex = sqrt(div_most$N2 / max(div_most$N2)) * 5,
     main = "Simpson diversity", 
     xlab = "x coordinate (km)", ylab = "y coordinate (km)")

lines(spa, col = "light blue")




# ------------------------------------------------------------------------------
# plot Evenness
# ------------------------------------------------------------------------------

par(mfrow = c(2,2))

plot(spa, asp = 1, cex.axis = 0.8, pch = 21, col = "white", bg = "blue", 
     cex = sqrt(div$q / max(div$q)) * 5,
     main = "species richness", 
     xlab = "x coordinate (km)", ylab = "y coordinate (km)")

lines(spa, col = "light blue")


plot(spa, asp = 1, cex.axis = 0.8, pch = 21, col = "white", bg = "brown", 
     cex = sqrt(div$E10 / max(div$E10)) * 5,
     main = "Shannon's Evenness", 
     xlab = "x coordinate (km)", ylab = "y coordinate (km)")

lines(spa, col = "light blue")


plot(spa, asp = 1, cex.axis = 0.8, pch = 21, col = "white", bg = "orange", 
     cex = sqrt(div$E20 / max(div$E20)) * 5,
     main = "Simpson's Evenness", 
     xlab = "x coordinate (km)", ylab = "y coordinate (km)")

lines(spa, col = "light blue")


plot(spa, asp = 1, cex.axis = 0.8, pch = 21, col = "white", bg = "green", 
     cex = sqrt(div$J / max(na.omit(div$J))) * 5,
     main = "Pielou Evenness", 
     xlab = "x coordinate (km)", ylab = "y coordinate (km)")

lines(spa, col = "light blue")
