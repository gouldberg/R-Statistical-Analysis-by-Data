# setwd("//media//kswada//MyFiles//R//coffee")
setwd("//media//kswada//MyFiles//R//Binary_ratio_multinominal_response_and_related_model//coffee")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Coffee
# ------------------------------------------------------------------------------

Coffee <- read.table(file = "Coffee.dat", header = T, stringsAsFactors = FALSE)


str(Coffee)


car::some(Coffee)



# ----------
from <- c(1, 2, 3, 4, 5)
to <- c("X01_HighPoint", "X02_TastersChoice", "X03_Sanka", "X04_Nescafe", "X05_Brim")
Coffee$brand <- to[match(Coffee$y, from)]

library(tidyverse)
tmp <- Coffee %>% dplyr::select(person, purchase, brand) %>% spread(., key = purchase, value = brand)
# 0: second purchase  1: first purchase
colnames(tmp) <- c("person", "second_p", "first_p")
cof.tab <- xtabs(~ first_p + second_p, data = tmp)


cof.tab



# ----------
# Coffee2 <- read.table(file = "Coffee2.dat", header = T, stringsAsFactors = FALSE)
# Coffee2



# ------------------------------------------------------------------------------
# Create data.frame for symmetry and quasi-symmetry model
# ------------------------------------------------------------------------------

tmp <- data.frame(cof.tab)

brands <- unique(tmp$first_p)

n <- length(brands)


output <- data.frame()

for(i in 1:(n - 1)){
  for(j in (i + 1):n){
    output0 <- c(rep(0, n))
    output0[i] <- 1
    output0[j] <- -1
    output <- rbind(output, output0)
  }
}


colnames(output) <- brands


output



# ----------
nij <- c()

nji <- c()

for(i in 1:nrow(output)){
  brand1 <- brands[which(output[i,] == 1)]
  brand2 <- brands[which(output[i,] == -1)]
  nij0 <- tmp %>% filter(first_p %in% brand1, second_p %in% brand2) %>% dplyr::select(Freq) %>% pull()
  nji0 <- tmp %>% filter(second_p %in% brand1, first_p %in% brand2) %>% dplyr::select(Freq) %>% pull()
  nij <- c(nij, nij0)
  nji <- c(nji, nji0)
}


output$nij <- nij

output$nji <- nji


output



# -->
# 10 pairs of opposite celss for nij and nji



# ------------------------------------------------------------------------------
# Test marginal homogeneity by deviance difference between the symmetry model and the quasi-symmetry model
# ------------------------------------------------------------------------------


# -1 in model statement sets intercept = 0
symm <- glm(nij / (nij + nji) ~ -1, family = binomial, weights = nij + nji, data = output)


summary(symm)



# ----------
QS <- glm(nij / (nij + nji) ~ -1 + X01_HighPoint + X02_TastersChoice + X03_Sanka + X04_Nescafe + X05_Brim, family = binomial, weights = nij + nji, data = output)


summary(QS)



# -->
# One category (Brim) has estimate = 0



# ----------
anova(symm, QS, test = "Chisq")



# The deviance difference of 22.47 - 9.97 = 12.50 between the symmetry model and the quasi-symmetry model, based on df = 4,
# provides evidence of marginal heterogeneity (p = 0.0014)


