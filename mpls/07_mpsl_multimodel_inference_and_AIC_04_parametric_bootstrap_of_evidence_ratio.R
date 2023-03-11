# setwd("//media//kswada//MyFiles//R//mpls")
setwd("//media//kswada//MyFiles//R//Mixed_effects_and_multilevel_model//mpls")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This examples are based on "Longitudinal Data Analaysis for the Behavioral Sciences Using R"


# ------------------------------------------------------------------------------
# data:  MPLS
#   - Sample data from Minneapolis School District (MPLS)
# ------------------------------------------------------------------------------

MPLS.LS <- read.table("MPLS.LS.txt", header = T)


str(MPLS.LS)


dim(MPLS.LS)


car::some(MPLS.LS)



# ----------
MPLS.LS$grade5 <- MPLS.LS$grade - 5

MPLS.LS <- MPLS.LS %>% mutate(eth2 = ifelse(eth == "Whi", "W", "Other"))



# ------------------------------------------------------------------------------
# compare models by AICc and effect size measures
# ------------------------------------------------------------------------------

library(lmd4)
library(AICcmodavg)


# here we apply na.action = na.exclude to refit in bootstrapping
model.1 <- lmer(read ~ grade5 + risk2 + (grade5 | subid), MPLS.LS, REML = FALSE, na.action = na.exclude)

model.2 <- lmer(read ~ grade5 + eth + (grade5 | subid), MPLS.LS, REML = FALSE, na.action = na.exclude)

model.3 <- lmer(read ~ grade5 + risk2 + eth + (grade5 | subid), MPLS.LS, REML = FALSE, na.action = na.exclude)

model.4 <- lmer(read ~ grade5 * risk2 + (grade5 | subid), MPLS.LS, REML = FALSE, na.action = na.exclude)

model.5 <- lmer(read ~ grade5 * eth + (grade5 | subid), MPLS.LS, REML = FALSE, na.action = na.exclude)

model.6 <- lmer(read ~ grade5 * risk2 + grade5 * eth + (grade5 | subid), MPLS.LS, REML = FALSE, na.action = na.exclude)




# ----------
mynames <- paste0("M", as.character(1:6), sep = "")

mymodels <- list(model.1, model.2, model.3, model.4, model.5, model.6)

myaicc <- aictab(cand.set = mymodels,
                 modnames = mynames, sort = FALSE, second.ord = TRUE)


myaicc2 <- as.data.frame(myaicc) %>% arrange(AICc) %>% mutate(Cum.Wt = cumsum(AICcWt), Eratio = max(AICcWt) / AICcWt)

print(myaicc2, digits = 3)



# ------------------------------------------------------------------------------
# Parametric bootstrap of evidence ratio
#   - evidence ratio < Q90:  plausible
#   - evidence ratio > Q99:  very implausible
#   - q90 < evidence ratio < q95:  plausible with suspicion
# ------------------------------------------------------------------------------

boot.func <- function(){
  
  simdv <- simulate(model.1)
  
  mynames <- paste0("M", as.character(1:6), sep = "")
  
  mymodels <- list(refit(model.1, simdv[,1]), refit(model.2, simdv[,1]),
                   refit(model.3, simdv[,1]), refit(model.4, simdv[,1]),
                   refit(model.5, simdv[,1]), refit(model.6, simdv[,1]))
  
  myaicc <- as.data.frame(aictab(mymodels, mynames, sort = F))
  
  b.eratio <- max(myaicc$AICcWt) / myaicc$AiCcWt[1]
}



# ----------
set.seed(1)

B <- 999

library(plyr)


# rdply:  evaluate expression n times then combine results into a data frame
mystorage0 <- rdply(.n = B, .expr = boot.func, .progress = "text")



# ----------
# Extract and sort bootstrap ratios

mystorage.s <- sort(mystorage0[,2])

a <- c(0.1, 0.05, 0.01)

b <- (B + 1) * (1 - a)


myquant <- as.data.frame(mystorage.s[b])

colnames(myquant) <- "quantile"

rownames(myquant) <- c("90%", "95%", "99%")

myquant




