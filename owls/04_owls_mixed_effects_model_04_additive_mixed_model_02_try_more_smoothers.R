# setwd("//media//kswada//MyFiles//R//owls")
setwd("//media//kswada//MyFiles//R//Mixed_effects_and_multilevel_model//owls")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Owls
# ------------------------------------------------------------------------------

Owls <- read.table(file = "Owls.txt", header = TRUE)


str(Owls)

dim(Owls)

car::some(Owls)




# ------------------------------------------------------------------------------
# Try nidek wutg 2 smoothers
# ------------------------------------------------------------------------------

M7 <- gamm(NegPerChick ~ FoodTreatment + 
          s(ArrivalTime, by = as.numeric(FoodTreatment=="Deprived")) +
          s(ArrivalTime, by = as.numeric(FoodTreatment=="Satiated")), random = list(Nest = ~1), data = Owls)


M8 <- gamm(NegPerChick ~ FoodTreatment +
          s(ArrivalTime, by = as.numeric(SexParent=="Female")) +
          s(ArrivalTime, by = as.numeric(SexParent=="Male")), random = list(Nest = ~1), data = Owls)


AIC(M6$lme, M7$lme, M8$lme)

