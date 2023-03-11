setwd("//media//kswada//MyFiles//R//ricefarms")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  RiceFarms
# ------------------------------------------------------------------------------

data("RiceFarms", package = "splm")


str(RiceFarms)


dim(RiceFarms)



# ------------------------------------------------------------------------------
# Tests for random effects, spatial and serial error correlation
# Conditional BSJK tests
# ------------------------------------------------------------------------------

# We now address the issue of serial correlation in the remainder errors of model.
# In other words, we check whether persistence characteristics in the output of an individual farm have effectively been accounted for by the individual effects,
# which in previous examples have proved significant,
# statistical evidence favoring the random hypothesis.
# On the spatial side, there has been ample evidence of spatial effects of the SEM type.
# For all this, on one hand a joint test is guaranteed to reject; on the other, tests for each single "effect" (spatial or serial correlation,
# or individual effects) will have to account for the possible presence of one or both of the others.


bsjk.LM <- matrix(ncol = 4, nrow = 2)


# J:  joint test
# C.2:  here particularly interested
tests <- c("J", paste("C", 1:3, sep = "."))


dimnames(bsjk.LM) <- list(c("LM test", "p-value"), tests)


for(i in tests){
  
  mytest <- bsjktest(riceprod, data = RiceFarms, index = "id", list = ricelw, test = i)
  
  bsjk.LM[1,i] <- mytest$statistic
  
  bsjk.LM[2,i] <- mytest$p.value
}



round(bsjk.LM, 6)




# -->
# All test reject the respective null hypotheses:  the joint and the C.1 (spatial effects) most forcefully
# and then the C.3 (random effects).
# The C.2 test rejects less forcefully
# still it provides evidence for some serial correlation in the remainder errors of the rice production equation
# AFTER controlling for spatial and individual random effects


