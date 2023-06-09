setwd("//media//kswada//MyFiles//R//intelligence")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  intelligence
# ------------------------------------------------------------------------------

data(intelligence, package = "smacof")


intelligence

car::some(intelligence)



# ------------------------------------------------------------------------------
# Compare exploratory MDS and confirmatory (circular) MDS
# ------------------------------------------------------------------------------

# method = max(wish):  integer value from which the similarity is subtracted
idiss <- sim2diss(intelligence[,paste0("T", 1:8)], method = max(wish))




# ----------
# Enforces a strict circle
# res1 <- smacofSphere(idiss, type = "ordinal")
res1 <- smacofSphere(idiss)


# uses a penalty function that pushes the MDS solution in the direction of a perfect circle
# default penalty = 100 and when setting it to 22, say, the force that pulls the solution toward a perfect circle is mitigated.
# When setting the penalty weight to 100 (as default), then the circle is perfect, too.

# res2 <- smacofSphere(idiss, type = "ordinal", algorithm = "dual", penalty = 22)
res2 <- smacofSphere(idiss, algorithm = "dual", penalty = 1)



# Exploratory MDS
# res3 <- mds(idiss, type = "ordinal")
res3 <- mds(idiss)



# ----------
res1$stress

res2$stress 

res3$stress 




# ----------
op <- par(mfrow = c(1,3))

colnames(idiss)

plot(res1, main="Circular MDS (primal)")

plot(res2, main="Circular MDS 2 (dual)")

plot(res3, main="Exploratory MDS")

par(op)



# -->
# As expected, the solution generated by the default algorithm has all country points on a perfect circle,
# while the solution computed by the dual algorithmn and using penalty only 1 is almost perfect circle.

# The increment in Stress is not much high.
# The exporatory MDS solution is not that far from being circular:
