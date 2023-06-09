setwd("//media//kswada//MyFiles//R//pvq40")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PVQ40
# ------------------------------------------------------------------------------

data(PVQ40, package = "smacof")


str(PVQ40)


attributes(PVQ40)


car::some(PVQ40)



# ------------------------------------------------------------------------------
# MDS on scores averaged by category
# ------------------------------------------------------------------------------

var_n <- c("SE", "CO", "TR", "BE", "UN", "SD", "ST", "HE", "AC", "PO")

var_n_select <- c("se1, se2", "co1, co2", "tr1, tr2", "be1, be2", "un1, un2, un3", "sd1, sd2", "st1, st2", "he1, he2", "ac1, ac2", "po1, po2")


# Take row means for each category of measure
for(i in 1:length(var_n)){
  eval(parse(text = paste0(var_n[i], " <- rowMeans(subset(PVQ40, select = c(", var_n_select[i], ")), na.rm = TRUE)")))
}

raw <- cbind(SE, CO, TR, BE, UN, SE, ST, HE, AC, PO)



# ----------
R <- cor(raw)

diss <- sim2diss(R)

result <- mds(diss, init = "torgerson", type = "ordinal")

plot(result)


# Stress Per Plot (SPP)
# plot(result, plot.type = "bubbleplot")



# ------------------------------------------------------------------------------
# Compare exploratory MDS and confirmatory (circular) MDS
# ------------------------------------------------------------------------------

# method = max(wish):  integer value from which the similarity is subtracted
idiss <- sim2diss(R, method = max(R))



# ----------
# Enforces a strict circle
res1 <- smacofSphere(idiss, type = "ordinal")
# res1 <- smacofSphere(idiss)


# uses a penalty function that pushes the MDS solution in the direction of a perfect circle
# default penalty = 100 and when setting it to 22, say, the force that pulls the solution toward a perfect circle is mitigated.
# When setting the penalty weight to 100 (as default), then the circle is perfect, too.

res2 <- smacofSphere(idiss, type = "ordinal", algorithm = "dual", penalty = 22)
# res2 <- smacofSphere(idiss, algorithm = "dual", penalty = 1)



# Exploratory MDS
res3 <- mds(idiss, type = "ordinal")
# res3 <- mds(idiss)



# ----------
res1$stress

res2$stress 

res3$stress 




# ----------
op <- par(mfrow = c(1,3))

rownames(idiss)

plot(res1, main="Circular MDS (primal)")

plot(res2, main="Circular MDS 2 (dual)")

plot(res3, main="Exploratory MDS")

par(op)



# -->
# As expected, the solution generated by the default algorithm has all country points on a perfect circle,
# while the solution computed by the dual algorithmn and using penalty only 1 is almost perfect circle.

# The increment in Stress is not much high.
# The exporatory MDS solution is not that far from being circular:
