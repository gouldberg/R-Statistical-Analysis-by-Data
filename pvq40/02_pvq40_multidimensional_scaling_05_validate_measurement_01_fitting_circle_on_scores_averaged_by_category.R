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
# Fit circle for averaged measurement
# ------------------------------------------------------------------------------

erg <- smacof::fitCircle(result$conf[,1], result$conf[,2])


erg


draw.circle(erg$cx, erg$cy, radius = erg$radius, border = "black", lty = 2)


# -->
# The ten value points are close to a circle fitted to the point configuration using the fitCircle().
# Moreover, the order of the points on this circle replicates what many other studies have found.

# Also the configurations exhibits an interpretation in terms of two bipolar directions:
# self-enhancement versus self-transcendence, and openness to change versus conservation.


