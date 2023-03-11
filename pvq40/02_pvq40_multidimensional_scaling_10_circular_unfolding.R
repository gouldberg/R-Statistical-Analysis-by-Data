setwd("//media//kswada//MyFiles//R//pvq40")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PVQ40
# ------------------------------------------------------------------------------

data(PVQ40, package = "smacof")
# data(PVQ40agg, package = "smacof")


str(PVQ40)

attributes(PVQ40)


car::some(PVQ40)
# car::some(PVQ40agg)



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

res <- mds(diss, init = "torgerson", type = "ordinal")



# ----------
graphics.off()
par(mfrow=c(1,1))
plot(res)
polygon(res$conf, lty = 2)

erg    <- fitCircle(res$conf[,1], res$conf[,2])
draw.circle(erg$cx, erg$cy, radius = erg$radius, border = "blue", lty = 2)



# ------------------------------------------------------------------------------
# Unfolding:  circular unfolding
#   - This model is the same as regular idela-point unfolding, except that it imposes a particular restriction onto the object
#     points (or the person points):  They must all lie on a circle.
#   - An application of this model is the case of importance ratings for personal values
# ------------------------------------------------------------------------------

# Preferences into dissimilarities
# pref <- max(PVQ40agg) - PVQ40agg

# Preferences into dissimilarities
( pref <- max(raw) - raw )


# ----------
# Search for an unfolding solution where all value points (formally: "column points") are strictly on a circle.
result <- unfolding(pref)
result_c <- unfolding(pref, circle = "column")


result$stress
result_c$stress



# ----------
par(mfrow=c(2,2))
plot(result_c)
plot(result_c, plot.type = "Shepard")
plot(result)
plot(result, plot.type = "Shepard")


# -->
# Perfect circle solution with a Stress of 0.237.
# Without the circle constraint with a Stress of 0.228.
# The additional constraint made almost no difference in terms of the overall model fit, and so this is an attractive unfolding model,
# because it suggests a simple law of formation.


