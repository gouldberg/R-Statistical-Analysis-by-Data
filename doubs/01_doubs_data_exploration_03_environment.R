setwd("//media//kswada//MyFiles//R//doubs")

packages <- c("dplyr", "vegan", "RgoogleMaps", "googleVis", "gclus")
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



source("./functions/panelutils.R")



# ------------------------------------------------------------------------------
# Site - Environmental data
# ------------------------------------------------------------------------------

car::some(env)


summary(env)


psych::describe(env)




# ------------------------------------------------------------------------------
# pairs plot
# ------------------------------------------------------------------------------

pairs(env, panel = panel.smooth, diag.panel = panel.hist, main = "Bivariate Plots with Histograms and Smooth Curves")




# ------------------------------------------------------------------------------
# scatter plot by z-scores
# ------------------------------------------------------------------------------

env.z <- vegan::decostand(env, "standardize")

# env.z <- as.data.frame(scale(env))


apply(env.z, 2, mean)

apply(env.z, 2, sd)


pairs(env.z, panel = panel.smooth, diag.panel = panel.hist, main = "Bivariate Plots with Histograms and Smooth Curves")



# ------------------------------------------------------------------------------
# Kendall's tau rank correlation among environmental variables
# ------------------------------------------------------------------------------

env.ken <- cor(env, method = "kendall")


# gclus::single:  reorder the variables prior to plotting
# orders objects using hierarchical clustering
env.o <- gclus::order.single(env.ken)


pairs(env[,env.o], lower.panel = panel.smoothb, upper.panel = panel.cor, no.col = TRUE, method = "kendall", diag.panel = panel.hist, main = "Kendall Correlation Matrix")



# -->
# "slo" has negative correlation with almost all environmental variables (except "oxy", "pH")
# "ele" has very strong negative correlation with "dis"
# "pH" has no significant correlation with other environmental variables
# "oxy" has negative correlation with other environmental variables whereas other variables such as har, dis, nit, amm pho and bod have positive correlation among those




# difference between kendall and pearson
( cor_p <- cor(env[,env.o], method = "pearson") )

( cor_k <- cor(env[,env.o], method = "kendall") )

round(cor_k - cor_p, digits = 3)




# ----------
# NOTE that "dfs" has different sign but same abolute value with "ele"
# kendall tau rank correlation between "dfs" and "ele" is just -1 !!
round(cor_k, digits = 3)

