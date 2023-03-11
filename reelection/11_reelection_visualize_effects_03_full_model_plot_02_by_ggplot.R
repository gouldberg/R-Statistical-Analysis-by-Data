setwd("//media//kswada//MyFiles//R//reelection")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Reelection
# ------------------------------------------------------------------------------

data("Reelection", package = "pder")


str(Reelection)


dim(Reelection)


car::some(Reelection)



# ----------
elect.l <- glm(reelect ~ ddefterm + ddefey + gdppc + dev + nd + maj,
               data = Reelection,
               family = "binomial", subset = narrow)


elect.p <- update(elect.l, family = binomial(link = "probit"))



mod_obj <- elect.l

# mod_obj <- elect.p



# ------------------------------------------------------------------------------
# Full-model plots by ggplots
# ------------------------------------------------------------------------------

re.fitp <- cbind(na.omit(subset(Reelection, narrow == "TRUE")), Probability = predict(mod_obj, type = "response"))


head(re.fitp)




library(ggplot2)


graphics.off()

gg <- ggplot(re.fitp, aes(x = gdppc, y = Probability)) + theme_bw() + geom_line(size = 2.5, aes(color = dev)) + geom_point(colour = "black", size = 1.5)

gg + facet_grid(~ dev + maj)

