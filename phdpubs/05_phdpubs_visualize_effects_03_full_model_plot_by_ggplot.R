setwd("//media//kswada//MyFiles//R//phdpubs")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PhdPubs data
# ------------------------------------------------------------------------------

data("PhdPubs", package = "vcdExtra")


dim(PhdPubs)

str(PhdPubs)


car::some(PhdPubs)



# ----------
PhdPubs <- within(PhdPubs, {
  female <- factor(female)
  married <- factor(married)
})



# ----------
modp <- glm(articles ~ ., data = PhdPubs, family = poisson(link = "log"))



# ------------------------------------------------------------------------------
# Full-model plots by ggplots
# ------------------------------------------------------------------------------

phd.fitp <- cbind(PhdPubs, Articles = predict(modp, type = "response"))


head(phd.fitp)




library(ggplot2)


graphics.off()

gg <- ggplot(phd.fitp, aes(x = mentor, y = Articles)) + theme_bw() + geom_line(size = 2.5, aes(color = married)) + geom_point(colour = "black", size = 1.5)

gg + facet_grid(~ kid5)

