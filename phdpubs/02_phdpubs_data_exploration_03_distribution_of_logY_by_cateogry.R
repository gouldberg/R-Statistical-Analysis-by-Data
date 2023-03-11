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



# ------------------------------------------------------------------------------
# data exploration:  log Y by category
# ------------------------------------------------------------------------------


boxplot(articles + 1 ~ married, data = PhdPubs, log = "y", varwidth = TRUE, ylab = "log(articles + 1)", xlab = "married + female", cex.lab = 1.25)


# -->
# The distribution of articles for married and non-married are quite similar,
# except that for the married students there are quite a few observations with a large number of publications.



# ----------
# combination of married and female
boxplot(articles + 1 ~ married + female, data = PhdPubs, log = "y", varwidth = TRUE, ylab = "log(articles + 1)", xlab = "married + female", cex.lab = 1.25)




# ------------------------------------------------------------------------------
# data exploration:  log Y (only positive values) by category
# ------------------------------------------------------------------------------

library(ggplot2)


PhdPubsPos <- subset(PhdPubs, articles > 0)


ggplot(PhdPubsPos, aes(x = female, y = articles)) + geom_boxplot(outlier.size = 3, notch = TRUE, aes(fill = married), alpha = 0.3) +
         geom_jitter(position = position_jitter(width= 0.1), alpha = 0.25) +
         facet_grid(. ~ kid5) + 
         scale_y_log10(breaks = c(0, 1, 2, 5, 10, 20, 50, 100, 200)) +
         theme(legend.position = "none") + labs(y = "articles (log scale)")


