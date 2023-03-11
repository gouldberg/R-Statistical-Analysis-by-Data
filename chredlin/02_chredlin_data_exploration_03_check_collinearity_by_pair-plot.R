setwd("//media//kswada//MyFiles//R//chredlin")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  chredlin
# ------------------------------------------------------------------------------

data("chredlin", package = "faraway")

str(chredlin)



# ------------------------------------------------------------------------------
# Assess collineartity among continuous variables:  multi-panel scatterplots
# ------------------------------------------------------------------------------

Mypairs <- function(Z) {
  MyVarx <- colnames(Z)
  pairs(Z, labels = MyVarx,
        cex.labels =  2,
        lower.panel = function(x, y, digits=2, prefix="", cex.cor = 7) {
          panel.cor(x, y, digits, prefix, cex.cor)}, 
        upper.panel =  function(x, y) points(x, y, 
                                             pch = 16, cex = 0.8, 
                                             col = gray(0.1)))
  #print(P)
}


panel.cor <- function(x, y, digits=1, prefix="", cex.cor = 6)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r1=cor(x,y,use="pairwise.complete.obs")
  r <- abs(cor(x, y,use="pairwise.complete.obs"))
  txt <- format(c(r1, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) { cex <- 0.9/strwidth(txt) } else {
    cex = cex.cor}
  text(0.5, 0.5, txt, cex = cex * r)
}



MyVar <- c("race", "fire", "theft", "age", "involact", "income")

Mypairs(chredlin[, MyVar])




# ------------------------------------------------------------------------------
# pairs panels with spearman correlation
# ------------------------------------------------------------------------------

psych::pairs.panels(chredlin, method = "spearman", stars = TRUE)



# -->
# race and fire is strongly correlated
# fire, race are strongly correlated to involact




# ------------------------------------------------------------------------------
# Also by ggplot with confidence intervals
# ------------------------------------------------------------------------------

library(ggplot2)

ggplot(chredlin, aes(race, fire)) + geom_point() + stat_smooth(method = "lm", col = "blue") + stat_smooth(method = "loess", col = "red")


ggplot(chredlin, aes(race, theft)) + geom_point() + stat_smooth(method = "lm", col = "blue") + stat_smooth(method = "loess", col = "red")



# -->
# We see that there is indeed a relationship between the fire rate and the percentage of minorities.
# We also see that there is large outlier that may have a disproportionate effect on the relationship between the theft rate and the percentage of minorities.

# race vs. income,  fire vs. theft, race vs, race are moderately correlated.

