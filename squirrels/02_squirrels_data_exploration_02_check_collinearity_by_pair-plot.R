setwd("//media//kswada//MyFiles//R//squirrels")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Red Squirrels
# ------------------------------------------------------------------------------

SQ <- read.table(file = "RedSquirrels.txt", header = TRUE, dec = ".")


str(SQ)


# ----------
# remove large DBH
SQ2 <- subset(SQ, DBH < 0.6)



# ------------------------------------------------------------------------------
# Data exploration:  Check collinearity by pairplot
# ------------------------------------------------------------------------------

panel.cor <- function(x, y, digits=1, prefix="", cex.cor = 6)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r1 = cor(x,y,use="pairwise.complete.obs")
  r <- abs(cor(x, y,use="pairwise.complete.obs"))
  txt <- format(c(r1, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) { cex <- 0.9/strwidth(txt) } else {cex = cex.cor}
  text(0.5, 0.5, txt, cex = cex * r)
}


MyVar <- c("SqCones", "Ntrees", "DBH", "TreeHeight", "CanopyCover")

pairs(SQ2[,MyVar], lower.panel = panel.cor)


# -->
# The pairplot indicates that there may be collinearity between Ntrees and DBH, and also between DBH and TreeHeight.
# Both correlations make biological sense.
# The more trees in a plot, the smaller the DBH is likely to be, and the larger the trees, the greater the DBH.

# Before removing anything, we inspect variance inflation factors (VIF).

