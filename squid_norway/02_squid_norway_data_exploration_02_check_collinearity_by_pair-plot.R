setwd("//media//kswada//MyFiles//R//squid_norway")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Squid Norway
# ------------------------------------------------------------------------------
Squid <- read.table(file = "SquidNorway.txt", header = TRUE)


str(Squid)



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

MyVar <- c("d15N" , "Lat", "Depth", "ML")

pairs(Squid[,MyVar], lower.panel = panel.cor)


# -->
# As to collinearity, the variables latitude and depth are moderately collinear, and we should drop one of these variables.
# We prefer to drop depth, as it has only a few unique values.
# There seems to be a relationship between mantle length (ML) and d15N and the relationship looks linear.


