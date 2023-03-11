setwd("//media//kswada//MyFiles//R//berkeley_growth")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  berkeley growth
#   - The data from the Berkeley Growth Study (Tuddenham and Snyder, 1954).
# ------------------------------------------------------------------------------

data("growth", package = "fda")

str(growth)



# ----------
# 39 boys and 31 points
# a 31 by 39 numeric matrix giging the heights in centimeters of 39 boys at 31 ages
dim(growth$hgtm)


# 54 girls and 31 points
# a 31 by 54 numeric matrix giging the heights in centimeters of 54 girls at 31 ages
dim(growth$hgtf)



# ----------
head(growth$hgtm)


head(growth$hgtf)



# ----------
growth$age


# -->
# The ages are not equally spaced;
# There are four measurements while the child is one year old, annual measurements from two to eight years,
# followed by heights measured biannually



# ------------------------------------------------------------------------------
# Landmark registration automatically
# ------------------------------------------------------------------------------
#  This is an automatic PGS (pubertal growth sput) spurt identification procedure.
#  A mouse click advances the plot to the next case.
#  Compute PGS mid point for landmark registration.
#  Downward crossings are computed within the limits defined
#  by INDEX.  Each of the crossings within this interval
#  are plotted.  The estimated PGS center is plotted as a vertical line.

#  The choice of range of argument values (6--18) to consider
#  for a potential mid PGS location is determined by previous
#  analyses, where they have a mean of about 12 and a s.d. of 1.

#  We compute landmarks for all 54 children

index  = 1:102  #  wide limits

nindex = length(index)

ageval = seq(8.5, 15, len=nindex)

PGSctr = rep(0, ncasef)

op = par(ask = TRUE)

for (icase in 1:ncasef) {
  accveci = eval.fd(ageval, accelfdUN[icase])
  aup     = accveci[2:nindex]
  adn     = accveci[1:(nindex-1)]
  indx    = (1:(nindex-1))[adn * aup < 0 & adn > 0]
  plot(ageval[2:nindex], aup, "p", xlim=c(7.9, 18), ylim=c(-6, 4))
  lines(c(8, 18), c(0, 0), lty=2)
  
  for (j in 1:length(indx)) {
    indxj = indx[j]
    aupj  = aup[indxj]
    adnj  = adn[indxj]
    agej  = ageval[indxj] + 0.1*(adnj/(adnj-aupj))
    if (j == length(indx)) {
      PGSctr[icase] = agej
      lines(c(agej, agej), c(-4,4), lty=1)
    } else {
      lines(c(agej, agej), c(-4,4), lty=3)
    }
  }
  title(paste('Case ',icase))
  # ****** CLICK ON EACH PLOT TO ADVANCE TO THE NEXT
  # ****** {par(ask=TRUE)}
}

par(op)

PGSctr


