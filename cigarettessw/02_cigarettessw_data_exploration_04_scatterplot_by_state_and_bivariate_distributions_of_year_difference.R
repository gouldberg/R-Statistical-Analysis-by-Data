setwd("//media//kswada//MyFiles//R//cigarettessw")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2", "AER", "foreign", "car", "lmtest", "stargazer", "broom", "knitr", "POE5Rdata")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# data:  CigarettesSW
# ------------------------------------------------------------------------------


data("CigarettesSW", package = "AER")



dim(CigarettesSW)


str(CigarettesSW)



car::some(CigarettesSW)




# ----------
CigarettesSW$rprice <- with(CigarettesSW, price / cpi)

CigarettesSW$salestax <- with(CigarettesSW, (taxs - tax) / cpi)

CigarettesSW$rincome <- with(CigarettesSW, income / population / cpi)

CigarettesSW$cigtax <- with(CigarettesSW, tax / cpi)




# ------------------------------------------------------------------------------
# data exploration: scatterplot by state,  change from 1985 to 1995
# ------------------------------------------------------------------------------


# change from 1985 to 1995

xyplot(rprice + salestax + cigtax ~ year | state, data = CigarettesSW, col = "black", pch = 1:3, type = "o")




# ------------------------------------------------------------------------------
# scatterplot matrix for differences from 1985 to 1995
# ------------------------------------------------------------------------------


packsdiff <- log(c1995$packs) - log(c1985$packs)


pricediff <- log(c1995$price/c1995$cpi) - log(c1985$price/c1985$cpi)


incomediff <- log(c1995$income/c1995$population/c1995$cpi) - log(c1985$income/c1985$population/c1985$cpi)


salestaxdiff <- (c1995$taxs - c1995$tax)/c1995$cpi - (c1985$taxs - c1985$tax)/c1985$cpi


cigtaxdiff <- c1995$tax/c1995$cpi - c1985$tax/c1985$cpi




# ----------
tmp <- data.frame(packsdiff, pricediff, salestaxdiff, cigtaxdiff, incomediff)



# ----------
par(mar = c(2,2,2,2))

formula <- ~ packsdiff + pricediff + salestaxdiff + cigtaxdiff + incomediff

scatterplotMatrix(formula, data = tmp,
                  smooth = FALSE, ellipse = list(levels = 0.5), 
                  id = list(n = 3), col = c("black"), pch = c(1, 20))


