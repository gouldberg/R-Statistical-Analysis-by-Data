setwd("//media//kswada//MyFiles//R//chredlin")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  chredlin
# ------------------------------------------------------------------------------

data("chredlin", package = "faraway")

str(chredlin)




# ----------
linmod <- lm(involact ~ race + fire + theft + age + log(income), data = chredlin)

# linmod <- lm(involact ~ race + fire + theft + age + income, data = chredlin)



# ------------------------------------------------------------------------------
# Sensitivity analysis for "race" variable
# ------------------------------------------------------------------------------

( listcombo <- unlist(sapply(0:4, function(x) combn(4, x, simplify = FALSE)), recursive = FALSE) )


( predterms <- lapply(listcombo, function(x) paste(c("race", c("fire", "theft", "age", "log(income)")[x]), collapse = "+")) )


coefm <- matrix(NA, 16, 2)



# ----------
for(i in 1:16){
  lmi <- lm(as.formula(paste("involact ~ ", predterms[[i]])), data = chredlin)

  # "race"'s estimated coefficient and pvalue
  coefm[i,] <- summary(lmi)$coef[2,c(1,4)]
}


rownames(coefm) <- predterms

colnames(coefm) <- c("beta", "pvalue")




# ----------
round(coefm, 4)


# -->
# We can see that the value of coefficient for race varies somewhat with a high value about double the low value.
# But in no case does the p-value rise above 5%.
# So although we may have some uncertainty over the magnitude of the effect, we can be sure that the significance of the effect is not sensitive
# to the choice of adjusters.


