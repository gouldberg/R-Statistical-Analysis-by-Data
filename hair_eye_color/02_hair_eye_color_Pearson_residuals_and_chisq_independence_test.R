
packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Hair Eye Color
# ------------------------------------------------------------------------------

data(HairEyeColor, package = "datasets")


str(HairEyeColor)


HairEyeColor




# ------------------------------------------------------------------------------
# Pearson residuals
# ------------------------------------------------------------------------------


# Pearson residuals for hair

expected <- rep(sum(hair) / 4, 4)


names(expected) <- names(hair)


# Pearson residuals

residuals <- (hair - expected) / sqrt(expected)

residuals




# ----------
# Pearson residuals for haireye by independence_table

exp <- independence_table(haireye)

resids <- (haireye - exp) / sqrt(exp)

round(resids, 2)




# ------------------------------------------------------------------------------
# Pearson Chisq statistic and test
# ------------------------------------------------------------------------------

( chisq <- sum(resids ^ 2) )


( df <- prod(dim(haireye) - 1) )


pchisq(chisq, df, lower.tail = FALSE)




# ----------
chisq.test(haireye)


chisq

df



# ----------
# Pearson residuals for haireye by chisq.test and residuals

round(residuals(chisq.test(haireye)), 2)


