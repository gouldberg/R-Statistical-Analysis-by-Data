setwd("//media//kswada//MyFiles//R//vietnam")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Vietnam
# ------------------------------------------------------------------------------

data("Vietnam", package = "vcdExtra")

dim(Vietnam)
str(Vietnam)


car::some(Vietnam)



# frequency form in dataframe to table
( vietnam.tab <- xtabs(Freq ~ sex + year + response, data = Vietnam) )



# ----------
# table form to case form
expand.dft(vietnam.tab)



# ------------------------------------------------------------------------------
# spline plot:  a stacked barchart of the row percentages
# doubledecker
# ------------------------------------------------------------------------------
ftab <- ftable(vietnam.tab, row.vars = c("year"), col.vars = c("response"))
dimnames(ftab) <- list(year = unique(Vietnam$year), response = unique(Vietnam$response))
spineplot(ftab)


ftab <- ftable(vietnam.tab, row.vars = c("sex"), col.vars = c("response"))
dimnames(ftab) <- list(year = unique(Vietnam$sex), response = unique(Vietnam$response))
spineplot(ftab)


# doubledecker(ftab)

