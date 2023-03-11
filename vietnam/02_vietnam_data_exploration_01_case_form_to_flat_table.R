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



# ------------------------------------------------------------------------------
# Overview by table
# ------------------------------------------------------------------------------

# frequency form in dataframe to table
( vietnam.tab <- xtabs(Freq ~ sex + year + response, data = Vietnam) )



# ----------
# 3 dimension xtabs table to 2 dimension ftable
ftable(vietnam.tab, row.vars = c("sex"), col.vars = c("response"))

round(prop.table(ftable(vietnam.tab, row.vars = c("sex"), col.vars = c("response")), 1), digits = 3)


ftable(vietnam.tab, row.vars = c("year"), col.vars = c("response"))

round(prop.table(ftable(vietnam.tab, row.vars = c("year"), col.vars = c("response")), 1), digits = 3)


ftable(vietnam.tab, row.vars = c("sex","year"), col.vars = c("response"))

round(prop.table(ftable(vietnam.tab, row.vars = c("sex","year"), col.vars = c("response")), 1), digits = 3)



# ----------
# table form to case form
expand.dft(vietnam.tab)


