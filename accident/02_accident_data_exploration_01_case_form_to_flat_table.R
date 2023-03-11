setwd("//media//kswada//MyFiles//R//accident")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Accident
# ------------------------------------------------------------------------------

data("Accident", package = "vcdExtra")

dim(Accident)

str(Accident)


car::some(Accident)



# ------------------------------------------------------------------------------
# Overview by table
# ------------------------------------------------------------------------------

# frequency form in dataframe to table
( acci.tab <- xtabs(Freq ~ age + result + mode + gender, data = Accident) )



# ----------
# 3 dimension xtabs table to 2 dimension ftable
ftable(acci.tab, row.vars = c("age"), col.vars = c("result"))

round(prop.table(ftable(acci.tab, row.vars = c("age"), col.vars = c("result")), 1), digits = 3)


ftable(acci.tab, row.vars = c("mode"), col.vars = c("result"))

round(prop.table(ftable(acci.tab, row.vars = c("mode"), col.vars = c("result")), 1), digits = 3)


ftable(acci.tab, row.vars = c("gender"), col.vars = c("result"))

round(prop.table(ftable(acci.tab, row.vars = c("gender"), col.vars = c("result")), 1), digits = 3)



# ----------
# table form to case form
expand.dft(acci.tab)

