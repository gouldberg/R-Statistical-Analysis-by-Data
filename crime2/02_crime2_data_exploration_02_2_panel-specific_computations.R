setwd("//media//kswada//MyFiles//R//crime2")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  crime2
# ------------------------------------------------------------------------------

# library(foreign)
# crime2 <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/crime2.dta")
# write.table(crime2, file = "crime2.txt", row.names = F, quote = F, sep = "\t")

# library(foreign)
# crime4 <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/crime4.dta")
# write.table(crime4, file = "crime4.txt", row.names = F, quote = F, sep = "\t")


crime2 <- read.table("crime2.txt", header = T, stringsAsFactors = FALSE, sep = "\t")

crime4 <- read.table("crime4.txt", header = T, stringsAsFactors = FALSE, sep = "\t")


str(crime2)
str(crime4)

dim(crime2)
dim(crime4)

car::some(crime2)
car::some(crime4)



# ----------
crime2.p <- pdata.frame(crime2, index = 46)



# index = c("cross-sectional unit", "time")
# If we have a balanced panel, and the observations are first sorted by cross-sectional unit and then by year

crime4.p <- pdata.frame(crime4, index = c("county", "year"))





# ------------------------------------------------------------------------------
# Panel-specific computations
# ------------------------------------------------------------------------------

# Calculations within the pdata.frame for crmrte (crime rate)

# Lag
crime4.p$cr.1 <- lag(crime4.p$crmrte)


# Difference
crime4.p$cr.d <- diff(crime4.p$crmrte)


# Between transformation (length n, across unit by time):  individual average
crime4.p$cr.b <- between(crime4.p$crmrte)


# Between transformation (length N)
# the average crmrte within the first 7 rows is given as the first s7 values of cr.B
crime4.p$cr.B <- Between(crime4.p$crmrte)


# Within transformation (demeaning)
# = crmrte - cr.B
crime4.p$cr.W <- Within(crime4.p$crmrte)



# ----------
crime4.p[1:16, c("county", "year", "crmrte", "cr.1", "cr.d", "cr.b", "cr.B", "cr.W")]



