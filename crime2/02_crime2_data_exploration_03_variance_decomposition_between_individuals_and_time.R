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
# Variance decomposition between individuals and time
# ------------------------------------------------------------------------------

# Variance decomposition:  "crmrte" is dependent variable in this example
summary(crime2.p$crmrte)

summary(crime2.p$unem)

summary(crime4.p$crmrte)



# -->
# The variation is mainly due to inter-individual (86.4%)


