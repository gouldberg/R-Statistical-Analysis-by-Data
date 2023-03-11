setwd("//media//kswada//MyFiles//R//crime2")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  crime2
#  - balanced panel of 46 cities, properly sorted, which is discused by Wooldridge (2016, Secion 13.3)
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



# ------------------------------------------------------------------------------
# Regression table with standard SE and clustered SE
# ------------------------------------------------------------------------------

# regression table with standard SE
coeftest(reg)


# regression table with clustered standard errors and respective t statistics in addition to the usual standard errors.
coeftest(reg_fd, vcovHC)

coeftest(reg_fd, vcov = vcovHC(reg_fd), type = "HCO", cluster = "county")



# -->
# standard errors are different.

