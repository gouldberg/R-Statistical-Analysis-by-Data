setwd("//media//kswada//MyFiles//R//swedish_mortality")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Swedish Mortality
#   - The data taken from census records in Sweden:  http://www.mortality.org/
#     For example, the Swedish mortality data can be found at   http://www.mortality.org/cgi-bin/hmd/country.php?cntr=SWE&level=1)
#     The data are the number of deaths at each age for women born in each year from 1751 to 1914 and for ages 0 to 80.
# ------------------------------------------------------------------------------

hmd <- read.table("Mx_1x1.txt", header=T, na.strings=".", stringsAsFactors = FALSE)

str(hmd)

head(hmd)



# ----------
# Notice Age = "110+"
table(hmd$Age, useNA = "always")


# We coerce this value to numeric since we do not use "110+"
hmd$Age <- as.numeric(hmd$Age)



# ----------
# We create 2 data:
#  SwedeMat:  a dataframe object with 81 rows and 144 columns
#             containing the log hazard values for ages 0 through 80
#             and years 1751 through 1884
#  Swede1914: a vector object containing log hazard values for 1914


hmd2 <- hmd %>% filter(Year >= 1751, Year <= 1894, Age >= 0, Age <= 80) %>% mutate(Female_log = log(Female)) %>% dplyr::select(Year, Age, Female_log)

SwedeMat <- tidyr::spread(hmd2, key = Year, value = Female_log) %>% as.data.frame()

SwedeLogHazard <- SwedeMat %>% dplyr::select(-Age) %>% as.matrix(nrow = 81, ncol = 144)

str(SwedeMat)

dimnames(SwedeLogHazard)[[2]] <- paste('b', 1751:1894, sep='')



# ----------
( Swede1914 <- hmd %>% filter(Year == 1914, Age >= 0, Age <= 80) %>% dplyr::select(Female) %>% .[[1]] %>% log() )



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

tmp_data = cbind(SwedeLogHazard[, c('b1751', 'b1810', 'b1860')], Swede1914)

SwedeTime = 0:80

graphics.off()
par(mfrow = c(1,1))
matplot(SwedeTime, tmp_data,
        type='l', lwd=2, xlab='age', ylab='log Hazard', col=1, cex.lab=1.5, cex.axis=1.5)
legend("bottomright", legend = colnames(Fig10.10data), cex = 0.8)



# -->
# Log hazard rates as function of age for Swedish women born in the years 1751, 1810, 1860 and 1914.
# The hazard rate is greatest for infnts and for the very elderly, and in most years attains its minimum in the early teens.
# The four curves indicate that the hazard rate decreases substantially as the health of the population improves over this period.

# However, there are localized features in each curve that reflect various historical events such as outbreaks of disease, war, and so on.
