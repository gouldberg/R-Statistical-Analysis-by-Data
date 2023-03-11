setwd("//media//kswada//MyFiles//R//coal_miners")

packages <- c("dplyr", "vcd", "MASS", "datasets")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Coal Miners
# ------------------------------------------------------------------------------

data("CoalMiners", package = "vcd")

data <- CoalMiners



# ----------
# Breathlessness * Wheeze * Age
dim(data)

dimnames(data)


data


# ----------
# Age 20-24 to be omitted from analysis here
dat2 <- data[,,2:9]


structable(. ~ Age, data = dat2)




# ------------------------------------------------------------------------------
# Fourfold display:  2 * 2
# ------------------------------------------------------------------------------

# unstandardized
# the area of each quadrant is proportional to the cell frequency, shown in numerically in each corner.
# The oods ratio is proportional to the product of the areas shaded dark, divided by the product of the areas shaded light
# here the no interpretation for confidence bands.

fourfold(margin.table(dat2, c(1,2)), std = "ind.max")




# ------------------------------------------------------------------------------
# Fourfold display:  2 * 2 * category
# ------------------------------------------------------------------------------


# unstandardized
# fourfold(dat2, std = "ind.max", fontsize = 30, mfcol = c(2,4))
fourfold(dat2, std = "ind.max", fontsize = 30)




# ----------
# standardize both margin
# confidence bands have interpretation as a test of H0: odds ratio = 1

fourfold(dat2, fontsize = 30)

cotabplot(~ Breathlessness + Wheeze | Age, data = dat2, panel = cotab_fourfold,
          text_gp = gpar(fontsize = 15))



# -->
# Although the panels for all age groups show an overwhelmingly positive association between Wheeze and Breathlessness,
# one can also (by looking carefully) see that the strength of this association declines with increasing age.

# However, note that the pattern of change over age is somewhat subtle
# compared to the dominant positive association within each panel.

