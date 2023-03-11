# setwd("//media//kswada//MyFiles//R//cpi_exr_wage")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\stackloss")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  stackloss
#   - Brownlee's stack loss plant data
#     Operational data of a plant for the oxidation of ammonia to nitric acid.
#   - Obtained from 21 days of operation of a plant for the oxidation of ammonia (NH3) to nitric acid (HNO3).
#     The nitric oxides produced are absorbed in a countercurrent absorption tower”. 
#   - Variables
#        - Air Flow:  the rate of operation of the plant.
#        - Water Temp:  temperature of cooling water circulated through coils in the absorption tower.
#        - Acid Conc:  concentration of the acid circulating, minus 50, times 10: that is, 89 corresponds to 58.9 per cent acid.
#        - stack.loss (the dependent variable) is 10 times the percentage of the ingoing ammonia to the plant
#          that escapes from the absorption column unabsorbed;
#          that is, an (inverse) measure of the over-all efficiency of the plant.
# ------------------------------------------------------------------------------

dat <- read.csv("stackloss.txt", header = TRUE, colClasses = "numeric", sep = "\t")


str(dat)


car::some(dat)




# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

