setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\hurricanes")

packages <- c("dplyr")

purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Hurricanes
#   - In 2014, a paper was published that was entitled "Female hurricanes are deadlier than male hurricanes".
#     As the title suggests, the paper claimed that hurricanes with female names have caused greater loss of life, and the explanation
#     given is that people unconsciously rate female hurricanes as less dangerous and so are less likely to evacuate.
#   - Statisticians severely criticized the paper after publication.
#     Here consider the hypothesis that hurricanes with female names are deadlier.
#   - Variables
#        - damage_norm:  Normlized estimate of damage in dollars
#        - min_pressure:  Minimum pressure, a measure of storm strength, low is stronger
# ------------------------------------------------------------------------------

# data("Hurricanes", package = "rethinking")

data <- read.csv(file = "Hurricanes.txt", header = T, sep = "\t")


dim(data)


str(data)


car::some(data)




# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

