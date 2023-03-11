# setwd("//media//kswada//MyFiles//R//lalonde_nswcps")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\lalonde_nswcps")

packages <- c("dplyr", "tidyverse", "MatchIt", "Matching", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data
# ------------------------------------------------------------------------------

# CPS1:  not working people ONLY with treated + people from other source (controlled)  (biased data)
cps1_data <- read_tsv("cps1_data.txt")


# CPS3:  not working people ONLY with treated + people from other source (controlled) but not working at 1976 spring  (biased data)
cps3_data <- read_tsv("cps3_data.txt")


# NSW:  not working people BOTH with controlled and treated  (RCT data)
nswdw_data <- read_tsv("nswdw_data.txt")



# 15992 * 11
str(cps1_data)
dim(cps1_data)


# 429 * 11
str(cps3_data)
dim(cps3_data)


# 445 * 11
str(nswdw_data)
dim(nswdw_data)



# ----------
# cps1 + nsw (only treated)
cps1_nsw_data <- nswdw_data %>% filter(treat == 1) %>% rbind(cps1_data)



# ----------
# cps3 + nsw (only treated)
cps3_nsw_data <- nswdw_data %>% filter(treat == 1) %>% rbind(cps3_data)




# ------------------------------------------------------------------------------
# data exploration:  black
# ------------------------------------------------------------------------------

xtabs(~ black + treat, data = nswdw_data)

xtabs(~ black + treat, data = cps1_data)

xtabs(~ black + treat, data = cps3_data)



# ----------
# re74 distribution of black

with(nswdw_data, by(re74, black, summary))


with(cps1_data, by(re74, black, summary))


with(cps3_data, by(re74, black, summary))





# ------------------------------------------------------------------------------
# data exploration:  hispanic
# ------------------------------------------------------------------------------

xtabs(~ hispanic + treat, data = nswdw_data)

xtabs(~ hispanic + treat, data = cps1_data)

xtabs(~ hispanic + treat, data = cps3_data)



# -->
# Note that from nswdw_data to cps3_data,
# black with treat 0 is decreased from 215 to 87
# on the other hand, hispanic is increased from 28 to 61





# ----------
# re74 distribution of black

with(nswdw_data, by(re74, hispanic, summary))


with(cps1_data, by(re74, hispanic, summary))


with(cps3_data, by(re74, hispanic, summary))

