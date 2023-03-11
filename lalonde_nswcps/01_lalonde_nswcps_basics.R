# setwd("//media//kswada//MyFiles//R//lalonde_nswcps")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\lalonde_nswcps")

packages <- c("dplyr", "tidyverse", "MatchIt", "Matching", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data
#   - NSW (National Supported Work) 
#   - CPS (Current Population Survey) :  survey data collected outside of this survey
#   - National Supported Work (NSW) is the program during 1970 to give counseling and short-term (9 - 18 months) experience of working to people who can not work
#   - The variables in this dataset include
#        - participation in the job training program (treat, which is equal to 1 if participatedin the program, and 0 otherwise), 
#          age (age), years of education (educ), race (blackwhich isequal to 1 if black, and 0 otherwise; hispanwhich is equal to 1 if hispanic, and 0 otherwise),
#          marital status (married, which is equal to 1 if married, 0 otherwise), high school degree (nodegree, which is equal to 1 if no degree, 0 otherwise),
#          1974 real earnings (re74), 1975 realearnings (re75), and the main outcome variable, 1978 real earnings (re78).
# ------------------------------------------------------------------------------

# ----------
# Read from NBER archive

# library(haven)
# cps1_data <- read_dta("https://users.nber.org/~rdehejia/data/cps_controls.dta")
# cps3_data <- read_dta("https://users.nber.org/~rdehejia/data/cps_controls3.dta")
# nswdw_data <- read_dta("https://users.nber.org/~rdehejia/data/nsw_dw.dta")

# write_tsv(cps1_data, path = "cps1_data.txt", quote_escape = FALSE)
# write_tsv(cps3_data, path = "cps3_data.txt", quote_escape = FALSE)
# write_tsv(nswdw_data, path = "nswdw_data.txt", quote_escape = FALSE)



# ----------
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



