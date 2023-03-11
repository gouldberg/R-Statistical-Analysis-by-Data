# setwd("//media//kswada//MyFiles//R//email")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\email")


packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  email
#   - The dataset on randomized controlled trial of mail marketing to users for electronic commerce web site
#   - Variables
#       - recency:  months from last buy
#       - history_segment:  segment for purchase amount in previous year
#       - history:  purchase amount at previous year
#       - mens:  whether or not purchased items for mens in previous year
#       - womens:  whether or not purchased items for womens in previous year
#       - zipcode:  region segment based on zipcode
#       - newbie:  whether or not become newly user in last 12 months
#       - channel:  from which channel purchased in previous year
#       - segment:  which type of mails are sent
#       - visit:  whether or not visited the web site within 2 weeks since the mails were sent
#       - conversion:  whether or not purchased within 2 weeks since the mails were sent
#       - spend:  purchase amount
# ------------------------------------------------------------------------------

email <- read_csv("Kevin_Hillstrom_MineThatData_E-MailAnalytics_DataMiningChallenge_2008.03.20.csv")

str(email)

dim(email)


car::some(email)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
