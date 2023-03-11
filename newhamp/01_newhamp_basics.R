
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\newhamp")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  newhamp
#   - On the 8the January 2008, primaries to select US presidential candidates were held in New Hampshire.
#     In the Democratic party primary, Hillary Clinton defeated Barack Obama contrary to the expectations of pre-election
#     opinion polls.
#     Two different voting technologies were used in New Hampshire.
#     Some wards (administrative districts) used paper ballots, counted by hand while others used optically scanned ballots,
#     counted by machine.
#     Among the paper ballots, Obama had more votes than Clinton while Clinton defeated Obama on just the machine-counted ballots.
#     Since the method of voting should make no causal difference to the outcome, suspicions were raised regarding
#     the integrity of the election.
#   - Variables:
#       - votesys:  'D' for digital or 'H' for hand.
#       - pObama:  proportion voting for Obama in each ward
# ------------------------------------------------------------------------------


dat <- read.csv("newhamp.txt", header = TRUE, sep = "\t")


str(dat)


car::some(dat)




# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------


summary(dat)




# ----------

dat <- dat %>% mutate(OC = ifelse(Obama > Clinton, "O", ifelse(Clinton > Obama, "C", "E")))


table(dat$OC, useNA = "always")




# ----------

xtabs(~ votesys + OC, data = dat)


round(prop.table(xtabs(~ votesys + OC, data = dat), 1), digits = 3)



# -->
# If votesys = 'D', Clinton defeated Obama by 66.1% vs. 33.9%
# If votesys = 'H', Obama defeated Clinton by 66.7% vs. 31.4%



