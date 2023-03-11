setwd("//media//kswada//MyFiles//R//bangladesh")

packages <- c("dplyr", "rethinking", "lattice", "tidyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  bangladesh
#   - The data come from the 1988 Bangladesh Fertility Survey.
#     In 1980, typical Bengali woman could have 5 or more children in her lifetime, By the year 2000, a typical Bengali woman had only 2 or 3.
#     Contraception was widely available but many families chose not to use it.
#   - Each row is one of 1934 women.
#     There are six variables, but we focus on 3 of them here.
#       - district:  ID number of administrative district each woman resided in 
#       - use.contraception:  An indicator (0/1) of whether the woman was using contraception
#       - urban:  An indicator (0/1) of whether the woman lived in a city, as opposed to living in a rural area
# ------------------------------------------------------------------------------
data("bangladesh", package = "rethinking")

d <- bangladesh

dim(d)

str(d)



# ----------
# District 54 is absent
sort(unique(d$district))


d$district_id <- as.integer(as.factor(d$district))
sort(unique(d$district_id))



# ------------------------------------------------------------------------------
# use.contraception by district
# ------------------------------------------------------------------------------
table(d$use.contraception, useNA="always")

par(mfrow=c(1,1))
d_sum <- d %>% as.tbl %>% group_by(district_id) %>% summarize(p_use_contraception=mean(use.contraception))
plot(d_sum$district_id, d_sum$p_use_contraception, pch=16, col = "black", xlab = "district_id", ylab = "contraception use ratio")
abline(h = sum(d$use.contraception) / nrow(d), lty=2, col = "gray")

# -->
# district 3:  the number of use.contraception == 0 is zero
# district 11, 49:  the number of use.contraception == 1 is zero