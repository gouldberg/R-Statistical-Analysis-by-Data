setwd("//media//kswada//MyFiles//R//uksoccer")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  UKSoccer
#  - The distribution of goals scored by the 20 teams in the 1995/96 season of the Premier League of the UK Footboll Association
#   as presented originally by Lee (1997).
#   Over a season each team plays each other team exactly once, so there are a total of 20 * 19 = 380 games.
#   Because there may be an advantage for the home team, the goals scored have been classified as "home team" goals and "away team" goals in the table.
# ------------------------------------------------------------------------------
data("UKSoccer", package = "vcd")

data <- UKSoccer

data
sum(data)

str(data)



# ------------------------------------------------------------------------------
# Convert table to dataframe
# ------------------------------------------------------------------------------
data_df <- as.data.frame(data, stringsAsFactors = FALSE)

data_df <- within(data_df, {
  Home <- as.numeric(Home)
  Away <- as.numeric(Away)
  Total <- Home + Away
})

str(data_df)

head(data_df)



# ------------------------------------------------------------------------------
# Calculate the mean and variance of these variables
# ------------------------------------------------------------------------------
# expand to ungrouped form
data_df2 <- expand.dft(data_df)
head(data_df2)


apply(data_df2, MARGIN=2, FUN=function(x) c(mean = mean(x), var = var(x)))


# -->
# means are all approximately equal to the corresponding variances.
# More to the point, the variance of the Total score is approximately equal to the sum of the individual variances.

# Note also there does appear to be an advantage for the home mean, of nearly half a goal



