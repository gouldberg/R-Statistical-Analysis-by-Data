setwd("//media//kswada//MyFiles//R//trolley")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Trolley
#   - This data come from a series of experiments conducted by philosophers.
#     In this case, the experiments aim to collect empirical evidence relevant to debates about moral intuition,
#     the forms of reasoning through which people develop judgments about the moral goodness and badness of actions.
#     These debates are relevant to all of the social sciences, because they touch on broader issues of reasoning, the role of emotions in decision making,
#     and theories of moral development, both in individuals and groups.
#     (see, "Statistical rethinking", p. 332-333)
#   - There are 12 columns and 9930 rows, comprising data for 331 unique individuals.
#     outcome:  "response", which is an integer from 1 to 7 indicating how morally permissible the participabnt found the action to be taken (or not) in the story.
#
#   - Previous research has leadto at least 3 important principles of unconscious reasoning that may explain variations in judgment.
#       - action principle:  Harm caused by action is morally worse than equivalent harm caused by omission
#       - intention principle:  Harm intended as the means to a goal is morally worse than equivalent harm foreseen as the side effect of a goal
#       - contact principle:  Using physical contact to cause harm to a victim is morally worse than causing equivalent harm to a victim without using physical contact
# ------------------------------------------------------------------------------
data("Trolley", package = "rethinking")

d <- Trolley

dim(d)

str(d)

table(d$action)



# ------------------------------------------------------------------------------
# Overall distribution and cumulative proportion and log-odds
# ------------------------------------------------------------------------------
# simle histogram
simplehist(d$response, xlim = c(1,7), xlab = "response")



# ----------
# cumulative proportion of each response values
pr_k <- table(d$response) / nrow(d)

cum_pr_k <- cumsum(pr_k)

plot(1:7, cum_pr_k, type = "b", xlab = "response", ylab = "cumulative proportion", ylim = c(0,1))



# ----------
# logarithm of cumulative odds of each response values
# Note that the log-cumulative-odds of response value 7 is infinity, so it is not shown
logit <- function(x) log(x / (1 - x))

lco <- logit(cum_pr_k)

plot(1:7, lco, type = "b", xlab = "response", ylab = "cumulative proportion", ylim = c(-2,2))


