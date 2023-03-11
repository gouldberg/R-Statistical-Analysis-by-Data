setwd("//media//kswada//MyFiles//R//ucb_admissions")

packages <- c("dplyr", "vcd", "MASS", "datasets")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  UCBAdmissions
# ------------------------------------------------------------------------------

data("UCBAdmissions", package = "datasets")


data <- UCBAdmissions


data


dimnames(data)


dim(data)




# ----------
# transform from matrix to aggregated data

library(tidyr)

d <- data.frame(data) %>% spread(., key = "Admit", value = "Freq") %>% dplyr::select(Dept, Gender, Admitted, Rejected) %>% mutate(applications = Admitted + Rejected)

colnames(d) <- c("dept", "applicant_gender", "admit", "reject", "applications")


d$male <- ifelse(d$applicant_gender == "Male", 1, 0)

d <- d %>% arrange(dept, applicant_gender)

d <- rapply(d, f = as.integer, classes = "numeric", how = "replace")



# ----------
str(d)

car::some(d)




# ------------------------------------------------------------------------------
# posterior validation check
# ------------------------------------------------------------------------------

# plot posterior predictions for the model

# postcheck() : default posterior validation check function

postcheck(mod1, n = 1e4)


for(i in 1:6){
  x <- 1 + 2 * (i - 1)
  y1 <- d$admit[x] / d$applications[x]
  y2 <- d$admit[x+1] / d$applications[x+1]
  lines(c(x, x+1), c(y1, y2), col = rangi2, lwd = 2)
  text(x + 0.5, (y1 + y2) / 2 + 0.05, d$dept[x], cex = 0.8, col = rangi2)
}




# -->
# Blue points: observed proportions admitted for each row in the data, with points from the same department connected by a blue line
# Open points: the tiny vertical black lines within them, and the crosses are expected proportions, 89% intervals of the expection, and 89% interval of simulated samples, respectively

# There are only two departments in which females had a lower rate of admittion than males (C and E)

# But you can see the steady decline in admission probability for both males and females from deparment A to deparment F.
# Females in these data tended not to apply to departments like A nad B, which had high overall admission rate.
# Instead they applied in large numbers to departments like F, which admitted less than 10% of applicants.

# So the right question is
#  - What is the average difference in probability of admission between females and maels within deparments ?

