setwd("//media//kswada//MyFiles//R//career_choice")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  career choice (1st case:  predictors have different values for different types of events)
#   - modeling choice of career for a number of young adults
# ------------------------------------------------------------------------------

# simulate career choices among 500 individuals
N <- 500
income <- 1:3  # expected income of each career
score <- 0.5 * income  # scores for each career, based on income

# converts scores to probabilities
p <- softmax(score[1], score[2], score[3])
p

# now simulate choice:  outcome career holds event type values, not counts
career <- rep(NA, N)
for(i in 1:N) career[i] <- sample(1:3, size = 1, prob = p)

career


# ------------------------------------------------------------------------------
# 1st case:  fit the model, using dcategorical and softmax link
# ------------------------------------------------------------------------------
# 1st case:
# dcategorical likelihood works when each value in the outcome variable, here career, contains the individual event types on each row.
# To convert all the scores to probabilities, we will use the multinomial logit link, "softmax"
# We also have to pick one of the event types ot be the reference type. We will use the first one.
# Instead of getting alinear model, that type is assigned a constant value. Then the other types get linear models that contain parameters relative to the reference type
# No intercepts in the linear model

mod1 <- map(
  alist(
    career ~ dcategorical(softmax(0, s2, s3)),
    s2 <- b * 2,  # linear model for event type 2
    s3 <- b * 3,  # linear model for event type 3
    b ~ dnorm(0, 5)
  ),
  data = list(career = career)
)


precis(mod1, corr = TRUE, digits = 3)


# ------------------------------------------------------------------------------
# data:  career choice (2nd case: parameters are distinct for each type of event)
#   - modeling choice of career for a number of young adults,
#     but this time estimate the association between each person's family income and which career he or she chooses.
#     So the predictor variable must have the same value in each linear model, for each row in the data.
# ------------------------------------------------------------------------------
N <- 100

family_income <- runif(N)

b <- (1:-1)

career2 <- rep(NA, N)

for(i in 1:N){
  score <- 0.5 * (1:3) + b * family_income[i]
  p <- softmax(score[1], score[2], score[3])
  career2[i] <- sample(1:3, size = 1, prob = p)
}


career2


# ------------------------------------------------------------------------------
# 2nd case:  fit the model, using dcategorical and softmax link
# ------------------------------------------------------------------------------

mod2 <- map(
  alist(
    career ~ dcategorical(softmax(0, s2, s3)),
    s2 <- a2 + b2 * family_income,
    s3 <- a3 + b3 * family_income,
    c(a2, a3, b2, b3) ~ dnorm(0, 5)
  ),
  data = list(career = career, family_income = family_income)
)


precis(mod2, corr = TRUE, digits = 3)
