setwd("//media//kswada//MyFiles//R//orings")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  orings
# ------------------------------------------------------------------------------

data("orings", package = "faraway")

str(orings)


dim(orings)


car::some(orings)



# ------------------------------------------------------------------------------
# Logit, Probit, Complementary log-log model
# ------------------------------------------------------------------------------

library(broom)


# logit model
result2 <- glm(cbind(damage, 6 - damage) ~ temp, family = binomial, orings)

tmp2 <- tidy(result2)


# probit model
result3 <- glm(cbind(damage, 6 - damage) ~ temp, family = binomial(link = probit), orings)

tmp3 <- tidy(result3)


# Complementary log-log model
result4 <- glm(cbind(damage, 6 - damage) ~ temp, family = binomial(link = cloglog), data)

tmp4 <- tidy(result4)



# ----------
summary(result2)

summary(result3)

summary(result4)



# ----------
stargazer::stargazer(result2, result3, result4, intercept.bottom = FALSE, type = "text")



# ----------
graphics.off()

par(mfrow = c(1,1))

plot(damage / 6 ~ temp, orings, xlim=c(25,85), ylim=c(0,1), xlab="Temperature", ylab="Prob of Damage")

x <- seq(25, 85, 1)

lines(x, exp(tmp2$estimate[1] + tmp$estimate[2] * x) / (1 + exp(tmp$estimate[1] + tmp$estimate[2] * x)), lty=2, col="blue")

lines(x, pnorm(tmp3$estimate[1] + tmp3$estimate[2] * x), lty=2, col="red")

lines(x, 1 - exp((-exp(tmp4$estimate[1] + tmp4$estimate[2] * x))), lty=2, col="orange")



# ----------
AIC(result2, result3, result4)

deviance(result2);  deviance(result3);  deviance(result4)


