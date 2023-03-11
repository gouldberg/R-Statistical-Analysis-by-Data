setwd("//media//kswada//MyFiles//R//nes96")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  nes96
# ------------------------------------------------------------------------------

data("nes96", package = "faraway")

str(nes96)

head(nes96)



# ----------
party <- nes96$PID

levels(party)


# We collapse this to three
levels(party) <- c("Democrat", "Democrat", "Independent", "Independent", "Independent", "Republican", "Republican")


# The income variable in the original data was an ordered factor with income ranges.
# We have converted this to a numeric variable by taking the midpoint of each range.
inca <- c(1.5, 4, 6, 8, 9.5, 10.5, 11.5, 12.5, 13.5, 14.5, 16, 18.5, 21, 23.5, 27.5, 32.5, 37.5, 42.5, 47.5, 55, 67.5, 82.5, 97.5, 115)

income <- inca[unclass(nes96$income)]



# For simplicity, we consider only the age, education level and income group of the respondents
rnes96 <- data.frame(party, income, education = nes96$educ, age = nes96$age)


car::some(rnes96)



# ------------------------------------------------------------------------------
# Model Understanding
# ------------------------------------------------------------------------------

summary(pomodi)


# -->
# Odds of moving from Democrat to Independent/Republican category (or from Democrat/Independent to Republican)
# increase by a factor of exp(0.013120) = 1.0132 as income increase by one unit ($1000).


# ----------
# For an income of $0, the predicted probability of being a Democrat is
ilogit(0.0209)


# while that of being an Independent is:
ilogit(1.292) - ilogit(0.209)




# ------------------------------------------------------------------------------
# Prediction
# ------------------------------------------------------------------------------

inclevels <- seq(0, 100, by = 20)

( df <- predict(pomodi, data.frame(income = inclevels, row.names = inclevels), type = "probs") )


# -->
# Notie how the probability of being a Democrat uniformly decreases with income while that for being a Republican uniformly increases as income increass.
# But the middle category of Independent increases then decreases.


# ----------
# We can see that for all income levels, the difference of logit is same.
logit(df[,"Democrat"]) - logit(df[,"Democrat"] + df[,"Independent"])




# ------------------------------------------------------------------------------
# Model Understanding: Latent variable interpretation
# ------------------------------------------------------------------------------

summary(pomodi)


x <- seq(-4, 4, by = 0.05)

par(mfrow=c(1,1))
plot(x, dlogis(x), type = "l")
abline(v = c(0.209, 1.292))
abline(v = c(0.209, 1.292) - 50*0.013120, lty = 2)
abline(v = c(0.209, 1.292) - 100*0.013120, lty = 5)


# -->
# Solid line:  an income of $50
# Dotted line: an income of $50
# Dashed line: an income of $100

# The probability of being a Democrat is given by the area lying to the left of the leftmost of each pair of line.
# The probability of being a Republican is given by the area lying to the right of the rightmost of each pair of line.
# Independents are represented by the area in-between.


