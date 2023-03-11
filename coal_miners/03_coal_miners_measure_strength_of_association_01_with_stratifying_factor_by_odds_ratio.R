setwd("//media//kswada//MyFiles//R//coal_miners")

packages <- c("dplyr", "vcd", "MASS", "datasets")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Coal Miners
# ------------------------------------------------------------------------------

data("CoalMiners", package = "vcd")

data <- CoalMiners



# ----------
# Breathlessness * Wheeze * Age
dim(data)

dimnames(data)


data


# ----------
# Age 20-24 to be omitted from analysis here
dat2 <- data[,,2:9]


structable(. ~ Age, data = dat2)




# ------------------------------------------------------------------------------
# (Log) odds ratio
# ------------------------------------------------------------------------------

# log odds ratio decline with age
loddsratio(dat2)



# ----------
# odds ratio
loddsratio(dat2, log = FALSE)




# ------------------------------------------------------------------------------
# (Log) odds ratio varies with a stratifying factor (age)
# ------------------------------------------------------------------------------

# The log odds ratio varies with a stratifying factor
# together with confidence interval error bars

lor <- loddsratio(dat2)

plot(lor, bars = FALSE, baseline = FALSE, whiskers = 0.2)




# ------------------------------------------------------------------------------
# Confidence interval of odds ratio
# ------------------------------------------------------------------------------

confint(lor)




# ------------------------------------------------------------------------------
# quadratice model
# ------------------------------------------------------------------------------

# Here we try using a quadratic model (poly(age, 2)) mainly to see if the trend is nonlinear
lor_df <- as.data.frame(lor)

lor_df


age <- seq(25, 60, by = 5) + 2

lmod <- lm(LOR ~ poly(age, 2), weights = 1 / ASE^2, data = lor_df)

grid.lines(seq_along(age), fitted(lmod), gp = gpar(col = "red", lwd = 2), default.units = "native")



# -->
# It appears that the decline in the log odds ratio levels off with increasing age.




# ----------
# But the test of additional contribution of the quadratic term turns out to be insignificant

summary(lmod)


