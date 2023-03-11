setwd("//media//kswada//MyFiles//R//chredlin")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  chredlin
# ------------------------------------------------------------------------------

data("chredlin", package = "faraway")

str(chredlin)




# ------------------------------------------------------------------------------
# Leave-one-out change in coefficient values for beta:
# The differences in the coefficient caused by the removal of one point
# ------------------------------------------------------------------------------

lmod <- lm(involact ~ race + fire + theft + age + log(income), data = chredlin)


# ----------
lm.influence(lmod)


# lm.influence
( diags <- data.frame(lm.influence(lmod)$coef) )

# influence.measures
( diags2 <- data.frame(influence.measures(lmod)$infmat) )



# ----------
ggplot(diags, aes(row.names(diags), race)) + geom_linerange(aes(ymax = 0, ymin = race)) +
  theme(axis.text.x = element_text(angle = 90)) + xlab("ZIP") + geom_hline(yintercept = 0)


ggplot(diags2, aes(row.names(diags), dfb.race)) + geom_linerange(aes(ymax = 0, ymin = dfb.race)) +
  theme(axis.text.x = element_text(angle = 90)) + xlab("ZIP") + geom_hline(yintercept = 0)



# Scatter plot of leave-one-out change by both of fire and theft
ggplot(diags, aes(fire, theft)) + geom_text(label = row.names(diags))



# ----------
plot(lmod, 5)



# ----------
car::influencePlot(lmod)



# ------------------------------------------------------------------------------
# See what happens when we exclude outliers
# ------------------------------------------------------------------------------
chredlin[c("60607", "60610"),]


# -->
# These are high theft and fire zip codes.



# ----------
( tmp <- match(c("60607", "60610"), row.names(chredlin)) )

lmode <- lm(involact ~ race + fire + theft + age + log(income), chredlin, subset = -c(tmp))

summary(lmode)


# -->
# theft and age are no longer significant at the 5% level.
# The coefficient for race is reduced compared to the full data fit but remains statistically significant.
# So we have verified that our conclusions are also robust to the exclusion of one or perhaps two cases from the data.



# ------------------------------------------------------------------------------
# Removing outliers and "theft" variable
# ------------------------------------------------------------------------------

modalt <- lm(involact ~ race + fire + log(income) + age, data = chredlin, subset = -c(tmp))

summary(modalt)


# -->
# In this model, race no longer meets the threshold for significance....  --> model uncertainty



# ----------
summary(lm(involact ~ race + fire + theft + age, subset = (side == "s"), data = chredlin))

summary(lm(involact ~ race + fire + theft + age, subset = (side == "n"), data = chredlin))



# -->
# We see that race is significant in the north, but not in the south.
# By dividing the data into smaller and smaller subsets it is possible to dilute the significance of any predictor.
# On the other hand, it is important not to aggregate all data without regard to whether it is reasonable.


