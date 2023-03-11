# setwd("//media//kswada//MyFiles//R//mpls")
setwd("//media//kswada//MyFiles//R//Mixed_effects_and_multilevel_model//mpls")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This examples are based on "Longitudinal Data Analaysis for the Behavioral Sciences Using R"


# ------------------------------------------------------------------------------
# data:  MPLS
#   - Sample data from Minneapolis School District (MPLS)
# ------------------------------------------------------------------------------

MPLS.LS <- read.table("MPLS.LS.txt", header = T)


str(MPLS.LS)


dim(MPLS.LS)


car::some(MPLS.LS)



# ----------
MPLS.LS$grade5 <- MPLS.LS$grade - 5

MPLS.LS <- MPLS.LS %>% mutate(eth2 = ifelse(eth == "Whi", "W", "Other"))



# ------------------------------------------------------------------------------
# compare models by AICc and effect size measures
# ------------------------------------------------------------------------------

library(lmd4)
library(AICcmodavg)


model.1 <- lmer(read ~ grade5 + risk2 + (grade5 | subid), MPLS.LS, REML = FALSE)

model.2 <- lmer(read ~ grade5 + eth + (grade5 | subid), MPLS.LS, REML = FALSE)

model.3 <- lmer(read ~ grade5 + risk2 + eth + (grade5 | subid), MPLS.LS, REML = FALSE)

model.4 <- lmer(read ~ grade5 * risk2 + (grade5 | subid), MPLS.LS, REML = FALSE)

model.5 <- lmer(read ~ grade5 * eth + (grade5 | subid), MPLS.LS, REML = FALSE)

model.6 <- lmer(read ~ grade5 * risk2 + grade5 * eth + (grade5 | subid), MPLS.LS, REML = FALSE)




# ----------
mynames <- paste0("M", as.character(1:6), sep = "")

mymodels <- list(model.1, model.2, model.3, model.4, model.5, model.6)

myaicc <- aictab(cand.set = mymodels,
                 modnames = mynames, sort = FALSE, second.ord = TRUE)


myaicc2 <- as.data.frame(myaicc) %>% arrange(AICc) %>% mutate(Cum.Wt = cumsum(AICcWt), Eratio = max(AICcWt) / AICcWt)

print(myaicc2, digits = 3)



# ------------------------------------------------------------------------------
# Bar graphs of the results
# ------------------------------------------------------------------------------

library(ggplot2)

myx <- scale_x_continuous(breaks = 5:8)

theme_set(theme_bw())


# Weight of evidence
g1 <- ggplot(myaicc2, aes(x = Modnames, y = AICcWt)) + ylab("Weight") +
  geom_bar(stat = "identity", fill = "grey80", colour = "black") + xlab("Model") +
  scale_y_continuous(limits = c(0,1))


print(g1)



# ----------
# Evidence ratio
g2 <- ggplot(myaicc2, aes(x = Modnames, y = Eratio)) + ylab("Ratio") + 
  geom_bar(stat = "identity", fill = "grey80", colour = "black") + xlab("Model") +
  geom_hline(yintercept = 1, linetype = 2)


print(g2)



# ------------------------------------------------------------------------------
# Confidence set of best model
# ------------------------------------------------------------------------------

# 0.99 is very high level of confidence, and any models outside the set would certainly be considered implausible

confset(cand.set = mymodels, modnames = mynames, level = 0.99)

confset(cand.set = mymodels, modnames = mynames, level = 0.95)



# ------------------------------------------------------------------------------
# Details of models:  confidence interval of fixed effects coefficients
# ------------------------------------------------------------------------------

summary(model.1)

summary(model.2)

summary(model.3)



# ----------
# currently we select model.1 (assumed)
mytab <- as.data.frame(summary(model.1)$coefficients)

mytab


mytab$LCI <- mytab$Estimate - 2 * mytab$'Std. Error'

mytab$UCI <- mytab$Estimate + 2 * mytab$'Std. Error'


print(mytab)



# ----------
# slightly differnt results
confint(model.1)



# ------------------------------------------------------------------------------
# Details of models:  fitted values
# ------------------------------------------------------------------------------

( plotdata <- model.1@frame )


# fitted values
plotdata$pred <- model.matrix(model.1) %*% fixef(model.1)


plotdata$grade <- plotdata$grade + 5



# ----------
myx <- scale_x_continuous(breaks = 5:8)

g1 <- ggplot(plotdata, aes(x = grade, y = read, linetype = risk2)) +
  stat_summary(fun.y = "mean", geom = "point", cex = 2) +
  stat_summary(aes(y = pred), fun.y = "mean", geom = "line") +
  myx + theme(legend.position = c(0.54, 0.1), legend.title = element_blank())


print(g1)

  

# ------------------------------------------------------------------------------
# Details of models:  main effect plot
# ------------------------------------------------------------------------------

library(effects)

plot(predictorEffects(model.1))

plot(predictorEffects(model.1, ~grade5))



# ------------------------------------------------------------------------------
# Details of models:  conditional effect plot
# ------------------------------------------------------------------------------

# effect plots for several predictors jointly or full-model plots

plot(Effect(c("grade5", "risk2"), model.1), 
     confint = list(style = "bands"),
     lines = list(multiline = TRUE, lty = c(5,4,3,2,1), col = c(gray(0.8), gray(0.6), gray(0.4), gray(0.2), "black")))



# ------------------------------------------------------------------------------
# Post-hoc analysis
# ------------------------------------------------------------------------------

# two additional models, one with intercept effects ofr risk, ethnicity, and gender
# second with intercept and slope effects for the same 3 predictors
model.7 <- lmer(read ~ grade5 + risk2 + eth + gen + (grade5 | subid), MPLS.LS, REML = FALSE)


model.8 <- lmer(read ~ grade5 * risk2 + grade5 * eth + grade5 * gen + (grade5 | subid), MPLS.LS, REML = FALSE)



# ----------
mynames <- paste0("M", as.character(1:8), sep = "")

mymodels <- list(model.1, model.2, model.3, model.4, model.5, model.6, model.7, model.8)


myaicc <- aictab(cand.set = mymodels,
                 modnames = mynames, sort = FALSE, second.ord = TRUE)


myaicc2 <- as.data.frame(myaicc) %>% arrange(AICc) %>% mutate(Cum.Wt = cumsum(AICcWt), Eratio = max(AICcWt) / AICcWt)

print(myaicc2, digits = 3)



# -->
# Model 7 and 8 are post hoc models, and their weights of evidence and evidence ratios should be taken less seriously than those of the predata models.
# "Less seriously" means that the values of the post hoc models might be somehow informally downgraded.

# Model 2 is still the best fitting, but its weight of evidence is a bit lower compared to omitting the additional models.

# The judgment of Model 7 is more difficult, as it is the 4th best-fitting model.



# ----------
# 0.99 is very high level of confidence, and any models outside the set would certainly be considered implausible

confset(cand.set = mymodels, modnames = mynames, level = 0.99)

confset(cand.set = mymodels, modnames = mynames, level = 0.95)



# -->
# Since Model 7 is a post hoc model and its evidence ratio is relatively large,
# it appears to be a relatively implausible candidate for the best approximating model.
# The introduction of the two post hoc models does not change the interpretation of the overall result in this example

