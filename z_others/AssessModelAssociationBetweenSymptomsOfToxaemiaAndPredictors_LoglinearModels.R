# ------------------------------------------------------------------------------
# Assess and model association between symptoms of toxaemia and predicotrs
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2", "vcd", "vcdExtra", "car", "effects", "MASS", "VGAM")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# Load Data --> Frequency Format Data
# ------------------------------------------------------------------------------
data("Toxaemia", package = "vcdExtra")

data <- Toxaemia
str(data)

describe(data)

some(data)


# ------------------------------------------------------------------------------
# Cross table
# ------------------------------------------------------------------------------
( dat.tab <- xtabs(Freq ~ class + smoke + hyper + urea, data = data) )

ftable(dat.tab, row.vars = 1)


# ------------------------------------------------------------------------------
# Examine marginal relationship between two responses and between two predictors
# ------------------------------------------------------------------------------
# smokin is relatively related to social class, with the highest levels of smokingin calsses 4 and 5 (Social class 1 is the highest in status here)
# Marginally hyper tension is somewhat more prevalent than protein urea
mosaic(~ smoke + class, data = dat.tab, shade = TRUE, main = "Predictors", legend = TRUE)
mosaic(~ hyper + urea, data = dat.tab, shade = TRUE, main = "Responses", legend = TRUE)


# ------------------------------------------------------------------------------
# Examine how the association between responses varies with reponses
# ------------------------------------------------------------------------------
# ignoring social class, interestingly, the association between hypertension and protein urea decreases with smoking
# BETTER TO USE cotabplot
# mosaic(~ hyper + urea | smoke, data = dat.tab, shade = TRUE, legend = FALSE)
cotabplot(~ hyper + urea | smoke, data = dat.tab, shade = TRUE, legend = FALSE, layout = c(2,2))


# ignoring smoking, the association is greatest in social class 3
cotabplot(~ hyper + urea | class, data = dat.tab, shade = TRUE, legend = FALSE, layout = c(2,3))


# ------------------------------------------------------------------------------
# Examine how the reponses are associated in the combination of the predictors
# ------------------------------------------------------------------------------
# Odds ratio appears to increases with both smoking nad social class number
# responses (two symptoms) are positively associated in nearly all cases
# In only two cases the odds ratio is significantly different from 1: mothers in classes 1 and 2, who smoke more than 20 cigarettes a day, but the frequency in this cell is quite small.
fourfold(aperm(dat.tab), fontsize = 16)


# plot odds ratios
( LOR <- loddsratio(urea ~ hyper | smoke + class, data = dat.tab) )

plot(LOR, confidence = FALSE, legend_pos = "bottomleft", xlab = "Smoking")
plot(t(LOR), confidence = FALSE, legend_pos = "bottomright", xlab = "Social class of mothers")


# ------------------------------------------------------------------------------
# Calculate logg odds for each responses within each predictors combination population
# Plot probability
# ------------------------------------------------------------------------------
modglm.hyper <- glm(hyper == "High" ~ class * smoke, weights =Freq, data = data, family = binomial)
modglm.urea <- glm(urea == "High" ~ class * smoke, weights =Freq, data = data, family = binomial)

summary(modglm.hyper)
summary(modglm.urea)

library(effects)

plot(allEffects(modglm.hyper), ylab="Probability (hypertension)", 
     xlab="Social class of mother", main = "Hypertension: class*smoke effect plot", 
     lwd = 3, multiline=TRUE, lty=1:3, colors = c("blue", "black", "red"), key.args = list(x = 0.05, y = 0.2, cex = 1.2, columns = 1))
     
plot(allEffects(modglm.urea), ylab="Probability (urea)", 
     xlab="Social class of mother", main = "Urea: class*smoke effect plot", 
     lwd = 3, multiline=TRUE, lty=1:3, colors = c("blue", "black", "red"), key.args = list(x = 0.65, y = 0.2, cex = 1.2, columns = 1))



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
dat.tab
( data_s <- t(matrix(aperm(dat.tab), 4, 15)) )
colnames(data_s) <- c("hu", "hU", "Hu", "HU")
rowlabs <- expand.grid(smoke = c("0", "1-19", "20+"), class = factor(1:5))
data_s <- cbind(data_s, rowlabs)

head(data_s)

# Fits a Palmgren(bivariate odds-ratio model, or bivariate logistic regression) model
# odds ratio is used as ameasure of dependency
modvglm1 <- vglm(cbind(hu, hU, Hu, HU) ~ class + smoke, binom2.or(zero = 3), data = data_s)
summary(modvglm1)

coef(modvglm1, matrix = TRUE)


# applying blogits(), we get the observed logits and log odds ratios
logits <- blogits(data_s[,4:1], add = 0.5)
colnames(logits)[1:2] <- c("logitH", "logitU")
logits <- cbind(logits, rowlabs)

head(logits)

matlines(logits[1:3], type="l", lty=1:3, lwd=1:3, pch=1:3)


# ------------------------------------------------------------------------------
# Ordinaly loglinear models
#
# ------------------------------------------------------------------------------
mod0 <- glm(Freq ~ class*smoke + hyper + urea, data = data, family = poisson)
mod1 <- glm(Freq ~ class*smoke + hyper*urea, data = data, family = poisson)
mod2 <- update(mod1, . ~ . + smoke*hyper + class*urea)
mod3 <- glm(Freq ~ (class + smoke + hyper + urea)^2, data = data, family = poisson)
mod4 <- glm(Freq ~ class*smoke*hyper + hyper*urea + class*urea, data = data, family = poisson)
mod5 <- update(mod4, . ~ . + smoke*urea)
mod6 <- update(mod4, . ~ . + class*smoke*urea)
mod7 <- update(mod6, . ~ . + smoke*hyper*urea)
mod8 <- glm(Freq ~ (class + smoke + hyper + urea)^3, data = data, family = poisson)
mod9 <- glm(Freq ~ (class + smoke + hyper + urea)^4, data = data, family = poisson)


lmtest::lrtest(mod1, mod2, mod3, mod4, mod5)


fit <- t(matrix(predict(mod2, type = "response"), 4, 15))
colnames(fit) <- c("HU", "Hu", "hU", "hu")
fit <- cbind(fit, rowlabs)
logitsfit <- blogits(fit[,1:4], add = 0.5)
colnames(logitsfit)[1:2] <- c("logitH", "logitU")
logitsfit <- cbind(logitsfit, rowlabs)

logitsfit
( matrix(logitsfit$logOR, 3, 5, dimnames = list(smoke = c("0", "1-19", "20+"), class = 1:5)) )

ggplot(logitsfit, aes(x = as.numeric(class), y = logitH, color = smoke)) + theme_bw() + geom_line(size = 1.2) +
  scale_color_manual(values = c("blue", "black", "red")) + 
  ylab("log odds (Hypertension)") + xlab("Social class of mother") + ggtitle("Hypertension") +
  theme(axis.title = element_text(size = 16)) +
  geom_point(data = logits, aes(x = as.numeric(class), y = logitH, color = smoke), size = 3) + theme(legend.position = c(0.85, 0.6))

ggplot(logitsfit, aes(x = as.numeric(class), y = logitU, color = smoke)) + theme_bw() + geom_line(size = 1.2) +
  scale_color_manual(values = c("blue", "black", "red")) + 
  ylab("log odds (Urea)") + xlab("Social class of mother") + ggtitle("Urea") +
  theme(axis.title = element_text(size = 16)) +
  geom_point(data = logits, aes(x = as.numeric(class), y = logitU, color = smoke), size = 3) + theme(legend.position = c(0.85, 0.2))

ggplot(logitsfit, aes(x = as.numeric(class), y = logOR, color = smoke)) + theme_bw() + geom_line(size = 1.2) +
  scale_color_manual(values = c("blue", "black", "red")) + 
  ylab("log odds (Urea)") + xlab("Social class of mother") + ggtitle("Urea") +
  theme(axis.title = element_text(size = 16)) +
  geom_point(data = logits, aes(x = as.numeric(class), y = logOR, color = smoke), size = 3) + theme(legend.position = c(0.85, 0.2))

