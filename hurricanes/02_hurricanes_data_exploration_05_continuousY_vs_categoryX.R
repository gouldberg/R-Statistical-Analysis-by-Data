setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\hurricanes")

packages <- c("dplyr")

purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Hurricanes
# ------------------------------------------------------------------------------

# data("Hurricanes", package = "rethinking")

data <- read.csv(file = "Hurricanes.txt", header = T, sep = "\t")


dim(data)


str(data)



car::some(data)




# ------------------------------------------------------------------------------
# data exploration:  Y vs. category X
# ~ female
# ------------------------------------------------------------------------------

# boxplot and wilcoxon 2-samples test

boxplot(log(deaths + 0.001) ~ female, data = data)

wilcox.test(log(deaths + 0.001) ~ female, alternative = c("two.sided"), correct = TRUE, data = data)


boxplot(log(min_pressure + 0.001) ~ female, data = data)

wilcox.test(log(min_pressure + 0.001) ~ female, alternative = c("two.sided"), correct = TRUE, data = data)


boxplot(log(damage_norm + 0.001) ~ female, data = data)

wilcox.test(log(damage_norm + 0.001) ~ female, alternative = c("two.sided"), correct = TRUE, data = data)



# -->
# no median differences between female or not




# ----------
# by ggplot

ggplot(data, aes(as.factor(female), femininity)) + geom_boxplot()


ggplot(data, aes(as.factor(female), deaths)) + geom_violin()





# ------------------------------------------------------------------------------
# data exploration:  Y vs. category X
# ~ category
# ------------------------------------------------------------------------------


# larger the category, deadlier the Hurricanes

boxplot(log(deaths + 0.001) ~ factor(category), data = data)


d_list <- split(data$deaths, f = factor(data$category))

kruskal.test(x = d_list)



# -->
# Kruskal-Wallis test detect difference of group median




# ----------
# TukeyHSD:
# Null Hypothesis:  mean of paired differences IS zero
# Althernative Hypothesis:  mean of paired difference is DIFFERENT from zero

# ( output1 <- TukeyHSD(aov(log(deaths + 0.001) ~ factor(category), data = data)) )
( output1 <- TukeyHSD(aov(I(deaths^0.1) ~ factor(category), data = data)) )


par(mfrow = c(1,1))
plot(output1)




             
# ----------             
boxplot(log(deaths + 0.001) ~ female + factor(category), data = data)



# -->
# In larger category, there are some female names deadlier Hurricanes.


