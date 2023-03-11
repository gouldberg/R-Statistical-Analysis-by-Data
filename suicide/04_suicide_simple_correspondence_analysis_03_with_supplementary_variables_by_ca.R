setwd("//media//kswada//MyFiles//R//suicide")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Suicide rates in Germany
# ------------------------------------------------------------------------------
data("Suicide", package = "vcd")

data <- Suicide

data


# Interactive coding of sex and age.group
data <- within(data, {
  age_sex <- paste(age.group, toupper(substr(sex, 1, 1)))
})


car::some(data)


( tab <- xtabs(Freq ~ age_sex + method2, data = data) )
( tab2 <- xtabs(Freq ~ sex + age.group + method2, data = data) )



# ------------------------------------------------------------------------------
# Correspondence Analysis:  Marginal tables and supplementary variables
# ------------------------------------------------------------------------------

# 2-way, ignoring sex
( tab3 <- xtabs(Freq ~ age.group + method2, data = data) )



# To treat the levels of sex as supplementary points, append 2-way table by sex and method
tab.sup <- xtabs(Freq ~ sex + method2, data = data)

( tab3s <- rbind(tab3, tab.sup) )




# ----------
# including sex
suicide.ca <- ca::ca(tab)


# ignoring sex, but sex is appended as supplementary variable
# In the call ca(), we then indicate these last 2 rows as supplementary
suicide.ca3s <- ca::ca(tab3s, suprow = 6:7)

chisq.test(tab3)

summary(suicide.ca3s)



# -->
# This CA analysis has the same total Pearson chi-square = 3422.5.
# However, the scree plot display shows that the association between age and method is essentially one-dimensional, 
# but note also that dimension 1 ("age-method") in this analysis has nearly the same inertia (0.0604) as the second dimension (0.0596) in the analysis of the stacked table.



# ----------
# plot 2D CA solution for the [Age][Method] marginal table, with category points for Sex shown as supplementary points
graphics.off();  par(mfrow=c(1,2));

res <- plot(suicide.ca3s, pch = c(16, 15, 17, 24), main = "2D CA: ignoring Sex")
lines(res$rows[6:7,])
plot(suicide.ca, main = "2D CA: Including Sex")



# --> 
# You can see that ignoring sex has collapsed the differences between males and females
# The supplementary poins for sex point toward the methods that are more prevalent for females and males
