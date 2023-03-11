setwd("//media//kswada//MyFiles//R//wght")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wght
#   - The data come from the National Longitudinal Survey of YOuth -- Children and Young Adults (NLSY-CYA: Center for Human Resource Research, 2004).
# ------------------------------------------------------------------------------

wght_long <- read.table("//media//kswada//MyFiles//references//GrowthModeling_StructuralEquationAndMultilevelModelingApproaches//Data//wght_data.dat",
                        na.strings = ".")


str(wght_long)

car::some(wght_long)


# ----------
names(wght_long) <- c('id','occ','occ_begin','year','time_in_study','grade','age','gyn_age', 'wght')



# ------------------------------------------------------------------------------
# restructuring data from long to wide
# ------------------------------------------------------------------------------

wght_long$age_r <- round(wght_long$age)

wght_wide <- reshape(wght_long, v.names='wght', idvar='id',
                    timevar='age_r', direction='wide')

head(wght_wide)



# ----------
wght_wide1 <- wght_wide[ , c('id','wght.5','wght.6','wght.7','wght.8','wght.9','wght.10',
                            'wght.11','wght.12','wght.13','wght.14','wght.15','wght.16',
                            'wght.17','wght.18','wght.19')]

head(wght_wide1)


names(wght_wide1) = c('id','wght5','wght6','wght7','wght8','wght9','wght10',
                      'wght11','wght12','wght13','wght14','wght15','wght16',
                      'wght17','wght18','wght19')


## Writing out File ##
write.table(wght_wide1, 'C:/GRE2016/Data/wght_wide.dat', sep=" ", row.names=FALSE, col.names=FALSE, na='.')



# ------------------------------------------------------------------------------
# restructuring data from wide back to long
# ------------------------------------------------------------------------------

wght_long_new <- reshape(wght_wide1, idvar='id', 
                        varying=c('wght5','wght6','wght7','wght8','wght9','wght10',
                                  'wght11','wght12','wght13','wght14','wght15','wght16',
                                  'wght17','wght18','wght19'),
                        times=c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19),
                        v.names='wght', direction='long')

wght_long_new = wght_long_new[order(wght_long_new$id, wght_long_new$time),]

wght_long_new1 = wght_long_new[which(!is.na(wght_long_new$wght)), ]

head(wght_long_new1)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

wght_long1 <- wght_long[which(wght_long$id>1300 & wght_long$id<1600), ]


library(ggplot2)


# ----------
# geom_line()
plot_obs <- ggplot(data=wght_long1, aes(x=age, y=wght, group=id)) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(breaks = c(5,7,9,11,13,15,17), name = "Chronological Age") + 
  scale_y_continuous(breaks = c(25,50,75,100,125,150,175,200,225), name = "Weight")


print(plot_obs)



# ----------
# geom_point()
plot_obs <- ggplot(data=wght_long1, aes(x=age, y=wght, group=id)) +
  geom_line() +
  geom_point(size=2) +
  theme_classic() +
  scale_x_continuous(breaks = c(5,7,9,11,13,15,17), name = "Chronological Age") + 
  scale_y_continuous(breaks = c(25,50,75,100,125,150,175,200,225), name = "Weight")

print(plot_obs)




# ----------
head(wght_wide1)

wght_vars = wght_wide1[ , c('wght5','wght6','wght7','wght8','wght9','wght10','wght11','wght12',
                            'wght13','wght14','wght15','wght16','wght17','wght18','wght19')]

head(wght_vars)



# ----------
library(psych)

describe(wght_vars)


# -->
# 1.  Sample size changes dramatically across age, with approximately 1,200 participants assessed at ages 10, 11, and 12 and less than 10 participants
# assessed at age 19.

# 2.  There appear to be more potential coding / reporting errors in the data.
# Minimum values for weight at ages 7, 8, aand 12 were less than 10 pounds,
# and maximum values for weights at ages 8, 9, and 10 were in the 200s.

# 3.  Both extremes would be unusual in early and middle childhood nad required further investigation and/or cleaning.
# THe age-specific means display an increasing trend over time, but the increases, though, are not constant as age-to-age differences.

# 4.  Age-to-age differences in the standard deviations indicate that the amount of between-child differences in weight
# also increased through early and middle childhood and stabilitzed around age 11
# (altough some caution in interpretation is warranted given that the sample size also decreased rapidly after age 11).

# 5. The skew and kurtosis values show that the distributions of weight, at most ages, are both positively skewed, which is typical of
# weight data since weight cannot go below zero, and platykurtic.
# The similarity of skew and kurtosis across ages (except for the age 8) suggests that non-normality may be a key feature of these data
# and that a model that accounts for non-normality may be warranted.



# ----------
# 
cor(wght_vars, use='pairwise.complete.obs')

cov(wght_vars, use='pairwise.complete.obs')



# ----------
# Basic Scatterplot Matrix
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}


pairs(~ wght5+wght6+wght7+wght8+wght9+
        wght10+wght11+wght12+wght13+wght14+
        wght15+wght16+wght17+wght18+wght19,
      data = wght_wide1, diag.panel = panel.hist)


pairs(~wght5+wght6+wght7+wght8+wght9+wght10, data=wght_wide1, diag.panel=panel.hist)



# -->
# 1. No individuals were measured at both ages.
# Considering the data collection setup for tne NLSY-CYA, this missingness is expected because assessments were obtained
# approximately every other year.
# 2. A number of unusual observations are evident.
# For example, in 8/9 year, there is an observation very far to the right that is all alone.
# Checking the axes, this individual appeared to have gained 100 pounds in one year, which is unlikely and suggests that
# additional cleaning of these data is required.


