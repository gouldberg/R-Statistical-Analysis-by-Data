setwd("//media//kswada//MyFiles//R//pvq40")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PVQ40
# ------------------------------------------------------------------------------

data(PVQ40, package = "smacof")


str(PVQ40)


attributes(PVQ40)


car::some(PVQ40)



# ------------------------------------------------------------------------------
# Search for a configuration with all 40 points and check clusters or transition
# ------------------------------------------------------------------------------


data.r <- na.omit(PVQ40)


# center ratings
data1 <- data.r - rowMeans(data.r)


# turn preference ratings into dissimilarities
diss <- max(data1) - data1



# ----------
# Search for a configuration with all 40 points and with 146 additional points for the 146 individuals
# such that the distances between the person points and the value points directly match the dissimilarities (except for an overall scaling factor)
#  --> This model is ratio unfolding (default)
unf <- unfolding(diss)

graphics.off()
plot(unf, what="columns", col.columns=1, asp=1, xlim=c(-1.3,1.3), ylim=c(-1.3,1.3),
     label.conf.columns=list(col="black"), main="", pch=16)



# ----------
# fit circle
circle <- fitCircle(unf$conf.col[,1], unf$conf.col[,2])

draw.circle(circle[[1]], circle[[2]], radius=circle[[3]])



# -->
# The items in some categories (e.g., TR) scatter quite a bit in space, while others (e.g., PO) form dense clusters.
# Also, there is considerable overlap of the various types of value items (e.g., BE and UN).
# This indicates that the circle of 10 basic values may be understood more as a continuum of personal values with gradual transitions
# rather than as a necklace of discrete points



# ------------------------------------------------------------------------------
# Plot individual by gender
# ------------------------------------------------------------------------------

gender <- attr(data.r, "Gender")[-attr(data.r, "na.action")]

points(unf$conf.row, pch=gender, cex=1, lwd=1.2, col = c("blue","red")[gender])



# ------------------------------------------------------------------------------
# Discriminant analysis to assess preference by gender
# ------------------------------------------------------------------------------

library(MASS)

Y <- unf$conf.row

z <- as.data.frame(cbind(gender, Y))



# ----------
fit <- lda(gender ~ Y, na.action="na.omit", data=z)


fit



# ----------
# plot the projection line on which females and males are best separated
slope <- fit$scaling[2] / fit$scaling[1]


angle <- atan(slope) * 180 / pi
px <- 1.4
py <- 0 + slope * px


abline(a=0, b=slope, lty=1, col="red")
text(x=px-.1, y=py-.1, "gender", srt=angle, cex=.8)



# -->
# Females tend to lie significantly more at the BE / UN end of this scale,
# and mena closer to PO / AC  --> also normal in value research.



# ----------
# T-test for gender discrimination
LDS <- as.data.frame(predict(fit))

L4 <- LDS[,"LD1"]

tt <- t.test(L4 ~ gender)

tt



# ------------------------------------------------------------------------------
# Multiple regression for age
# ------------------------------------------------------------------------------

age <- attr(data.r, "Age")[-attr(data.r, "na.action")]


# regress age on unfolded distance D1 and D2
f <- lm(age ~ Y[,"D1"] + Y[,"D2"])


summary(f)



# Ceef for D2 and D1
wy <- f$coefficients[3]
wx <- f$coefficients[2]
slope <- wy / wx


age.pred <- predict(f)
r <- cor(age, age.pred)
r


angle = atan(slope) * 180 / pi

px <- 1.4
py <- 0 + slope * px


abline(a=0, b=slope, lty=2, col="blue")
text(x=px-.2, y=py-.2, "age", srt=angle, cex=.8)


# -->
# The projections of the persons onto this line are correlated with their age with r = 0.41.
# Older respondents tend to lean more towared TR and CO, and younger ones more towared ST / HE and PO / AC, a typical finding in value research.


