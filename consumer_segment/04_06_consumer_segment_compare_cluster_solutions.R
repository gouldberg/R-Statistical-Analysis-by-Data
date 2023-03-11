# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("//media//kswada//MyFiles//R//consumer_segment")

packages <- c("dplyr", "Hmisc", "mclust")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Consumer Segment Data (created data)
# ------------------------------------------------------------------------------

seg.df <- read.csv("rintro-chapter5.csv", header = TRUE)


str(seg.df)


Hmisc::describe(seg.df)

summary(seg.df)

car::some(seg.df)


# remove the known segment assignments
seg.df2 <- seg.df[, -7]


# 300 * 6
dim(seg.df2)



# ---------------------------------------------------------------------------
# Comparing Cluster Solutions
#   - mclust::mapClass() examines all permutations of how two sets of class assignments might be related and selects a mapping that maximizes agreement
#     between the two assignment schemes.
#   - adjustedRandIndex(): likwise matches two assignments schemes and then computes the defree of agreement over and above what might be attributed to "chance"
#     by simply assigining all obervations to be largest group. Its magnitude may be interpreted similarly to a standard r correlation coeffs.
# ---------------------------------------------------------------------------

# cross-tabs
table(seg.LCA3$predclass, seg.LCA4$predclass)



# ----------
mapClass(seg.LCA3$predclass, seg.LCA4$predclass)


# -->
# "1" in the LCA3 model (a) maps best to "2" in the LCA4 model (b)



# ----------
adjustedRandIndex(seg.LCA3$predclass, seg.LCA4$predclass)


# -->
# The adjusted Rand index indicates that the matches between the two assignments lists is better than chance (= zero)



# ----------
set.seed(11021)

random.data <- sample(4, length(seg.LCA4$predclass), replace = TRUE)

adjustedRandIndex(random.data, seg.LCA4$predclass)


# -->
# If we were to test a randome assignment scheme, the adjustedRandIndex is close to zero.



# ----------
# Finally we compare the LCA 4-cluster solution to the true segments
table(seg.df$Segment, seg.k$cluster)
table(seg.df$Segment, seg.mc4$class)
table(seg.df$Segment, seg.LCA4$predclass)
table(seg.df$Segment, kpres$cluster)


adjustedRandIndex(seg.df$Segment, seg.k$cluster)
adjustedRandIndex(seg.df$Segment, seg.mc4$class)
adjustedRandIndex(seg.df$Segment, seg.LCA4$predclass)
adjustedRandIndex(seg.df$Segment, kpres$cluster)


# --<>
# With a Rand index of 0.38, the LCA solution matches the true segment assignments moderately better than chance alone, and other clustering methods.
# k-prototype algorithm performs not good...



