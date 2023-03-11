setwd("//media//kswada//MyFiles//R//prostate")

packages <- c("dplyr", "Hmisc", "rms", "lattice", "ggplot2")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  prostate
# ------------------------------------------------------------------------------

getHdata(prostate)

str(prostate)

car::some(prostate)


# ----------
# convert an old data format to R format
prostate$sdate <- as.Date(prostate$sdate)
data <- prostate
str(data)


data$cvd <- data$status %in% c("dead - heart or vascular", "dead - cerebrovascular")



# ------------------------------------------------------------------------------
# conversion
# ------------------------------------------------------------------------------

# combining an infrequent category with the next category, and dichotomizing ekg

levels(data$ekg)[levels(data$ekg) %in% c("old MI", "recent MI")] <- "MI"

data$ekg.norm <- 1 * (data$ekg %in% c("normal", "benign"))

levels(data$ekg) <- abbreviate(levels(data$ekg))

data$pfn <- as.numeric(data$pf)

levels(data$pf) <- levels(data$pf)[c(1,2,3,3)]

data$rxn <- as.numeric(data$rx)



# ------------------------------------------------------------------------------
# Single imputations (before regression analysis) and transformation
#    - transcan() by default use a maximum generalized variance (MGV) method that incorporates canonical variates to optimally transform
#      both sides of a multiple regression model.
#      Each predictor is treated in turn as a variable being predicted, and all variables are expanded into restricted cubic splines
#      (for continuous variables) or dummy variables (for categorical ones)
#    - The goal of maximum generalized variance (MGV) method is to transform each variable so that it is most similar to predictions from the other
#      transformed variables. MGV does not use principal components, but one all variables have been transformed, you may wanto to summarize them
#      with the first PC.
#    - The SAS PRINQUAL procedure implements the maximum total variance (MTV) and maximum generalized variance (MGV) methods,
#      and allows for very flexible transformations of the predictors, including monotonic splines and ordinary cubic splines.
#      SAS PRINQUAL implemented also a method for simultaneously imputing missing values while solving for transformations.
#    - A simple modification of SAS PRINQUAL is implemented in transcan()
#      Imputed values are initialized to medians of contious variables and the most frequent category of categorical variables.
#      For continuous variables, transformations are initialized to linear functions.
#      For categorical ones, transformations may be initialized to the identify function, to dummy variables indicating whether the observation
#      has the most prevalent categorical value, or to random numbers.
#      Transfomred variables are normalized to have mean 0 and standard deviation 1.
#
#    - A very flexible automatic procedure for transforming each predictor based on all remaining predictors is the ACE
#      (alternating conditional expectation) procedure of Breiman and Friedman.
#      Unfortunately, ACE does not handle missing values.
# ------------------------------------------------------------------------------

ptrans <- transcan(~ sz + sg + ap + sbp + dbp + age + wt + hg + ekg + pf + bm + hx + dtime + rx, data = data,
                   imputed = TRUE, transformed = TRUE, trantab = TRUE, show.na = TRUE, frac = 0.1, pl = TRUE, pr = TRUE)


# ---------- 
ggplot(ptrans, scale=TRUE) + theme(axis.text.x = element_text(size=6))
# ggplot(ptrans, scale=FALSE) + theme(axis.text.x = element_text(size=6))


# -->
# Imputed values are shown as red plus signs.
# Transformed values are arbitrarily scaled to [0,1]

# Note that at face value the transformation of ap was derved in a circular manner,
# since the combined index of stage and histologic grade (sg) uses in its stage component a cutoff on ap.
# Note that bm and hx are represented as binary variables, so their coefficeitns in the table of canonical variable coefficients are on a different scale.



# ----------
summary(ptrans, digits = 4)


# -->
# R or adjusted R^2:
#   - age, wt, ekg, pf, and hx are not strongly related to other variables.
#     Imputations for age, wt, ekg are thus relying more on the median or modal values from the marginal distributions.

# the coeffs of first (standardized) canonical variates: 
#   - sbp is predicted almost solely from dbp
#   - bm is predicted mainly from ap, hg and pf



# ------------------------------------------------------------------------------
# Single imputations (before regression analysis)
# ------------------------------------------------------------------------------

# the value to be imputed
ptrans$imputed



# ----
# impute all missing values in all variables given to transcan
( imp <- impute(ptrans, data = data, list.out=TRUE) )

imp$sz


var_na <- c("sz", "sg", "age", "wt", "ekg")
for(i in var_na) data[[i]] <- imp[[i]]


# check sum(is.na)
sum(is.na(data))
check_sumisna <- function(data){ sum(is.na(data)) }
apply(data, FUN=check_sumisna, MARGIN=2)



# ------------------------------------------------------------------------------
# Transform data
# ------------------------------------------------------------------------------
data_trans <- as.data.frame(ptrans$transformed)

data_trans$cvd <- data$cvd

( var <- colnames(data_trans) )



# ----------
# Check the transfomed data
psych::describe(data_trans)

psych::describe(data)

psych::describe(as.data.frame(imp))


# -->
# Note that the transformed data have mean 0 and variance 1 for continuous data (other than binary bm and hx)



# ----------
psych::pairs.panels(data_trans[,var])

psych::pairs.panels(data[,var])


# but for example "hg" is really skewed now in transformed data.

