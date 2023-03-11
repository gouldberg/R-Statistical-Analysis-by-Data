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
# Variable Clustering
#
#   - Hierarchical cluster analysis on an similatrity matrix (such as squared correlations)
#   - It is often advisable to use robust (e.g., rank-based) measures for continuous variables if they are skewed, as skewed variables can
#     greatly affect ordinary correlation coefficeients.
#   - Pairwise deletion of missing values is also advisable for thie procedure:  casewise deletion can result in a small biased sample.
#   - When variables are not monotonically related to each other, Pearson or Spearman squared correlations can miss important associations and thus
#     are not always good similarity measures.
#     General and robust similarity measure is Hoeffding's D:  D statistic will detect a wide variety of dependencies between two variables.
# ------------------------------------------------------------------------------

# data has list type, so do not use dplyr::select()
x <- with(data, cbind(stage, rx, age, wt, pf, hx, sbp, dbp, ekg.norm, hg, sz, sg, ap, bm))
str(x)



# ----------
# To handle skewness, we use Spearman rank correlations for continuous variables
# spearman rho rank correlation plot

r <- rcorr(x, type="spearman")$r

maxabsr <- max(abs(r[row(r) != col(r)]))

p <- nrow(r)

v <- dimnames(r)[[1]]

plot(c(-.35, p+.5), c(.5, p+.25), type="n", axes=FALSE, xlab="", ylab="")

text(rep(.5, p), 1:p, v, adj=1)

for(i in 1:(p-1)){
  for(j in (i+1):p){
    lines(c(i,i), c(j,j+r[i,j]/maxabsr/2), lwd=3, lend="butt")
    lines(c(i-.2, i+.2), c(j,j), lwd=1, col=grey(.7))
  }
  text(i, i, v[i], str=-45,  adj=0)
}



# ----------
# Variable Clustering  -->  we combine sbp and dbp, and tentatively combine ap, sg
# We use Hoeffding's D: D will detect nonmonotonic associations
vc <- varclus(~ stage + rxn + age + wt + pfn + hx + sbp + dbp + ekg.norm + hg + sz + sg + ap + bm, sim="hoeffding", data = data)

plot(vc)


