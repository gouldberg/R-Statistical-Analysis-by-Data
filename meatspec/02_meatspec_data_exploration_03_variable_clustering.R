# setwd("//media//kswada//MyFiles//R//melanoma")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\meatspec")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  meatspec
# ------------------------------------------------------------------------------

data(meatspec, package="faraway")


str(meatspec)


dim(meatspec)




# ------------------------------------------------------------------------------
# data exploration:  variable clustering
# ------------------------------------------------------------------------------

library(Hmisc)


coln <- colnames(meatspec)


char0 <- paste0(coln, collapse="+")


eval(parse(text = paste0("vc <- varclus( ~ ", char0, ", sim='hoeffding', data = meatspec)")))



# ----------
par(mfrow = c(1,1))

plot(vc)
