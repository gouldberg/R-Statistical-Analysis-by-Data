# setwd("//media//kswada//MyFiles//R//email")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\m_tenstocks")


packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  m-tenstocks
# ------------------------------------------------------------------------------

mtenstocks <- read.csv("m-tenstocks.txt", sep = " ", header = T)


str(mtenstocks)


dim(mtenstocks)


car::some(mtenstocks)



# ----------
# log returns

rtn <- log(mtenstocks[,2:11] + 1)



# ------------------------------------------------------------------------------
# compute correlation matrix
# ------------------------------------------------------------------------------

polcor <- cor(rtn)


polcor




# ------------------------------------------------------------------------------
# specify the constraints
# ------------------------------------------------------------------------------


h1 <- c(1,1,1,1,rep(0,6))

h2 <- c(0,0,0,0,1,1,1,0,0,0)

h3 <- c(rep(0,7), 1,1,1)


H <- cbind(h1, h2, h3)




# ------------------------------------------------------------------------------
# constrained factor model
# ------------------------------------------------------------------------------


cfm <- MTS::hfactor(rtn, H, 3)



# -->
# "variability explained" = 0.724, this is close to unconstrained 3-factor model,
# so we confirms that the constrintas are acceptable.





# ------------------------------------------------------------------------------
# plot factor scores
# ------------------------------------------------------------------------------


graphics.off()

par(mfrow = c(1,1), mar = c(2,2,2,2))

MTSplot(cfm$F)




# ------------------------------------------------------------------------------
# plot factor scores in 2D by year and month
# ------------------------------------------------------------------------------


yyyy <- mtenstocks %>% data.frame() %>% mutate(yyyy = substring(date, 1, 4)) %>% dplyr::select(yyyy) %>% pull()

mm <- mtenstocks %>% data.frame() %>% mutate(mm = substring(date, 5, 6)) %>% dplyr::select(mm) %>% pull()


tmp <- data.frame(cfm$F) %>% mutate(yyyy = yyyy, mm = mm)



# ----------
par(mfrow = c(1,1), mar = c(2,2,2,2))


lattice::xyplot(X2 ~ X1 | yyyy, data = tmp, pch = 20, col = "blue")

lattice::xyplot(X2 ~ X1 | mm, data = tmp, pch = 20, col = "blue")
