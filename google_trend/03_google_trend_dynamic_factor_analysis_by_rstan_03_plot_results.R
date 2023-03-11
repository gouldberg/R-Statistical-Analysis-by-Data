# setwd("//media//kswada//MyFiles//R//google_trend//")
setwd("//media//kswada//MyFiles//R//Bayesian_inference//google_trend//")

packages <- c("dplyr", "rstan")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  google trend
# ------------------------------------------------------------------------------

library(tidyverse)

s.dfData2 <- read_csv("dfData2.csv", locale = locale(encoding = "cp932"))


str(s.dfData2)


dim(s.dfData2)


car::some(s.dfData2)



# ------------------------------------------------------------------------------
# Dynamic Fdactor Analysis:  plot results
# ------------------------------------------------------------------------------

dfX <- as.data.frame(t(s.lDFARot108$mgX_mean_rotated))

colnames(dfX) <- paste0("F", 1:8)


dfX <- dfX %>%
  mutate(dDate = sort(unique(s.dfData2$date)))  %>%
  gather(sVar, gValue, -dDate) %>%
  mutate(sLabel = sVar, bFactor = 1) %>%
  dplyr::select(dDate, sLabel, gValue, bFactor)


# ----------
g <- ggplot(dfX, aes(x = dDate, y = gValue))

g <- g + geom_hline(yintercept = 0, color ="gray")
g <- g + geom_line()
g <- g + facet_wrap(sLabel ~ ., ncol = 4)
g <- g + labs(x = "週次", y = "因子得点")
g <- g + theme_bw(base_family = "MeiryoUI")

print(g)

# ggsave(g, file = paste0(GRAPHDIR, "/plotDFArot108_1.png"), width = 12, height = 8, units = "cm")



# ------------------------------------------------------------------------------
# Dynamic Fdactor Analysis:  plot results  city names relevant to F1
# ------------------------------------------------------------------------------

dfZ <- as.data.frame(lDFARot108$mgZ_mean_rotated)

colnames(dfZ) <- paste0("F", 1:8)


dfTable2 <- s.dfTable2 %>%
  filter(nSum > 10000, gSD2 > 11.75) 


dfZ <- bind_cols(dfTable2, dfZ) %>%
  filter(abs(F1) > 0.2, abs(F4) < 0.2) %>%
  top_n(3, F1) %>%
  mutate(sLabel = sprintf("%s(%+3.2f)", keyword, F1)) %>%
  dplyr::select(nCityID, sLabel)


dfPlot <- dfData2 %>%
  filter(nCityID %in% dfZ$nCityID) %>%
  mutate(fCityID = factor(nCityID, levels=dfZ$nCityID, labels=dfZ$sLabel))


# ----------
g <- ggplot(dfPlot, aes(x = date, y = hits))
g <- g + geom_line()
g <- g + facet_wrap(fCityID ~ .)
g <- g + labs(x = "週次", y = "検索量")
g <- g + theme_bw()
g <- g + theme_bw(base_family = "MeiryoUI")

print(g)

# ggsave(g, file = paste0(GRAPHDIR, "/plotDFArot108_2.png"), width = 12, height = 5, units = "cm")



# ------------------------------------------------------------------------------
# Dynamic Fdactor Analysis:  plot results  city names relevant to F4
# ------------------------------------------------------------------------------

anTARGET <- c(38, 306, 334)

dfZ <- as.data.frame(s.lDFARot108$mgZ_mean_rotated)

colnames(dfZ) <- paste0("F", 1:8)


dfTable2 <- s.dfTable2 %>%
  filter(nSum > 10000, gSD2 > 11.75) 


dfZ <- bind_cols(dfTable2, dfZ) %>%
  filter(abs(F4) > 0.2) %>%
  filter(nCityID %in% anTARGET) %>%
  mutate(sLabel = sprintf("%s(%+3.2f)", keyword, F4)) %>%
  dplyr::select(nCityID, sLabel)


dfPlot <- s.dfData2 %>%
  filter(nCityID %in% dfZ$nCityID) %>%
  mutate(fCityID = factor(nCityID, levels=dfZ$nCityID, labels=dfZ$sLabel))


# ----------
g <- ggplot(dfPlot, aes(x = date, y = hits))

g <- g + geom_line()
g <- g + facet_wrap(fCityID ~ .)
g <- g + labs(x = "週次", y = "検索量")
g <- g + theme_bw()
g <- g + theme_bw(base_family = "MeiryoUI")
print(g)

# ggsave(g, file = paste0(GRAPHDIR, "/plotDFArot108_3.png"), width = 12, height = 5, units = "cm")
