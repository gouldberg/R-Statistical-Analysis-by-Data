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
# data exploration:  hits by city visualized in time series
# ------------------------------------------------------------------------------

anTarget <- c(100, 366, 8)  # 秋葉原, 屋久島, 江差


dfPlot <- s.dfData2 %>% filter(nCityID %in% anTarget) 



# ----------
dfLabel <- dfPlot %>% dplyr::select(nCityID, keyword) %>% distinct()

asLabel <- dfLabel$keyword

names(asLabel) <- dfLabel$nCityID


dfPlot <- dfPlot %>% mutate(fKeyword = factor(keyword, levels=asLabel[as.character(anTarget)]))


# ----------
g <- ggplot(dfPlot, aes(x = date, y = hits))
g <- g + geom_line()
g <- g + facet_wrap(fKeyword ~ .)
g <- g + labs(x = "週次", y = "検索量")
g <- g + theme_bw()
g <- g + theme_bw(base_family = "MeiryoUI")
print(g)


# ggsave(g, file = paste0(GRAPHDIR, "/plotData2_2.png"), width = 12, height = 5, units = "cm")



# ------------------------------------------------------------------------------
# data exploration:  hits by city  total and standard deviation
# ------------------------------------------------------------------------------

s.dfTable2 <- s.dfData2 %>%
  group_by(nCityID, keyword) %>%
  summarize(
    nSum = sum(hits), 
    gSD1  = sd(hits), 
    gSD2  = sd(gAdjusted)
  ) %>%
  ungroup()


s.dfTable2



# ----------
library(ggrepel)

g <- ggplot(data = s.dfTable2, aes(x = nSum, y = gSD2, label = keyword))
g <- g + geom_rect(xmin = 10000, xmax = 20000, ymin = 11.75, ymax = 25, fill = "gray", alpha = 0.1)
g <- g + geom_point(size = 1, alpha = 1)
g <- g + geom_text(size = 2.5, hjust = "left", alpha = 0.5, nudge_x = 200)

g <- g + labs(x = "検索量合計", y = "季節調整後の検索量SD")
g <- g + theme_bw(base_family = "MeiryoUI")
print(g)


# ggsave(g, file = paste0(GRAPHDIR, "/plotTable2_1.png"), width = 12, height = 8, units = "cm")
