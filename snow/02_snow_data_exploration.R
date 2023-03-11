setwd("//media//kswada//MyFiles//R//snow")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  SNOW
# ------------------------------------------------------------------------------



# ------------------------------------------------------------------------------
# data exploration:  summary by company and year
# ------------------------------------------------------------------------------

# summary by company and year
( JS_grp_summary <- JS_sum %>%
  mutate(year = paste("year", year, sep = "_")) %>%
  spread(year, death) %>%
  mutate(gap = year_1854 - year_1849,
         gap_rate = year_1854 / year_1849 - 1) )



# summary by company and year in log
( JS_grp_summary_ln <- JS_sum %>%
  mutate(year = paste("year", year, sep = "_"),
         death = log(death)) %>%
  spread(year, death) %>%
  mutate(gap = year_1854 - year_1849) )




# ------------------------------------------------------------------------------
# data exploration:  visualization of the change
# ------------------------------------------------------------------------------

( did_plot <- JS_sum %>%
  ggplot(aes(y = death, x = year, shape = company)) +
  geom_point(size = 2) +
  geom_line(aes(group = company), linetype = 1) +
  ylim(2000, 4250) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.margin = margin(1,1,1,1, "cm")) )



did_plot +
  annotate("text", x = 2.2, y = 2400, label = "(1)") +
  annotate("text", x = 2.2, y = 3904 + 197*0.6, label = "(2)") +
  annotate("text", x = 2.2, y = 3300, label = "(3)") +
  annotate("segment", # for common trend in treatment group
           x = 1, xend = 2,
           y = 3904, yend = 3904 + 197,
           arrow = arrow(length = unit(.2,"cm")),
           size = 0.1,
           linetype = 2) +
  annotate("segment", # for parallel trend
           x = 1, xend = 2,
           y = 2261, yend = 2261,
           size = 0.1,
           linetype = 2) +
  annotate("segment", # for parallel trend
           x = 1, xend = 2,
           y = 3904, yend = 3904,
           size = 0.1,
           linetype = 2) +
  annotate("segment", # for (1)
           x = 2.07, xend = 2.07,
           y = 2261, yend = 2458,
           arrow = arrow(ends = "both",
                         length = unit(.1,"cm"),angle = 90)) +
  annotate("segment", # for (2)
           x = 2.07, xend = 2.07,
           y = 3904, yend = 3904 + 197,
           arrow = arrow(ends = "both",
                         length = unit(.1,"cm"),angle = 90)) +
  annotate("segment", # for (3)
           x = 2.07, xend = 2.07,
           y = 3904, yend = 2547,
           arrow = arrow(ends = "both",
                         length = unit(.1,"cm"),angle = 90))

