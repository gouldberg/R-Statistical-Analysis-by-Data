setwd("//media//kswada//MyFiles//R//snow")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  SNOW
#   - Data from Table.12 in John Snow "On the mode of communication of cholera" (Edinburgh medical journal 1.7 (1856)
#     http://www.ph.ucla.edu/epi/snow/table12a.html
#   - In 1849, both of water supplier companies (Southwalk and Vauxhall, and Lambeth) has water source from Thames river.
#     In 1852, Lambeth company changed its warter source to upper side of Thames river.
#     The decreased number of died by chorela infection in the area where water was supplied by Lambeth
#     owes to its change of water source ?
# ------------------------------------------------------------------------------


# number of died by cholera by area in 1849
# Southwark and Vauxhall Company
sv1849 <- c(283, 157, 192, 249, 259, 226, 352, 97, 111, 8, 235, 92)

# Lambeth Company & Southwark and Vauxhall Company
lsv1849 <- c(256, 267, 312, 257, 318, 446, 143, 193, 243, 215, 544, 187, 153, 81, 113, 176)



# ----------
# number of died by cholera by area in 1854
# Southwark and Vauxhall Company
sv1854 <- c(371, 161, 148, 362, 244, 237, 282, 59, 171, 9, 240, 174)

# Lambeth Company & Southwark and Vauxhall Company
lsv1854 <- c(113, 174, 270, 93, 210, 388, 92, 58, 117, 49, 193, 303, 142, 48, 165, 132)



# ----------
sv_death <- c(sv1849, sv1854)
lsv_death <- c(lsv1849, lsv1854)

sv_area <- paste0("sv_", c(1:length(sv1849), 1:length(sv1854)))
lsv_area <- paste0("lsv_", c(1:length(lsv1849), 1:length(lsv1854)))

sv_year <- c(rep("1849", length(sv1849)), rep("1854", length(sv1854)))
lsv_year <- c(rep("1849", length(lsv1849)), rep("1854", length(lsv1854)))


sv_death
lsv_death



# ----------
# Southwark & Vauxhall
sv <- data.frame(area = sv_area,
                 year = sv_year,
                 death = sv_death,
                 LSV = "0",
                 company = "Southwark and Vauxhall")


# Lambeth & Southwark and Vauxhall
lsv <- data.frame(area = lsv_area,
                  year = lsv_year,
                  death = lsv_death,
                  LSV = "1",
                  company = "Lambeth & Southwark and Vauxhall")


# by area and year
JS_df <- rbind(sv, lsv) %>%
  mutate(LSV =
           if_else(company == "Lambeth & Southwark and Vauxhall", 1, 0))


# by company
JS_sum <- JS_df %>%
  group_by(company, LSV, year) %>%
  summarise(death = sum(death))



# ----------
car::some(JS_df)

car::some(JS_sum)

dim(JS_df)

dim(JS_sum)

