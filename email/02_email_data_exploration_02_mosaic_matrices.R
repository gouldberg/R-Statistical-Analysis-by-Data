setwd("//media//kswada//MyFiles//R//email")

packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  email
# ------------------------------------------------------------------------------

email <- read_csv("Kevin_Hillstrom_MineThatData_E-MailAnalytics_DataMiningChallenge_2008.03.20.csv")

str(email)

dim(email)


car::some(email)



# ------------------------------------------------------------------------------
# data exploration:  data distribution by each variable
# ------------------------------------------------------------------------------

Hmisc::describe(email)



# ------------------------------------------------------------------------------
# data exploration:  mosaic matrices
# ------------------------------------------------------------------------------

tab <- xtabs(~ recency + history_segment + conversion, data = email)

tab2 <- xtabs(~ mens + womens + zip_code + newbie + conversion, data = email)

tab3 <- xtabs(~ channel + segment + visit + conversion, data = email)


tab

dim(tab)



# ----------
library(vcd);  library(vcdExtra);


pairs(tab, gp = shading_Friendly2, space = 0.25, gp_args = list(interpolate = 1:4), diag_panel_args = list(offset_varnames = -0.5))


pairs(tab2, gp = shading_Friendly2, space = 0.25, gp_args = list(interpolate = 1:4), diag_panel_args = list(offset_varnames = -0.5))


pairs(tab3, gp = shading_Friendly2, space = 0.25, gp_args = list(interpolate = 1:4), diag_panel_args = list(offset_varnames = -0.5))



# -->
# very hard for small conversion data ...

# history_segment: small  *  recency >= 8
# history_segument: large  *  recency <= 3

# visit = 1  *  Mens Emai
# No visit  *  No Email



