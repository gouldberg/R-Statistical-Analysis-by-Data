# setwd("//media//kswada//MyFiles//R//mpls")
setwd("//media//kswada//MyFiles//R//Mixed_effects_and_multilevel_model//mpls")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This examples are based on "Longitudinal Data Analaysis for the Behavioral Sciences Using R"


# ------------------------------------------------------------------------------
# data:  MPLS
#   - Sample data from Minneapolis School District (MPLS)
# ------------------------------------------------------------------------------

MPLS <- read.table("MPLS.txt", header = T, na.strings = "-99")


str(MPLS)


dim(MPLS)


car::some(MPLS)



# ------------------------------------------------------------------------------
# data preparation:  Recoding
# ------------------------------------------------------------------------------


MPLS$ell2 <- factor(MPLS$ell, levels = c(0, 1), labels = c("No", "Yes"))


MPLS$riskC[MPLS$risk == "HHM"] <- "DADV"

MPLS$riskC[MPLS$risk == "POV"] <- "DADV"

MPLS$riskC[MPLS$risk == "ADV"] <- "ADV"

MPLS$risk2 <- factor(MPLS$riskC)



# ------------------------------------------------------------------------------
# data preparation:  wide to long format
# ------------------------------------------------------------------------------

MPLS.L <- reshape(data = MPLS, varying = 2:5, v.names = "read", timevar = "grade", times = 5:8, idvar = "subid", direction = "long")

MPLS.L <- MPLS.L %>% arrange(subid, grade)

head(MPLS.L, 10)



# ----------
rownames(MPLS.L) <- NULL



# ----------
MPLS.LS <- subset(MPLS.L, select = -riskC)


# ----------
write.table(MPLS.LS, file = "MPLS.LS.txt", row.names = F, quote = F)





