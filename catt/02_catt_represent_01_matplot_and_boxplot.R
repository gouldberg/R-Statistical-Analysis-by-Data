setwd("//media//kswada//MyFiles//R//catt")

packages <- c("dplyr", "fda", "refund")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  CATT
# ------------------------------------------------------------------------------

# VAS Longitudinal Measurement
Data <- read.csv("va.csv")


# VAS Baseline Measurements
Base <- read.csv("bv.csv")


# Other Subject Information
general_info <- read.csv("gi.csv")


str(Data)

str(Base)

str(general_info)



# ----------
car::some(Data)

car::some(Base)

car::some(general_info)



# ------------------------------------------------------------------------------
# Represent by matplot()
# ------------------------------------------------------------------------------

# preprocess:  convert to matrix of week(row) * subject(column)

Data$week <- as.character(Data$week)

dat <- data.frame(week = as.character(unique(Data[,"week"])))

tmp <- split(Data[,c("alpha_code", "week", "studyeye_va")], f = Data$alpha_code)

for(i in 1:length(tmp)){
  tmptmp <- tmp[[i]] %>% dplyr::select(week, studyeye_va) %>% as.data.frame()
  colnames(tmptmp) <- c("week", names(tmp)[i])
  dat <- dat %>% left_join(., tmptmp, by = "week")
}

dat$week <- as.numeric(dat$week)

dat <- dat %>% arrange(week) %>% as.matrix()



# ----------
# plot all subjects
matplot(dat[,2:ncol(dat)], x = unique(Data[,"week"]), type = "l")


# -->
# Those data are difficult to be represented by functional data ...



# ------------------------------------------------------------------------------
# Represent by bwplot()
# ------------------------------------------------------------------------------

Data$week <- factor(Data$week, levels = seq(4, 104, 4))

lattice::bwplot(studyeye_va ~ week, data = Data)

