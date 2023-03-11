setwd("//media//kswada//MyFiles//R//harvardpsych")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  HarvardPsych
# ------------------------------------------------------------------------------

data("HarvardPsych", package = "MPsychoR")

str(HarvardPsych)

dim(HarvardPsych)


# researchers in rows, words in columns  (29 * 43)
head(HarvardPsych)



# ----------
rownames(HarvardPsych)

colnames(HarvardPsych)




# ------------------------------------------------------------------------------
# Distribution for each variables
# ------------------------------------------------------------------------------

summary(HarvardPsych)


sort(apply(HarvardPsych, 2, max), decreasing = T)



# -->
# Some researcher use "memory" 15 times
# Some researcher use "brain" 10 times
# Some researcher use "visual" 10 times
# Some researcher use "language" 9 times



# ----------
sort(round(apply(data.frame(HarvardPsych), 2, sd), digits = 3), decreasing = T)



# -->
# memory has the largest standard deviation and the second is visual




# ------------------------------------------------------------------------------
# "memory" and "Schacter"
# ------------------------------------------------------------------------------

data.frame(HarvardPsych, researcher = rownames(HarvardPsych)) %>% filter(memory > 0) %>% dplyr::select(researcher, memory)



# -->
# Schacter and Buckner use memory 15 times and 8 times respectively



# ----------
data.frame(HarvardPsych, researcher = rownames(HarvardPsych)) %>% filter(researcher == "Schacter")



# -->
# For Schacter "memory" is typical
# But NOTE that Schacter use "cognitive" 3 times  (largest is )




# ------------------------------------------------------------------------------
# "cognitive"
# ------------------------------------------------------------------------------

data.frame(HarvardPsych, researcher = rownames(HarvardPsych)) %>% filter(cognitive > 0) %>% dplyr::select(researcher, cognitive)



# -->
# most frequent user:  Spelke, Konkle (4 times)
# Shacter use 3 times and positioned in second




# ------------------------------------------------------------------------------
# "brain" and "Buckholtz" and "Buckner"
# ------------------------------------------------------------------------------

data.frame(HarvardPsych, researcher = rownames(HarvardPsych)) %>% filter(brain > 0) %>% dplyr::select(researcher, brain)



# -->
# Buckholtz use brain 10 times
# Buckner use 3 times



# ----------
data.frame(HarvardPsych, researcher = rownames(HarvardPsych)) %>% filter(researcher == "Buckholtz")



# -->
# Other than brain,
# Buckholtz use behavior, selfcontrol many times



# ----------
data.frame(HarvardPsych, researcher = rownames(HarvardPsych)) %>% filter(researcher == "Buckner")



# -->
# Buckner uses memory 8 times (more frequently than brain)




# ------------------------------------------------------------------------------
# "visual" and "Xu", "Carammazza" and "Nakayama"
# ------------------------------------------------------------------------------

data.frame(HarvardPsych, researcher = rownames(HarvardPsych)) %>% filter(visual > 0) %>% dplyr::select(researcher, visual)



# -->
# Xu use visual 10 times, and Caramazza and Nakayama 5 times



# ----------
data.frame(HarvardPsych, researcher = rownames(HarvardPsych)) %>% filter(researcher == "Xu")



# -->
# Other than visual,
# Buckholtz use information 5 times



# ----------
data.frame(HarvardPsych, researcher = rownames(HarvardPsych)) %>% filter(researcher == "Caramazza")



# -->
# Other than visual,
# Carammaza uses representation 6 times



# ----------
data.frame(HarvardPsych, researcher = rownames(HarvardPsych)) %>% filter(researcher == "Nakayama")



# -->
# For Nakayama,
# visual is typical



# ------------------------------------------------------------------------------
# "language" and "Snedeker" and "Pinker"
# ------------------------------------------------------------------------------

data.frame(HarvardPsych, researcher = rownames(HarvardPsych)) %>% filter(language > 0) %>% dplyr::select(researcher, language)



# -->
# Snedeker use language 9 times and Pinker 7 times



# ----------
data.frame(HarvardPsych, researcher = rownames(HarvardPsych)) %>% filter(researcher == "Snedeker")




# ----------
data.frame(HarvardPsych, researcher = rownames(HarvardPsych)) %>% filter(researcher == "Pinker")




# ------------------------------------------------------------------------------
# Correlation among variables
# ------------------------------------------------------------------------------

round(cor(HarvardPsych), digits = 3)


# average correlation
sort(apply(cor(HarvardPsych), 2, mean))



psych::pairs.panels(HarvardPsych)



# ------------------------------------------------------------------------------
# Correlation among researchers
# ------------------------------------------------------------------------------

cor(t(HarvardPsych))


# average correlation
sort(apply(cor(t(HarvardPsych)), 2, mean))


# psych::pairs.panels(t(HarvardPsych))

