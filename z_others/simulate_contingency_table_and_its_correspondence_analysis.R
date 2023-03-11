setwd("//media//kswada//MyFiles//R//discrete_data_analysis_with_r")

packages <- c("dplyr", "vcd", "vcdExtra", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Simulate contingency table with Poisson counts with some association
# ------------------------------------------------------------------------------

# set.seed(1234)


dim <- c(3, 2, 2, 2)


# specify a non negative vector of Poisson means to create some associations among the table factors 
rnd <- rpois(prod(dim), c(15,10,5,20))


tab <- array(rnd, dim = dim)



dimnames(tab) <- list(Pet = c("dog", "cat", "bird"), 
                      Age = c("young", "old"),
                      Color = c("black", "white"),
                      Sex = c("male", "female"))


tab



# ----------
# by ftable
( pet.mat <- as.matrix(ftable(Pet + Age ~ Color + Sex, tab), sep = ".") )




# ----------
# or by interaction coding
tab.df <- as.data.frame(as.table(tab))


tab.df <- within(tab.df, {
  Pet.Age = interaction(Pet, Age)
  Color.Sex = interaction(Color, Sex)
})


( pet.mat <- xtabs(Freq ~ Color.Sex + Pet.Age, data = tab.df) )




# ------------------------------------------------------------------------------
# mosaic plot
# ------------------------------------------------------------------------------

vcd::mosaic(pet.mat, shade=TRUE, suppress=0, labeling=labeling_residuals, gp_text=gpar(fontface=2))

vcd::mosaic(pet.mat, shade = TRUE, labeling = labeling_residuals)



# ------------------------------------------------------------------------------
# Correspondence analysis
# ------------------------------------------------------------------------------

pet.ca <- ca(pet.mat)

summary(pet.ca)


par(mfrow = c(1,1))
plot(pet.ca)




