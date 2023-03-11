setwd("//media//kswada//MyFiles//R//ceaq")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  CEAQ
#   - data analyzed in Bond and Fox (2015).
#     The Children's Empathic Attitudes Questionnaire (CEAQ) is a 16 item scale to measure empathy of late elementary and
#     middle school-aged children.
#     Each item has three ordered responses: "no" (1), "maybe" (2), and "yes" (3).
# ------------------------------------------------------------------------------
#  ceaq1:  When I'm mean to someone, I usually feel bad about it later.
#  ceaq2:  I'm happy when the teacher says my friend did a good job.
#  ** ceaq3:  I would get upset if I saw someone hurt an animal.
#  ceaq4:  I understand how other kids feel.
#  ceaq5:  I would feel bad if my mom's friend got sick.
#  ** ceaq6:  Other people's problems really bother me.
#  ceaq7:  I feel happy when my friend gets a good grade.
#  ceaq8:  When I see a kid who is upset it really bothers me.
#  ceaq9:  I would feel bad if the kid sitting next to me got in trouble.
#  * ceaq10:  It's easy for me to tell when my mom or dad has a good day at work.
#  ceaq11:  It bothers me when my teacher doesn't feel well.
#  ceaq12:  I feel sorry for kids who can't find anyone to hang out with.
#  ** ceaq13:  Seeing a kid who is crying makes me feel like crying.
#  ** ceaq14:  If two kids are fighting, someone should stop it.
#  * ceaq15:  It would bother me if my friend got grounded.
#  ceaq16:  When I see someone who is happy, I feel happy too.
#
#  *:  removed lated due to misfit by rating scale model
#  **:  threshold by rating scale model is very much different from other items
# ------------------------------------------------------------------------------


data("CEAQ", package = "MPsychoR")


str(CEAQ)


car::some(CEAQ)




# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
