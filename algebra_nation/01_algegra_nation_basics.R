# setwd("//media//kswada//MyFiles//R//lalonde_nswcps")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\algebra_nation")

packages <- c("dplyr", "tidyverse", "MatchIt", "Matching", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data: Algebra Nation
#   - Virtual learning environments such as virtual K-12 schools, e-learning management systems, intelligent turtoring systems,
#     massive open online course (MOOCs) provide the opportunity of quick collection of responses to surveys and scales,
#     as well as collection of server logs, click streams, time on task, and discussion posts (U.S. Department of Education, 2012).
#     These data provide opportunities to evaluate whether virtual learning environments lead to improvements in teaching and learning.
#   - The objective of this example is to estimate the effect of school participation in the Algebra Nation virtual learning environment
#     on the school-level means of student scores on Florida's Algebra I End-of-Course (EOC) assessment.
#     Algebra Nation is a virtual learning environment that supports students and teachers in meeting mathematics state standards required
#     on the Florida's Algebra I EOC assessment.
#     Algebra Nation has video lessons covering algebra concepts, formative assessments, and an interactive student wall.
#     Algebra Nation is a schoolwide program because it is implemented by integrating the school's network system with the Algebra Nation system
#     so that students and teachers can access Algebra Nation from school computers and personal computers, smartphones, and tablets using their school ID
#     and passwaord.
#     Also, Algebra Nation implementation includes offering training to the mathematics teachers in the school about howw to use Algebra Nation
#     in their class-romms and sending paper copies of the Algebra Nation workbook to the teachers for distribution and use with ther students.
#   - The study's population consisted of high schools, middle / high schools, and senior high schools in all school districts in Florida.
#     Data for this example were collected from February to April 2014 and contain observations for 448 schools.
#     The outcome for this example is the school-level means of the student scores on the spring 2014 Algebra I EOC exam.
# ------------------------------------------------------------------------------


data <- read.csv("algebra_nation.csv")


str(data)


car::some(data)



