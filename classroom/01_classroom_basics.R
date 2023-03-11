
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\classroom")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  SII Project, classroom
#   - The SII (Study of Instructional Improvement Project) was carried out to assess the match achievement scores
#     of first- and third-grade pupils in randomly selected classromms from a national US sample of elementary schools
#     The dataset includes results for 1,190 first-grade pupils sampled from 312 classrooms in 107 schools.
#   - The SII data exhibit a hierarchical structure. That is, pupils are grouped in classes, which, in turn,
#     are grouped within schools.
#   - School-level variables:
#       - schoolid:  school's ID number
#       - housepov:  % of households in the neighborhood of the school below the poverty level
#       - classid:  classroom's ID number
#       - yearstea:  years of teacher's experience in teaching in the first grade
#       - mathprep:  the number of preparatory courses on the first-grade math contents and methods followed by the teacher
#       - mathknow:  teacher's knowledge of the first-grade math contents
#                    (higher values indicate a higher knowledge of the contents)
#   - Pupil-level variables:
#       - childid:  pupil7s ID number
#       - mathgain:  pupil's gain in the math achievement score from the spring of kindergarten to the spring of first grade
#       - mathkind:  pupil's math score in the spring of the kindergarten year
#       - sex:  an indicator variable for sex
#       - minority:  an indicator variable for the minority status
#       - ses:  pupil's socioeconomic status
# ------------------------------------------------------------------------------


data(classroom, package = "WWGbook")


str(classroom)


car::some(classroom)




# ------------------------------------------------------------------------------
# data check
# ------------------------------------------------------------------------------


summary(classroom)



# -->
# there are 109 NA7s mathknow



Hmisc::describe(classroom)




# ------------------------------------------------------------------------------
# convert to factor variable
# ------------------------------------------------------------------------------

SIIdata <- within(classroom,
                  {
                    sex <- factor(sex, levels = c(0,1), labels = c("M", "F"))
                    minority <- factor(minority, labels = c("Mnrt:No", "Mnrt:Yes"))
                    schoolid <- factor(schoolid)
                    classid <- factor(classid)
                    childid <- factor(childid)
                  })


str(SIIdata)


