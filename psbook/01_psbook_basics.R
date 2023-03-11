setwd("//media//kswada//MyFiles//R//psbook")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PSbook_data.csv
#   - 架空のある疾患で入院した　15,000人のデータ
#     Age:　入院時年齢
#     HT:　高血圧　(1:あり　0:なし)
#     DM:　糖尿病　(1:あり　0:なし)
#     Stroke:　脳卒中の既往　(1:あり　0:なし)
#     MI:　心筋梗塞の既往　(1:あり　0:なし)
#     TreatmentX:　治療薬Xの使用　(1:あり　0:なし)
#     sequela:　退院時後遺症　(1:あり　0:なし)
#     ADL_disc:　退院時ADLスコア　(1:あり　0:なし)
#
#     sex, Age, HT, DM, Stroke, MI が　交絡因子
#     TreatmentX が治療の割当変数
#     sequela, ADL_disc がアウトカムに相当
# ------------------------------------------------------------------------------

ps.df <- read.csv(file = "//media//kswada//MyFiles//references//できる傾向スコア分析//PSbook_data.csv", header = TRUE, sep = ",", fileEncoding = "Shift-JIS")

str(ps.df)
glimpse(ps.df)

