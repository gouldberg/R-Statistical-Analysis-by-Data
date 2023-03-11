# library(DoE.base)


# ------------------------------------------------------------------
# 直交表の作成
# ------------------------------------------------------------------
generate_sikei_doe <- function(){

  # library(DoE.base)
  # sikei_design <- oa.design(
  #   factor.names = list(
  #     sex = c("male", "female"),
  #     age = c("young", "old"),
  #     n_higaisha = c("one", "over1"),
  #     taiho_hist = c("taiho", "jishu"),
  #     kill_hist = c("planned", "no_planned"),
  #     prev_hist = c("zenka", "no_zenka"),
  #     shobatu_kanjo = c("kanjo_ari", "kanjo_nashi")),
  #   seed=1234)
  # 
  # sikei_doe <- caEncodedDesign(sikei_design)
  
  prof1 <- c(1,1,1,1,1,1,1)
  prof2 <- c(2,1,1,2,2,2,1)
  prof3 <- c(1,2,2,1,2,2,1)
  prof4 <- c(2,1,2,1,1,2,2)
  prof5 <- c(2,2,1,1,2,1,2)
  prof6 <- c(1,1,2,2,2,1,2)
  prof7 <- c(2,2,2,2,1,1,1)
  prof8 <- c(1,2,1,2,1,2,2)
  
  sikei_doe <- data.frame(rbind(prof1, prof2, prof3, prof4, prof5, prof6, prof7, prof8))
  rownames(sikei_doe) <- NULL
  colnames(sikei_doe) <- c("sex", "age", "n_higaisha", "taiho_hist", "kill_hist", "prev_hist", "shobatu_kanjo")

  # ----------
  # 水準ラベルの作成
  sikei_label <- c(
    "male", "female",
    "young", "old",
    "one", "over1",
    "taiho", "jishu",
    "planned", "no_planned",
    "zenka", "no_zenka",
    "kanjo_ari", "kanjo_nashi"
  )

  return(list(sikei_doe=sikei_doe, sikei_label=sikei_label))
}  
  
