### 「消費者トレンドをつかまえろ」Rコード (公開用)
### 2019/04/26 S.Ono

readRawdata2.1 <- function(BASEDIR){
  # purpose: 観光地名検索量データ取得
  # args: 
  #   BASEDIR: ベース
  # return: 
  #   a list
  # notes:
  #   WindowsではgtrendsRパッケージがうまく動かない
  
  library(gtrendsR)
  library(stringi)
  
  # - - - - -
  sFILENAME = "citylist_2.csv"
  # - - - - -
  
  cat("[readRawdata2.1] Hello\n")
  
  # category 67:Travel
  # print(categories %>% filter(grepl("Travel", name))
  
  # country_code JP: JAPAN
  # print(countries %>% filter(grepl("JAPAN", name)))

  dfCity <- read.csv(
    paste0(BASEDIR, "/", sFILENAME),
    stringsAsFactors = FALSE
  ) 
  
  lOut <- lapply(
    dfCity$sCity,
    function(sCurrent){
      cat("[readRawdata2.1]", sCurrent, "...\n")
      Sys.setlocale(locale="C")
      out <- try(
        gtrends(
          keyword  = sCurrent,
          geo      = "JP",
          time     = "2014-01-01 2018-12-31",
          category = 67
        )
      )
      Sys.setlocale("LC_ALL", 'ja')
      return(out)
    }
  )
  cat("[readRawdata2.1] Done.\n")
  return(lOut)
}
makeData2.1 <- function(lRawdata2){
  # purpose: 観光地名検索量データ2の整形
  # args: 
  #   dfRawdata: generated by readRawdata2.1()
  # return: 
  #   a data frame
  #   行: 週(2014-2018)
  #   $nCityID
  #   $keyword
  #   $date
  #   $hit
  
  cat("[makeData2.1] Hi,\n")
  
  lOut <- lapply(
    seq_along(lRawdata2), 
    function(nCityID){
      out <- lRawdata2[[nCityID]]$interest_over_time
      if (!is.null(out)){
        out <- out %>%
          mutate(
            nCityID = nCityID,
            keyword = iconv(keyword,from="utf8",to="shift_jis")
          )
      }
      return(out)
    }
  )
  abExist <- sapply(lOut, function(x) {!is.null(x)})
  out <- bind_rows(lOut[abExist]) %>%
    dplyr::select(nCityID, date, hits, keyword) %>%
    arrange(nCityID, date) %>%
    group_by(nCityID) %>%
    mutate(
      gSeasonal = as.vector(
        decompose(ts(hits, start=c(2014, 1), frequency = 52), type = "additive")$seasonal  
      ), 
      gAdjusted = hits - gSeasonal
    )
  
  cat("[makeData2.1] Done\n")
  return(out)
}
exportData2.1 <- function(dfData2, WORKDIR){
  # purpose: 観光地名検索量データのCSV出力 (書籍原稿用)
  # args: 
  #   dfData2: generated by makeData2.1()
  #   WORKDIR: 出力先
  
  write.csv(dfData2, paste0(WORKDIR, "/dfData2.csv"), row.names = FALSE)
  
}
makeTable2.1 <- function(dfData2){
  # purpose: 観光地名検索量データ2の集計表を作成
  # args: 
  #   dfData2: generated by makeData2.1()
  # return: 
  #   a data frame
  
  cat("[makeTable2.1] Hello\n")
  
  out <- dfData2 %>%
    group_by(nCityID, keyword) %>%
    summarize(
      nSum = sum(hits), 
      gSD1  = sd(hits), 
      gSD2  = sd(gAdjusted)
    ) %>%
    ungroup()
  ## print(out)
  
  cat("[makeTable2.1] Done\n")
  return(out)
}
makeDataDFA100.1 <- function(dfData, dfTable) {
  # purpose: 観光地名検索量データ2のDFA用データを作成
  # args: 
  #   dfData: generated by makeData2.1()
  #   dfTable: generated by makeTable2.1()
  # return: 
  #   a list of two matrices
  
  cat("[makeDataDFA100.1] Hello\n")
  
  mgData <- dfData %>%
    left_join(dfTable %>% dplyr::select(nCityID, nSum, gSD2), by = "nCityID") %>%
    filter(nSum > 10000, gSD2 > 11.75) %>%
    dplyr::select(date, nCityID, hits) %>%
    spread(nCityID, hits) %>%
    dplyr::select(-date) %>%
    as.matrix(.)
  
  mgCov <- tibble(
    gC1 = sin(2*pi*1*(1:nrow(mgData))/52.14286), 
    gC2 = sin(2*pi*2*(1:nrow(mgData))/52.14286), 
    gC3 = sin(2*pi*3*(1:nrow(mgData))/52.14286), 
    gC4 = sin(2*pi*4*(1:nrow(mgData))/52.14286), 
    gC5 = cos(2*pi*1*(1:nrow(mgData))/52.14286), 
    gC6 = cos(2*pi*2*(1:nrow(mgData))/52.14286), 
    gC7 = cos(2*pi*3*(1:nrow(mgData))/52.14286), 
    gC8 = cos(2*pi*4*(1:nrow(mgData))/52.14286)
  ) %>%
    as.matrix(.)
  
  lOut <- list(
    mgData = mgData, 
    mgCov  = mgCov
  )
  
  cat("[makeDataDFA100.1] Done\n")
  return(lOut)
}
makeDFA108.1 <- function(lDataDFA100, STANDIR){
  # purpose: 8因子DFAモデルの推定
  # args: 
  #   lDataDFA100: generated by makeDataDFA100.1()
  #   STANDIR:     Stanファイルの所在
  # return: 
  #   sub_Estimate_Stan.409()の返し値
  
  lModel <- sub_Estimate_Stan.409 (
    mgData     = lDataDFA100$mgData,
    mgCov      = lDataDFA100$mgCov,
    nNumFactor = 8,
    nChain     = 2,
    nIter      = 3000,
    nWarm      = 1000, 
    sStanDir   = STANDIR
  )
  return(lModel)
}
sub_Estimate_Stan.409 <- function(
  mgData      = mgData,
  mgCov       = mgCov,
  nNumFactor  = 2,
  nIter       = 400,
  nWarm       = 200,
  nChain      = 1, 
  sStanDir    = "."
) {
  # purpose: Stanコード ver.409を実行
  # args: 
  #   mgData:     データ行列
  #   mgCov:      共変量行列
  #   nNumFactor: 因子数
  #   nIter:      iteration
  #   nWarm:      warmup
  #   nChain:     num.chains
  # return: 
  #   Stanオブジェクト
  # notes: 
  #   カレントディレクトリにStanコードがあることを前提にしている
  
  # - - - - 
  sModel <- paste0(STANDIR, "/sub_Estimate_Stan.409.stan")
  # - - - - 
  
  stopifnot(nrow(mgData) == nrow(mgCov))
  
  # mgY: データ(時点が列)
  mgY <- t(mgData)
  # mgC: 共変量(時点が列)
  mgC <- t(mgCov)
  # nNumTime: 時点数
  nNumTime <- ncol(mgY)
  # nNumSeries: 時系列の数
  nNumSeries <- nrow(mgY)
  # nNumFactor: 因子数
  nNumFactor <- nNumFactor 
  # nNumC:共変量の数
  nNumC <- nrow(mgC)
  
  # 標準化
  for (i in 1:nNumSeries) {
    mgY[i, ] = scale(mgY[i, ], center = TRUE, scale = TRUE)
  }
  
  # mbZType: 因子負荷行列と同じサイズの行列
  #          0: 0に固定する場所(因子fの上から(f-1)行)
  #          1: 自由推定する場所
  mbZType <- matrix(1, nNumSeries, nNumFactor)
  for (f in 2:nNumFactor){
    mbZType[1:(f-1), f] <- 0
  }
  
  # mnZIndex: 因子負荷行列と同じサイズの行列
  #           0: 0に固定する場所
  #           1からはじまる連番: 自由推定する場所
  mnZIndex <- matrix(cumsum(mbZType), nrow=nNumSeries) * mbZType
  
  # dfZIndex: 行は因子負荷行列の要素
  dfZIndex <- crossing(nRow = 1:nrow(mbZType), nCol = 1:ncol(mbZType))
  anType  <- mbZType[as.matrix(dfZIndex)]
  dfZIndex <- dfZIndex %>%
    mutate(nType = anType) %>%
    arrange(nType, nCol, nRow) %>%
    group_by(nType) %>%
    mutate(nID = seq_along(nType)) %>%
    ungroup() 
  
  mnZLoc0 <- dfZIndex %>% filter(nType == 0) %>% dplyr::select(nRow, nCol) %>% as.matrix(.)
  mnZLoc1 <- dfZIndex %>% filter(nType == 1) %>% dplyr::select(nRow, nCol) %>% as.matrix(.)
  nNumZ0 <- nrow(mnZLoc0)
  nNumZ1 <- nrow(mnZLoc1)
  
  lData <- list(
    nNumTime,
    nNumSeries,
    nNumFactor,
    nNumZ0,
    nNumZ1,
    mnZLoc0, 
    mnZLoc1,
    mgY,
    nNumC,
    mgC
  )
  asPars <- c("mgX", "mgZ", "gSigma", "mgBeta", "mgPred", "log_lik")
  
  lStan <- rstan::stan(
    data    = lData,
    file    = sModel,
    pars    = asPars,
    chains  = nChain,
    iter    = nIter,
    warmup  = nWarm,
    control = list(adapt_delta = 0.99, max_treedepth = 15),
    refresh = 50,
    verbose = TRUE,
    seed    = 123
  )
  return(lStan)
}
sub_Rotate_Stan.1 <- function(lModel){
  # purpose: 
  #   DFAの因子を回転する
  # args:
  #   lModel: generated by Sub_Estimate_Stan.*()
  # return: 
  #   a list of matrices
  
  cat("[sub_Rotate_Stan.1] Hello\n")
  
  # nNumMCMC, nNumSeries, nNumFactor
  mgZ <- rstan::extract(lModel)$mgZ
  # nNumMCMC, nNumFactor, nNumTime
  mgX <- rstan::extract(lModel)$mgX
  
  cat("[sub_Rotate_Stan.1] mean -> rotation ...\n")
  
  mgZ_mean <- apply(mgZ, c(2, 3), mean)
  mgX_mean <- apply(mgX, c(2, 3), mean)
  mgRotMat <- stats::varimax(mgZ_mean)$rotmat
  mgZ_mean_rotated <- mgZ_mean %*% mgRotMat
  mgX_mean_rotated <- solve(mgRotMat) %*% mgX_mean
  
  cat("[sub_Rotate_Stan.1] rotation -> mean ...\n")
  
  nNumMCMC   <- dim(mgZ)[1]
  nNumSeries <- dim(mgZ)[2]
  nNumFactor <- dim(mgZ)[3]
  nNumTime   <- dim(mgX)[3]
  
  mgZ_rotated <- array(0, dim = c(nNumMCMC, nNumSeries, nNumFactor))
  mgX_rotated <- array(0, dim = c(nNumMCMC, nNumFactor, nNumTime))
  
  for(i in 1:nNumMCMC) {
    mgCurrentZ <- mgZ[i,,]
    mgCurrentX <- mgX[i,,]
    
    mgRotMat <- stats::varimax(mgCurrentZ)$rotmat
    mgCurrentZ_rotated <- mgCurrentZ %*% mgRotMat
    mgCurrentX_rotated <- solve(mgRotMat) %*% mgCurrentX
    
    mgZ_rotated[i,,] <- mgCurrentZ_rotated
    mgX_rotated[i,,] <- mgCurrentX_rotated
  }
  
  mgZ_rotated_mean <- apply(mgZ_rotated, c(2,3), mean)
  mgX_rotated_mean <- apply(mgX_rotated, c(2,3), mean)
  
  lOut <- list(
    mgZ_mean         = mgZ_mean, 
    mgX_mean         = mgX_mean, 
    mgZ_mean_rotated = mgZ_mean_rotated, 
    mgX_mean_rotated = mgX_mean_rotated, 
    mgZ_rotated      = mgZ_rotated, 
    mgX_rotated      = mgX_rotated, 
    mgZ_rotated_mean = mgZ_rotated_mean,
    mgX_rotated_mean = mgX_rotated_mean
  )
  cat("[sub_Rotate_Stan.1] Done.\n")
  return(lOut)
}


tabDFARot108.2 <- function(mgZ_mean_rotated, dfTable2, WORKDIR){
  # purpose: 
  #   各因子に対して負荷を持つ観光地名の表を出力
  # args: 
  #   mgZ_mean_rotated: 回転後因子負荷行列
  #   dfTable2:         generated by makeTable2.1()
  #   WORKDIR:          出力先
  
  out1 <- dfTable2 %>%
    filter(nSum > 10000, gSD2 > 11.75) 
  colnames(mgZ_mean_rotated) <- paste0("F", 1:8)
  
  out <- cbind(out1, mgZ_mean_rotated) %>% 
    dplyr::select(keyword, starts_with("F")) %>%
    gather(sVar, gValue, starts_with("F")) %>%
    mutate(gValue = as.numeric(gValue)) %>%
    filter(abs(gValue) > 0.2) %>%
    mutate(sLabel = sprintf("%s(+%3.2f)", keyword, gValue)) %>%
    group_by(sVar) %>%
    summarize(sLabel = paste0(sLabel, collapse = ",")) %>%
    ungroup()
  
  print(out)
  write.csv(out, paste0(WORKDIR, "/tabDFARot108.2.csv"), row.names = FALSE)
}
plotData2.2 <- function(dfData2, GRAPHDIR){
  # purpose: 検索量時系列チャート
  # args: 
  #   dfData2:  generated by makeData2.1()
  #   GRAPHDIR: 出力先
  
  # - - - 
  anTarget <- c(100, 366, 8)  # 秋葉原, 屋久島, 江差
  # - - - 
  
  # データを絞って
  dfPlot <- dfData2 %>%
    filter(nCityID %in% anTarget) 
  
  # ラベルのベクトルをつくり
  dfLabel <- dfPlot %>%
    dplyr::select(nCityID, keyword) %>%
    distinct()
  asLabel <- dfLabel$keyword
  names(asLabel) <- dfLabel$nCityID
  
  # 因子に変える
  dfPlot <- dfPlot %>%
    mutate(fKeyword = factor(keyword, levels=asLabel[as.character(anTarget)]))
  
  g <- ggplot(dfPlot, aes(x = date, y = hits))
  g <- g + geom_line()
  g <- g + facet_wrap(fKeyword ~ .)
  g <- g + labs(x = "週次", y = "検索量")
  g <- g + theme_bw()
  g <- g + theme_bw(base_family = "MeiryoUI")
  print(g)
  ggsave(g, file = paste0(GRAPHDIR, "/plotData2_2.png"), width = 12, height = 5, units = "cm")
}
plotTable2.1 <- function(dfTable2, GRAPHDIR){
  # purpose: 検索量の合計と変動の分布
  # args: 
  #   dfTable2: generated by makeTable2.1()
  #   GRAPHDIR: 出力先
  
  library(ggrepel)
  
  g <- ggplot(data = dfTable2, aes(x = nSum, y = gSD2, label = keyword))
  g <- g + geom_rect(xmin = 10000, xmax = 20000, ymin = 11.75, ymax = 25, fill = "gray", alpha = 0.1)
  g <- g + geom_point(size = 1, alpha = 1)
  g <- g + geom_text(size = 2.5, hjust = "left", alpha = 0.5, nudge_x = 200)
  
  g <- g + labs(x = "検索量合計", y = "季節調整後の検索量SD")
  g <- g + theme_bw(base_family = "MeiryoUI")
  print(g)
  ggsave(g, file = paste0(GRAPHDIR, "/plotTable2_1.png"), width = 12, height = 8, units = "cm")
  
}


plotDFArot108.1 <- function(lDFARot108, dfData2, GRAPHDIR){
  # purpose: 抽出された因子時系列
  # args: 
  #   lDFARot108: generated by makeDFA108.1()
  #   dfData2:    generated by makeData2.*()
  #   GRAPHDIR:   出力先
  
  dfX <- as.data.frame(t(lDFARot108$mgX_mean_rotated))
  colnames(dfX) <- paste0("F", 1:8)
  dfX <- dfX %>%
    mutate(dDate = sort(unique(dfData2$date)))  %>%
    gather(sVar, gValue, -dDate) %>%
    mutate(sLabel = sVar, bFactor = 1) %>%
    dplyr::select(dDate, sLabel, gValue, bFactor)
  
  g <- ggplot(dfX, aes(x = dDate, y = gValue))
  g <- g + geom_hline(yintercept = 0, color ="gray")
  g <- g + geom_line()
  g <- g + facet_wrap(sLabel ~ ., ncol = 4)
  g <- g + labs(x = "週次", y = "因子得点")
  g <- g + theme_bw(base_family = "MeiryoUI")
  print(g)
  ggsave(g, file = paste0(GRAPHDIR, "/plotDFArot108_1.png"), width = 12, height = 8, units = "cm")
}
plotDFArot108.2 <- function(lDFARot108, dfData2, dfTable2, GRAPHDIR){
  # purpose: F1に関連する観光地名
  # args: 
  #   lDFARot108: generated by makeDFA108.1()
  #   dfData2:    generated by makeData2.*()
  #   dfTable2:   generated by makeTable2.*()
  #   GRAPHDIR:   出力先
  
  dfZ <- as.data.frame(lDFARot108$mgZ_mean_rotated)
  colnames(dfZ) <- paste0("F", 1:8)
  
  dfTable2 <- dfTable2 %>%
    filter(nSum > 10000, gSD2 > 11.75) 
  
  dfZ <- bind_cols(dfTable2, dfZ) %>%
    filter(abs(F1) > 0.2, abs(F4) < 0.2) %>%
    top_n(3, F1) %>%
    mutate(sLabel = sprintf("%s(%+3.2f)", keyword, F1)) %>%
    dplyr::select(nCityID, sLabel)
  
  dfPlot <- dfData2 %>%
    filter(nCityID %in% dfZ$nCityID) %>%
    mutate(fCityID = factor(nCityID, levels=dfZ$nCityID, labels=dfZ$sLabel))
  
  g <- ggplot(dfPlot, aes(x = date, y = hits))
  g <- g + geom_line()
  g <- g + facet_wrap(fCityID ~ .)
  g <- g + labs(x = "週次", y = "検索量")
  g <- g + theme_bw()
  g <- g + theme_bw(base_family = "MeiryoUI")
  print(g)
  ggsave(g, file = paste0(GRAPHDIR, "/plotDFArot108_2.png"), width = 12, height = 5, units = "cm")
}
plotDFArot108.3 <- function(lDFARot108, dfData2, dfTable2, GRAPHDIR){
  # purpose: F4に関連する観光地名
  # args: 
  #   lDFARot108: generated by makeDFA108.1()
  #   dfData2:    generated by makeData2.*()
  #   dfTable2:   generated by makeTable2.*()
  #   GRAPHDIR:   出力先
  
  anTARGET <- c(38, 306, 334)
  
  dfZ <- as.data.frame(lDFARot108$mgZ_mean_rotated)
  colnames(dfZ) <- paste0("F", 1:8)
  
  dfTable2 <- dfTable2 %>%
    filter(nSum > 10000, gSD2 > 11.75) 
  
  dfZ <- bind_cols(dfTable2, dfZ) %>%
    filter(abs(F4) > 0.2) %>%
    filter(nCityID %in% anTARGET) %>%
    mutate(sLabel = sprintf("%s(%+3.2f)", keyword, F4)) %>%
    dplyr::select(nCityID, sLabel)
  
  dfPlot <- dfData2 %>%
    filter(nCityID %in% dfZ$nCityID) %>%
    mutate(fCityID = factor(nCityID, levels=dfZ$nCityID, labels=dfZ$sLabel))
  
  g <- ggplot(dfPlot, aes(x = date, y = hits))
  g <- g + geom_line()
  g <- g + facet_wrap(fCityID ~ .)
  g <- g + labs(x = "週次", y = "検索量")
  g <- g + theme_bw()
  g <- g + theme_bw(base_family = "MeiryoUI")
  print(g)
  ggsave(g, file = paste0(GRAPHDIR, "/plotDFArot108_3.png"), width = 12, height = 5, units = "cm")
  
}
# = = = = = = = = = = = 
# ここから本文

library(tidyverse)
library(lubridate)
library(readxl)
library(scales)
library(assertr)
library(beepr)

library(rstan)
rstan_options(auto_write = TRUE)
Sys.setenv(LOCAL_CPPFLAGS = '-march=native')
options(mc.cores = parallel::detectCores())

if (Sys.info()["sysname"] == "Windows") windowsFonts(MeiryoUI = "Meiryo UI")

BASEDIR     <- "C:/work"
GRAPHDIR    <- "C:/work"
WORKDIR     <- "C:/work"
STANDIR     <- "C:/work"

### 参考: データの取得と整形
### s.lRawdata2 <- readRawdata2.1 (WORKDIR)
### s.dfData2   <- makeData2.1 (s.lRawdata2)
### exportData2.1(s.dfData2, WORKDIR)

# 整形後データの読み込み
s.dfData2 <- read_csv(
  paste0(WORKDIR, "/dfData2.csv"),
  locale = locale(encoding = "cp932")
)

# 集計
s.dfTable2 <- makeTable2.1 (s.dfData2)

# 図1
plotData2.2(s.dfData2, GRAPHDIR)

# 図2
plotTable2.1 (s.dfTable2, GRAPHDIR)

# 動的因子分析のための整形
s.lDataDFA100  <- makeDataDFA100.1 (s.dfData2, s.dfTable2)

# 動的因子分析
s.lDFA108 <- makeDFA108.1 (s.lDataDFA100, STANDIR)

RDATADIR    <- "C:/Users/ono/OneDrive/Study/201904DFA/RData"
s.lDFA108 <- readRDS(file = paste0(RDATADIR, "/s.lDFA108.rds"))

# 回転
s.lDFARot108 <- sub_Rotate_Stan.1(s.lDFA108)

# 表1
tabDFARot108.2 (s.lDFARot108$mgZ_mean_rotated, s.dfTable2, WORKDIR)

# 図3
plotDFArot108.1 (s.lDFARot108, s.dfData2, GRAPHDIR)

# 図4
plotDFArot108.2 (s.lDFARot108, s.dfData2, s.dfTable2, GRAPHDIR)

# 図5
plotDFArot108.3 (s.lDFARot108, s.dfData2, s.dfTable2, GRAPHDIR)

