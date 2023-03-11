// sub_fitDFA.1.stan からの変更履歴：
// sub_Estimate_Stan.401 共変量処理を削除し引数をリネーム
// sub_Estimate_Stan.402 因子負荷行列の引き渡しを変更
// sub_Estimate_Stan.403 データの引き渡しを変更
// sub_Estimate_Stan.404 分散をgSigmaのみに
// sub_Estimate_Stan.405 targetを使わない 
// sub_Estimate_Stan.406
// sub_Estimate_Stan.407
// sub_Estimate_Stan.408 
// sub_Estimate_Stan.409 事前分布を変更
data {
  int<lower=0> nNumTime;    // 時点数
  int<lower=0> nNumSeries;  // 時系列の数
  int<lower=0> nNumFactor;  // 因子の数
  int<lower=0> nNumZ0;      // 因子負荷行列のうち推定しない要素の数
  int<lower=0> nNumZ1;      // 因子負荷行列のうち自由推定する要素の数
  int<lower=0> mnZLoc0[nNumZ0, 2]; // 因子負荷行列のうち推定しない要素の(行番号, 列番号)
  int<lower=0> mnZLoc1[nNumZ1, 2]; // 因子負荷行列のうち自由推定する要素の(行番号, 列番号)
  matrix [nNumSeries, nNumTime] mgY; 
  int<lower=0> nNumC;
  matrix [nNumC, nNumTime] mgC;  
}
parameters {
  matrix[nNumFactor,nNumTime] mgX;     // 因子時系列
  matrix[nNumSeries, nNumC]   mgBeta;  // 共変量の係数
  vector[nNumZ1]              agZfree; // 因子負荷行列のうち自由推定する要素
  vector<lower=0>[nNumFactor] agZposi; // 因子負荷行列のうち自由推定する要素(正値制約つき)
  real<lower=0>               gSigma;  // 観察時系列の撹乱項の分散
}
transformed parameters {
  matrix[nNumSeries,nNumTime]   mgPred; // 予測値
  matrix[nNumSeries,nNumFactor] mgZ;    // 因子負荷行列
  // 因子負荷行列を作成する
  for(i in 1:nNumZ0) {
    mgZ[mnZLoc0[i,1],mnZLoc0[i,2]] = 0;
  }
  for(i in 1:nNumZ1) {
    mgZ[mnZLoc1[i,1],mnZLoc1[i,2]] = agZfree[i]; 
  }
  for(k in 1:nNumFactor) {
    mgZ[k,k] = agZposi[k];
  }
  // 予測値を作成する
  mgPred = mgZ * mgX + mgBeta * mgC;
}
model {
  // 因子時系列
  for(k in 1:nNumFactor) {
    mgX[k,1] ~ normal(0, 1);
    for(t in 2:nNumTime) {
      mgX[k,t] ~ normal(mgX[k,t-1], 1);
    }
  }
  // 因子負荷
  agZfree ~ normal(0, 1);
  agZposi ~ normal(0, 1);
  // 共変量の係数
  // to_vector(mgBeta) ~ normal(0, 1);
  // 撹乱項の分散
  gSigma ~ student_t(3, 0, 2);
  // 尤度
  to_vector(mgY) ~ normal(to_vector(mgPred), gSigma);
}
generated quantities {
  vector[nNumSeries] log_lik;
  for(i in 1:nNumSeries) {
    log_lik[i] = normal_lpdf(mgY[i] | mgPred[i], gSigma);
  }
}

