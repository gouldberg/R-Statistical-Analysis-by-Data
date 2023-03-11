data { 
  int<lower=0> n;                             //データ数   
  vector[2] x[n];                             //データ     
  real EQU;                                   //ＳＤ、1 - 共通、0 - 異なる
  real mL;real mH;real sL;real sH;            //事前分布
}
parameters {
  vector<lower=mL,upper=mH> [2] mu;          //平均(範囲指定)
  real<lower=sL,upper=sH> sigma1;            //標準偏差(範囲指定)
  real<lower=sL,upper=sH> dummy;             //ダミーのＳＤ(範囲指定)
  real<lower=-1,upper=1>  rho;                //相関
}
transformed parameters {
  real<lower=0>           sigma2;             //標準偏差2
  cov_matrix[2]           Sigma;
  sigma2 =EQU>0.5 ? sigma1 : dummy;
  Sigma[1,1]  = pow(sigma1,2);                //共分散行列
  Sigma[2,2]  = pow(sigma2,2);
  Sigma[1,2]  = sigma1*sigma2*rho;
  Sigma[2,1]  = Sigma[1,2];
}
model {
  for(i in 1:n){x[i]~multi_normal(mu,Sigma);} //2変量正規分布
}
generated quantities{
  vector[2] xaste;
  real log_lik;
  xaste  =  multi_normal_rng(mu,Sigma);       //予測分布
  log_lik  = multi_normal_lpdf(x | mu, Sigma);  //対数尤度
}
