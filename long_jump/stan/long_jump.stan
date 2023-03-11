data { 
	int<lower=0> N;
	real<lower=0> x[N];
}

parameters {
	real<lower=0>   mu;
	real<lower=0>	sigma;
}

model {
	for (i in 1:N)
		x[i] ~ gumbel(mu, sigma);
}

generated quantities{
	real	s;          //SD
	real	p_w;        //世界記録は何パーセント点か
	real	x_99;       //99パーセント点
	real	u_rover100; //再現期間が100年より長い確率
	real	r_w;        //世界記録の再現レベル(1/再現期間)
	real	xpred;      //xの予測値
	real	p_new;      //予測値>8.95の確率
	s = sqrt(pow(pi(), 2) * pow(sigma, 2)/6);
	p_w = exp(-exp((mu - 8.95) / sigma));
	x_99 = mu - sigma * (log(-log(0.99)));
	u_rover100 = 8.95 > x_99 ? 1 : 0;
	r_w = 1 / (1 - exp(-exp((mu - 8.95) / sigma)));
	xpred = gumbel_rng(mu, sigma);
	p_new = xpred > 8.95 ? 1: 0;
}
