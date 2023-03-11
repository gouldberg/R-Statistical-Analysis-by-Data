data{
	int<lower=0> Ny;
	vector<lower=0>[2] y[Ny];
	int<lower=0> Nx;
	real x[Nx];
}

parameters{
	vector[2] mu;
	vector<lower=0>[2] sigma;
	real sig2xy;

	vector[2] mu2;
	vector<lower=0>[2] sigma2;
	real sig2xy2;
}

transformed parameters{
	matrix[2,2] Sigma;
	vector[2] sigsq;
	matrix[2,2] S2;
	vector[2] sigsq2;

	sigsq2[1] = sqrt(sigma2[1]);
	sigsq2[2] = sqrt(sigma2[2]);
	S2[1,1] = sigma2[1];
	S2[2,2] = sigma2[2];
	S2[2,1] = sig2xy2;
	S2[1,2] = sig2xy2;

	sigsq[1] = sqrt(sigma[1]);
	sigsq[2] = sqrt(sigma[2]);
	Sigma[1,1] = sigma[1];
	Sigma[2,2] = sigma[2];
	Sigma[2,1] = sig2xy;
	Sigma[1,2] = sig2xy;
}

model{
	//切断データの相関係数推定
	for(i in 1:Ny){
		y[i] ~ multi_normal(mu2, S2);
	}
	//切断効果の補正
	for(i in 1:Ny){
		y[i] ~ multi_normal(mu, Sigma);
	}
	for(i in 1:Nx){
		x[i] ~ normal(mu[1], sqrt(sigma[1]));
	}
}

generated quantities{
	real<lower=-1,upper=1> rho_truncated; //切断相関係数
	real<lower=-1,upper=1> rho_corrected; //補正された切断相関係数
	rho_truncated = sig2xy2 / (sigsq2[1] * sigsq2[2]);
	rho_corrected = sig2xy / (sigsq[1] * sigsq[2]);
}

