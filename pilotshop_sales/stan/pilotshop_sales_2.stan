data{
	int<lower=0> Ny;
	vector<lower=0>[2] y[Ny];
}

parameters{
	vector[2] mu3;
	vector<lower=0>[2] sigma3;
	real sig2xy3;
}

transformed parameters{
	matrix[2,2] S3;
	vector[2] sigsq3;
	sigsq3[1] = sqrt(sigma3[1]);
	sigsq3[2] = sqrt(sigma3[2]);
	S3[1,1] = sigma3[1];
	S3[2,2] = sigma3[2];
	S3[2,1] = sig2xy3;
	S3[1,2] = sig2xy3;
}

model{
	for(i in 1:Ny){
		y[i] ~ multi_normal(mu3, S3);
	}
}

generated quantities{
	real<lower=-1,upper=1> rho_complete;
	rho_complete = sig2xy3 / (sigsq3[1] * sigsq3[2]);
}
