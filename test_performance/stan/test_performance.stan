data{
	int<lower=0>  N; //人数
	vector[3] x[N];
}

parameters{
	vector[3] mu;
	vector<lower=0>[3] sigma;
	corr_matrix[3] rho;
}

transformed parameters{
	vector<lower=0>[3] sig2;
	matrix[3,3] Sigma;
	/*
	sig2[1] = pow(sigma[1], 2);
	sig2[2] = pow(sigma[2], 2);
	sig2[3] = pow(sigma[3], 2);
	Sigma[1,1] = sig2[1];
	Sigma[2,2] = sig2[2];
	Sigma[3,3] = sig2[3];
	Sigma[2,1] = sigma[2] * sigma[1] * rho[2,1];
	Sigma[3,1] = sigma[3] * sigma[1] * rho[3,1];
	Sigma[3,2] = sigma[3] * sigma[2] * rho[3,2];
	*/
	for(i in 1:3){
		sig2[i] = pow(sigma[i], 2);
	}
	Sigma = diag_matrix(sigma) * rho * diag_matrix(sigma);
}

model{
	for(i in 1:N){
		x[i] ~ multi_normal(mu, Sigma);
	}
}

generated quantities{
	real delta_r2;
	real delta_r2_over;
	delta_r2 = rho[3,2] - rho[2,1];
	delta_r2_over = step(delta_r2);
}
