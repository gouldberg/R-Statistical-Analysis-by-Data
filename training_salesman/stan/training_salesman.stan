data { 
	int<lower=0>  N;    
	vector[2] x[N];       
}

parameters {
	vector[2]           mu;
	vector<lower=0>[2]  sigma;
	real<lower=-1,upper=1>    rho;
}

transformed parameters {
	vector<lower=0>[2] sigmasq;
	matrix[2,2]      Sigma;

	sigmasq[1] = pow(sigma[1],2);
	sigmasq[2] = pow(sigma[2],2);
	Sigma[1,1] = sigmasq[1];
	Sigma[2,2] = sigmasq[2];
	Sigma[1,2] = sigma[1] * sigma[2] * rho;
	Sigma[2,1] = sigma[1] * sigma[2] * rho;
}

model {
	for(i in 1:N){
		x[i] ~ multi_normal(mu, Sigma);
	}
}

generated quantities{
	real delta;
	real delta_over;
	real delta_over2;
	real rho_over;
	real rho_over05;

	delta = mu[2] - mu[1];
	delta_over = step(delta);
	delta_over2 = delta > 2 ? 1 : 0;
	rho_over = step(rho);
	rho_over05 = rho > 0.5 ? 1 : 0;
}
