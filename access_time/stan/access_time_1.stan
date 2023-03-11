data {
	int<lower=0> N;
	real x[N];
}
parameters {
	simplex[2] theta;
	real<lower=0,upper=2*pi()-0.001> mu1;
	real<lower=mu1,upper=2*pi()-0.001> mu2;
	real<lower=0> sigma;
}
model {
	real ps[2];
	for(n in 1:N){
		ps[1]=log(theta[1])+von_mises_lpdf(x[n]| mu1, sigma);
		ps[2]=log(theta[2])+von_mises_lpdf(x[n]| mu2, sigma);
		target += log_sum_exp(ps);
	}
	

}
generated quantities{
	real ps2[2];
	vector[N] logLike;
	for(n in 1:N){
		ps2[1]= log(theta[1])+von_mises_lpdf(x[n]| mu1, sigma);
		ps2[2]= log(theta[2])+von_mises_lpdf(x[n]| mu2, sigma);
		logLike[n]= log_sum_exp(ps2);
	}
}
