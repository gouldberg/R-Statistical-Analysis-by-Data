data {
	int<lower=0> N;
	real x[N];
}
parameters {
	simplex[4] theta;
	real<lower=0,upper=2*pi()-0.001> mu1;
	real<lower=mu1,upper=2*pi()-0.001> mu2;
	real<lower=mu2,upper=2*pi()-0.001> mu3;
	real<lower=mu3,upper=2*pi()-0.001> mu4;
	real<lower=0> sigma;
}
model {
	real ps[4];
	for(n in 1:N){
		ps[1]=log(theta[1])+von_mises_lpdf(x[n] | mu1, sigma);
		ps[2]=log(theta[2])+von_mises_lpdf(x[n] | mu2, sigma);
		ps[3]=log(theta[3])+von_mises_lpdf(x[n] | mu3, sigma);
		ps[4]=log(theta[4])+von_mises_lpdf(x[n] | mu4, sigma);
		target += log_sum_exp(ps);
	}
	

}
generated quantities{
	real ps2[4];
	vector[N] logLike;
	real V;
	real v;
	for(n in 1:N){
		ps2[1]= log(theta[1])+von_mises_lpdf(x[n] |mu1, sigma);
		ps2[2]= log(theta[2])+von_mises_lpdf(x[n] |mu2, sigma);
		ps2[3]= log(theta[3])+von_mises_lpdf(x[n] |mu3, sigma);
		ps2[4]= log(theta[4])+von_mises_lpdf(x[n] |mu4, sigma);
		logLike[n]=log_sum_exp(ps2);
	}
	V = 1-(modified_bessel_first_kind(1, sigma)/modified_bessel_first_kind(0, sigma));
        v = sqrt(-2*log((modified_bessel_first_kind(1, sigma)/modified_bessel_first_kind(0, sigma)))); 
}
