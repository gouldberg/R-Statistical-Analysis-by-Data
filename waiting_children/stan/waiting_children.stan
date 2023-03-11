data{
	int<lower=0> N;
	int<lower=0> S[N];
	int<lower=0> x[N];
}
parameters{
	real<lower=0,upper=1> mu;
	real<lower=0.01> kap;
	real<lower=0,upper=1-0.0001> theta[N];
}
transformed parameters{
	real<lower=0> a;
	real<lower=0> b;
	a = mu * kap;
	b = kap - a;
}
model{
	kap ~ pareto(0.1, 1.5);
	for(i in 1:N){
		theta[i] ~ beta(a, b);
		x[i] ~ binomial(S[i],theta[i]);
	}
}
generated quantities{
	real sig;
	sig = a * b / ((a + b)^2 * (a + b + 1));
}

