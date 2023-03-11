data { 
	int<lower=1> N;
	int<lower=1> ntrials;
	int<lower=1> maxpump;
	vector<lower=0,upper=1>[ntrials] p;
	int<lower=1> options[N,ntrials];
	int d[ntrials, maxpump,N];
}

parameters {
	vector<lower=0,upper=10>[N] gamma;
	vector<lower=0,upper=10>[N] beta;
	real mu_g;
	real<lower=0> sigma_g;
	real mu_b;
	real<lower=0> sigma_b;
}

transformed parameters {
	vector<lower=0>[ntrials] omega[N];

	for(i in 1:N){
		for(j in 1:ntrials){
			omega[i,j] = -gamma[i] / log1m(p[j]);
		}
	}
}

model {
	for(i in 1:N){
		for (j in 1:ntrials) {
			for (k in 1:options[i,j]) {
				real theta;
				theta = 1 - inv_logit(-beta[i] * (k - omega[i,j]));
				d[j][k,i] ~ bernoulli(theta);
			}
		}
	}
	gamma ~ normal(mu_g,sigma_g);
	beta ~ normal(mu_b,sigma_b);
}
