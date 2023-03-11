data {
	int<lower=1> ntrials;
	int<lower=1> maxpump;
	vector<lower=0,upper=1> [ntrials] p;
	int<lower=1> options[ntrials];
	int d[ntrials, maxpump];
}

parameters {
	real<lower=0,upper=10> gamma;
	real<lower=0,upper=10> beta;
} 

transformed parameters {
	vector<lower=0>[ntrials] omega;

	for(j in 1:ntrials){
		omega[j] = -gamma / log1m(p[j]);
	}
}

model {
	for (j in 1:ntrials) {
		for (k in 1:options[j]) {
			real theta;
			theta = 1 - inv_logit(-beta * (k - omega[j]));
			d[j,k] ~ bernoulli(theta);
		}
	}
}
