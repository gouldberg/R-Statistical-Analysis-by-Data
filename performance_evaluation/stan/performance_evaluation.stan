data{
	int<lower=0>  N; //人数
	vector[2] xA[N]; //A
	vector[2] xB[N]; //B
}

parameters{
	vector[2] muA;
	vector<lower=0>[2] sigmaA;
	real<lower=-1,upper=1> rhoA;
	vector[2] muB;
	vector<lower=0>[2] sigmaB;
	real<lower=-1,upper=1> rhoB;
}

transformed parameters{
	vector<lower=0>[2] sig2A;
	matrix[2,2] SigmaA;
	vector<lower=0>[2] sig2B;
	matrix[2,2] SigmaB;

	sig2A[1] = pow(sigmaA[1], 2);
	sig2A[2] = pow(sigmaA[2], 2);
	SigmaA[1,1] = sig2A[1];
	SigmaA[2,2] = sig2A[2];
	SigmaA[1,2] = sigmaA[1] * sigmaA[2] * rhoA;
	SigmaA[2,1] = sigmaA[1] * sigmaA[2] * rhoA;

	sig2B[1] = pow(sigmaB[1], 2);
	sig2B[2] = pow(sigmaB[2], 2);
	SigmaB[1,1] = sig2B[1];
	SigmaB[2,2] = sig2B[2];
	SigmaB[1,2] = sigmaB[1] * sigmaB[2] * rhoB;
	SigmaB[2,1] = sigmaB[1] * sigmaB[2] * rhoB;
}

model{
	for(i in 1:N){
		xA[i] ~ multi_normal(muA, SigmaA);
		xB[i] ~ multi_normal(muB, SigmaB);
	}
}

generated quantities{
	real delta_r;
	real delta_r_over;
	#real delta_r_over2;
	delta_r = rhoB - rhoA;
	delta_r_over = step(delta_r);
	#delta_r_over2 = delta_r > 0.1 ? 1:0;
}

