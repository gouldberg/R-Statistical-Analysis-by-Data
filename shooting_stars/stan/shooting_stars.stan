data { 
	int<lower=0>  N;    
	int<lower=0> x[N];  
}

parameters { 
	real<lower=0>  lambda; 
}

model { 
	x ~ poisson(lambda); 
}

generated quantities{ 
	real<lower=0,upper=1> p; 
	real sqrt_lambda; 
	p = (exp(-lambda) * pow(lambda, 2) )/2; 
	sqrt_lambda = sqrt(lambda); 
}    

