//
// exponential regression model to estimate individual functional responses
//

// data

data {
  int<lower = 1> N;
  int<lower = 1> R[N];
  real<lower = 0> time[N];
}

// parameters

parameters {
  real<lower = 0> a;
  real<lower = 0, upper = min(time)> h;
}

// model

model {
  for (i in 1:N){
    time[i] ~ exponential(a*R[i]) T[h,];
  }
  a ~ cauchy(10, 10);
  h ~ cauchy(0,5);
}

// posterior predictive distribution 

generated quantities{
  real ypred[max(R)];
  for (i in 1:max(R)){
    ypred[i] = exponential_rng(a*i) + h;
  }
}
