
data{
  int k ; // the number of data points
  real y[k] ; // the data as a vector of length k
}
parameters {
  real mu ; // define the parameter mu
  real <lower=0> sigma ; // define the parameter sigma, noting that it has a lower bound of zero
}
model{
mu ~ normal(0,10) ; // set the prior for mu
sigma ~ exponential(1) ; // set the prior for sigma
y ~ normal(mu,sigma) ; // calculate the likelihood of the data, y, given the parameters, mu and sigma
}    

