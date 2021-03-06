// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> Nobs;
  vector[Nobs] yobs;
  int<lower=0> Ncen;
  vector[Ncen] ycen;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real<lower=0> shape;
  real<lower=0> scale;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  shape ~ gamma(2.5, 1);
  scale ~ gamma(1, 0.0001);
  target += weibull_lpdf(yobs | shape, scale);
  target += weibull_lccdf(ycen | shape, scale);
}

