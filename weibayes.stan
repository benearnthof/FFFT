// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] y;
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
  shape ~ gamma(0.0001, 0.0001);
  scale ~ gamma(0.0001, 0.0001);
  y ~ weibull(shape, scale);
}

