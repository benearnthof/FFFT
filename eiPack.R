# eiPack
install.packages("eiPack")
library(eiPack)

# http://docs.zeligproject.org/articles/zeligei_eirxc.html
# https://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/20090023159.pdf
# http://soft.vub.ac.be/Publications/2012/vub-soft-tr-12-13.pdf
# arXiv:2005.01285
# Richard.MUELLER@mtu.de
# stefan.gehring@mtu.de
# rebecca.huber@mtu.de

install.packages("zeligverse")
# https://stats.stackexchange.com/questions/44144/multinomial-dirichlet-model-with-hyperprior-distribution-on-the-concentration-pa
# http://www.stat.columbia.edu/~gelman/book/
# https://www.imsbio.co.jp/RGM/R_rdfile?f=DirichletMultinomial/man/dmn.Rd&d=R_BC

library(eiPack)
data(senc)
z <- ei.MD.bayes(cbind(dem, rep, non) ~ cbind(black, white, natam),
                 total = "total", data = senc, verbose = 1)
cover.plot(z, row = "black", column = "dem")

r <- ei.reg(cbind(dem, rep, non) ~ cbind(black, white, natam),
            total = "total", data = senc)

# lets try simulating data and estimating manually
install.packages("MultiRNG")
library(MultiRNG)
set.seed(1)
test <- draw.dirichlet.multinomial(100, 3, c(1,3,4), beta = 2, N = 1000)
colnames(test) <- c("white", "black", "natam")
t2 <- draw.dirichlet.multinomial(100, 3, c(1,3,4), beta = 2, N = 1000)
colnames(t2) <- c("dem", "rep", "non")
dta <- cbind(test, t2)
dta <- as.data.frame.matrix(dta)
dta$total <- 1000

mod <- ei.MD.bayes(cbind(dem, rep, non) ~ cbind(black, white, natam),
                   total = "total", data = dta, verbose = 1)

set.seed(1)
dir <- draw.dirichlet(100, 3, c(1, 3, 4), 2)
mlt <- draw.multinomial(100, 3, c(1/8, 3/8, 4/8), 1000)

colnames(dir) <- c("dem", "rep", "non")
colnames(mlt) <- c("white", "black", "natam")

dta <- cbind(dir, mlt)
dta <- as.data.frame.matrix(dta)
dta$total <- 1000

mod <- ei.MD.bayes(cbind(dem, rep, non) ~ cbind(black, white, natam),
                   total = "total", data = dta, verbose = 1)

s <- summary(mod)
cnts <- s$draws$Cell.counts
cnts <- as.data.frame.matrix(cnts)

means <- cnts$Mean
mat <- matrix(nrow = 3, ncol = 3)
mat[] <- means
rownames(mat) <- c("black", "white", "natam")
colnames(mat) <- c("dem", "rep", "non")

# seems to work, should be applicable in our case. Need to pay attention to chosen 
# priors etc
# also need to pay close attention to model specification as the stuff above was 
# pretty much chosen arbitrarily.

library(mgcv)
library(brms)
dat <- mgcv::gamSim(2, n = 1000, scale = 0.3)$data
fit <- brm(y ~ gp(x, z, k = 5, c = 5/4), data = dat, chains = 4, cores = 4)
conditional_effects(fit, "x:z")
conditional_effects(fit, "x:z", surface = TRUE, resolution = 30)

library(brms)
library(rstan)

# Bayesian inference of random durations using Weibull likelihood with diffuse 
# gamma priors
set.seed(1)
dta <- list(y = rweibull(1000, 2.51, 14400), N = 1000)
fit <- rstan::stan(file = "weibayes.stan", data = dta)

summary(fit)
# the means are very close to the actual values, that is pretty good
# lets see if that holds for the parameters we use in practice
set.seed(1)
dta2 <- list(y = rweibull(10000, 2.51, 14400), N = 10000)
fit2 <- rstan::stan(file = "weibayes.stan", data = dta2, chains = 4, cores = 1)
summary(fit2)

# also really good results it seems. Full bayesian inference seems feasible 
# at least for noncensored data. 

set.seed(1)
dta3 <- list(y = rweibull(100000, 2.51, 14400), N = 100000)
fit3 <- rstan::stan(file = "weibayes.stan", data = dta3, chains = 4, cores = 1)
summary(fit3)

stan_diag(fit3, information = "sample")
stan_diag(fit3, information = "stepsize")
stan_diag(fit3, information = "treedepth")
stan_diag(fit3, information = "divergence")

library(bayesplot)
bayesplot::mcmc_areas(fit, pars = "scale", prob = 0.95)
bayesplot::mcmc_areas(fit2, pars = "scale", prob = 0.95)
bayesplot::mcmc_areas(fit3, pars = "scale", prob = 0.95)

color_scheme_set("viridis")
bayesplot::mcmc_combo(fit)

bayesplot::mcmc_areas_ridges(fit3, pars = "scale")
# https://discourse.mc-stan.org/t/numerical-problem-in-fitting-gamma-and-weibull-distributions/1813
