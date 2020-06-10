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
fit <- rstan::stan(file = "weibayes.stan", data = dta, chains = 4, cores = 4)

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


set.seed(1)
dta3 <- list(y = rweibull(100000, 2.51, 14400), N = 100000)
fit3 <- rstan::stan(file = "weibayes.stan", data = dta3, chains = 4, cores = 1)
summary(fit3)

# saveRDS(suspensions, file = "suspensions.RDS")
suspensions <- readRDS(file = "suspensions.RDS")
failures <- c(1017, 1876, 4112, 1228, 4541, 1379, 3511, 4082, 3118, 3585, 1550)

dta_cens <- list(yobs = failures, Nobs = length(failures),
                 ycen = suspensions, Ncen = length(suspensions))

fit_cens <- rstan::stan(file = "weicens.stan", data = dta_cens, chains = 4, cores = 4)
summary(fit_cens)
plot(fit_cens)
stan_diag(fit_cens, "sample")

bayesplot::mcmc_areas(fit_cens, pars = "scale", prob = 0.95)

extra <- extract(fit_cens)
shp <- extra$shape
sca <- extra$scale

dta <- rweibull(1000, shape = 2.51, scale = 14400)
dta <- data.frame(time = dta)
mod <- brms::brm(time ~ 1, family = weibull(link_shape = "log"), data = dta, chains = 4, cores = 4)

library(mgcv)
library(gamlss)
gam(time ~ 1, family = weibull, data = dta)

install.packages("bayestestR")
library(bayestestR)
map_estimate(shp)
map_estimate(sca)

# lets do the sampling one more time for informative and diffuse priors
fit_cens_diffuse <- rstan::stan(file = "weicens_diffuse.stan", data = dta_cens, chains = 4, cores = 4)
summary(fit_cens_diffuse)
plot(fit_cens_diffuse)
# informative priors
fit_cens_informative<- rstan::stan(file = "weicens_informative.stan", data = dta_cens, chains = 4, cores = 4)
summary(fit_cens_informative)
plot(fit_cens_informative)

# compiling all estimates to a neat table
get_estimates <- function(fit) {
  s <- summary(fit)
  mean <- s$summary[1:2, 1]
  medi <- s$summary[1:2, 6]
  map_shape <- bayestestR::map_estimate(rstan::extract(fit)$shape)
  map_scale <- bayestestR::map_estimate(rstan::extract(fit)$scale)
  maps <- c(shape = map_shape, scale = map_scale)
  res <- rbind(mean, medi, maps)
  return(res)
}

res_diffuse <- get_estimates(fit_cens_diffuse)
res_informa <- get_estimates(fit_cens_informative)

res_diffuse
res_informa
library(bayesplot)
color_scheme_set("brewer-RdBu")
bayesplot::mcmc_trace(fit_cens_informative, pars = c("scale"))

bayesplot::mcmc_areas(fit_cens_diffuse, pars = "scale", prob = 0.95)
bayesplot::mcmc_areas(fit_cens_diffuse, pars = "shape", prob = 0.95)
bayesplot::mcmc_areas(fit_cens_informative, pars = "scale", prob = 0.95)
bayesplot::mcmc_areas(fit_cens_informative, pars = "shape", prob = 0.95)

?bayesplot
bayesplot::mcmc_pairs(fit_cens_diffuse)
bayesplot::mcmc_pairs(fit_cens_informative)

bayesplot::mcmc_combo(fit_cens_diffuse)
bayesplot::mcmc_combo(fit_cens_informative, pars = c("shape", "scale"))

bayesplot::mcmc_trace(fit_cens_informative, pars = c("shape"))
bayesplot::mcmc_trace(fit_cens_informative, pars = c("scale"))

bayesplot::mcmc_scatter(fit_cens_informative, pars = c("shape", "scale"))
bayesplot::mcmc_hex(fit_cens_informative, pars = c("shape", "scale"))
bayesplot::mcmc_scatter(fit_cens_diffuse, pars = c("shape", "scale"))
bayesplot::mcmc_hex(fit_cens_informative, pars = c("shape", "scale")) +
  xlab("Shape") +
  ylab("Scale") +
  ggtitle("Posterior Density Estimate", subtitle = "Informative Priors")

# https://theoreticalecology.wordpress.com/2010/09/17/metropolis-hastings-mcmc-in-r/

# simulating how many errors there are in a certain timeframe for the fleet
# we want to run n simulations with n parameter pairs from the informative sampling
fleet <- c(failures, suspensions)

source("functions.R")
inspectionmatrix <- matrix(c(100000, 0), nrow = 1)
bnds <- get_hbands(fleet, inspectionmatrix = inspectionmatrix)
shapes <- as.vector(rstan::extract(fit_cens_informative)$shape)
scales <- as.vector(rstan::extract(fit_cens_informative)$scale)
result_informative <- simulation_multi_bayes(hmonth = 100, nmonths = 10, failures = failures, suspensions = suspensions, 
                                             beta = shapes, theta = scales, maxlyf = 1000000)

boxplot(colSums(result_informative))
plot(result_informative)

mlt <- reshape2::melt(result_informative)
library(ggplot2)
ggplot(mlt, aes(x = Var1, y = value, group = Var1)) +
  geom_boxplot() +
  theme_bw() +
  ggtitle("Expected Events per Month", subtitle = "Informative Priors") +
  ylab("Events") +
  xlab("Months") +
  scale_x_continuous(breaks = c(1:10))

csums <- colSums(result_informative)

mlt2 <- reshape2::melt(csums)
ggplot(mlt2, aes(x = value)) +
  geom_density() +
  theme_bw() +
  ggtitle("Total Expected Events after 10 Months", subtitle = "Density Estimate of Informative Priors") +
  ylab("Density") +
  xlab("Events")

# repeating for uninformative priors
shapes <- as.vector(rstan::extract(fit_cens_diffuse)$shape)
scales <- as.vector(rstan::extract(fit_cens_diffuse)$scale)
result_diffuse <- simulation_multi_bayes(hmonth = 100, nmonths = 10, failures = failures, suspensions = suspensions, 
                                             beta = shapes, theta = scales, maxlyf = 1000000)

boxplot(colSums(result_diffuse))
plot(result_informative)

mlt <- reshape2::melt(result_diffuse)
library(ggplot2)
ggplot(mlt, aes(x = Var1, y = value, group = Var1)) +
  geom_boxplot() +
  theme_bw() +
  ggtitle("Expected Events per Month", subtitle = "Diffuse Priors") +
  ylab("Events") +
  xlab("Months") +
  scale_x_continuous(breaks = c(1:10))

csums <- colSums(result_diffuse)

mlt2 <- reshape2::melt(csums)
ggplot(mlt2, aes(x = value)) +
  geom_density() +
  theme_bw() +
  ggtitle("Total Expected Events after 10 Months", subtitle = "Density Estimate of Diffuse Priors") +
  ylab("Density") +
  xlab("Events")
