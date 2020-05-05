# multi mode engine specific simulation
# process:
# Take input parameters of the specified failure distribution and find what
# percentage of the fleet will succumb to the different modes
# Assign every engine a prior lifetime as drawn from the failure mode distriution
# that was assigned to the engine
# Simulate the fleet for monthly discrete timesteps
# find the proportion of engines failed in all subgroups at every timestep.
#

# question 1: is the assignment of failure times prior to running the simulation
# not equivalent to not assigning them at all and just counting the relative error
# amounts the individual engines carry in every month?

# https://coolbutuseless.github.io/2018/05/12/challenge-simultaneous-parallel-min-and-which.min/
params <- list(beta = c(2.5, 3, 1), eta = c(14400, 10000, 16000), t0 = c(0, 0, 0), n = 100000)
one <- rweibull3(params$n, shape = params$beta[1], scale = params$eta[1], thres = params$t0[1])
two <- rweibull3(params$n, shape = params$beta[2], scale = params$eta[2], thres = params$t0[2])
tre <- rweibull3(params$n, shape = params$beta[3], scale = params$eta[3], thres = params$t0[3])

mat <- cbind(one, two, tre)
min <- max.col(-mat)
tbl <- as.data.frame(table(min))

tbl$rel <- tbl$Freq/params$n
# tbl$rel are approximate probabilities for the occurence of every failure mode.
# Assumptions: The failure modes are independent from one another;
# Every engine can only fail once;
# An engine fails as soon as a single one of the failure modes occurs.
# Under these assumptions it is given, that the ratio of failed engines for all
# modes is multinomially distributed with parameters tbl$rel

# a more intuitive way to understand:

params <- list(beta = c(1, 1, 1, 1), eta = c(10000, 10000, 10000, 10000), t0 = c(0, 0, 0, 0), n = 100000)
one <- rweibull3(params$n, shape = params$beta[1], scale = params$eta[1], thres = params$t0[1])
two <- rweibull3(params$n, shape = params$beta[2], scale = params$eta[2], thres = params$t0[2])
tre <- rweibull3(params$n, shape = params$beta[3], scale = params$eta[3], thres = params$t0[3])
fou <- rweibull3(params$n, shape = params$beta[4], scale = params$eta[4], thres = params$t0[4])

mat <- cbind(one, two, tre, fou)
min <- max.col(-mat)
tbl <- as.data.frame(table(min))
tbl$rel <- tbl$Freq/params$n
tbl

# if all parameters are equal, the minima (the index of the failure mode that caused the failure)
# are distributed uniformly. The proportion of engines that fail from each mode is approximately
# given by tbl$rel

# wrapping everything in functions
# function that expects a list of the following structure as input
params <- list(beta = c(2.5, 3), eta = c(14400, 10000), t0 = c(0, 0))
get_failure_proportions <- function(params, n) {
  mat <- matrix(nrow = n, ncol = length(params$beta))
  for (i in seq_along(params$beta)) {
    mat[,i] <- FAdist::rweibull3(n, params$beta[i], params$eta[i], params$t0[i])
  }
  min <- max.col(-mat)
  tbl <- as.data.frame(table(min))
  tbl$rel <- tbl$Freq/n
  tbl
}

# next step: simulate a fleet of ~ 10000 engines and assign every engine a failure
# in proportion to the Freqs estimated above.
# Then:
# Run the simulation once getting exact values from the standard errorcalc process
# Run the simulation once again getting values from the proportion of failed/nonfailed
# engines when assigning every engine a prior maximum life.
# Then: Compare results and see which method is preferred.
# Should be fully vectorizable so performance should be no issue

