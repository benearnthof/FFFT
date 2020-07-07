# this function just serves to fill in the data frame used to visualize multiple runs
simulation_multi_bayes <- function(hmonth = 150, errorfreeperiod = 0, nmonths = 1, failures,
                                   beta, theta, simdist = "weibull", simple = TRUE, suspensions, maxlyf, ...) {
  # initial data processing
  inspectionmatrix <- matrix(c(100000, 0), nrow = 1)
  if (!is.null(failures)) {
    suspensions <- sort(c(suspensions, failures))
  }
  suspensions <- sort(suspensions)
  bnds <- get_hbands(suspensions, simple = simple, inspectionmatrix = inspectionmatrix)
  amount <- sum(bnds[, 2])
  monthly_errors <- matrix(nrow = nmonths, ncol = length(beta))
  # loop to calculate monthly errors
  for (j in 1:length(beta)) {
    tmp <- bnds
    for (i in 1:nmonths) {
      amount <- sum(tmp[, 2])
      initial_errors <- calc_errors_fast(bnds = tmp, tht = theta[j], bet = beta[j], efperiod = errorfreeperiod, dist = simdist, maxlyf = maxlyf)
      aged_fleet <- inspect_engines_pod(bnds = tmp, hmonthly = hmonth, insmatrix = inspectionmatrix)
      aged_errors <- calc_errors_fast(bnds = aged_fleet, tht = theta[j], bet = beta[j], efperiod = errorfreeperiod, dist = simdist, maxlyf = maxlyf)
      monthly_errors[i, j] <- aged_errors - initial_errors
      tmp <- inspect_engines_pod(bnds = tmp, hmonthly = hmonth, insmatrix = inspectionmatrix)
    }
  }
  # if inspection intervals cause monthly errors less than 0
  # monthly_errors[is.na(monthly_errors)] <- 0
  return(monthly_errors)
}
