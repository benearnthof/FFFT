# this function just serves to fill in the data frame used to visualize multiple runs
simulation_multi <- function(hmonth = 150, errorfreeperiod = 0, nmonths = 1, failures,
                             beta = 2.51, theta = 14400, simdist = "weibull", simple = TRUE, suspensions,
                             removaldist, removalamnts, removalmethod, removaltype, removeyesno = T, removefaulty = F,
                             inspectionmatrix, doinflux, influxelps, influxamounts, influxmethod, betaalpha = 1,
                             betabeta = 1, replacefaulty = F, maxlyf, ...) {
  # initial data processing
  if (!is.null(failures)) {
    suspensions <- sort(c(suspensions, failures))
  }
  suspensions <- sort(suspensions)
  bnds <- get_hbands(suspensions, simple = simple, inspectionmatrix)
  amount <- sum(bnds[, 2])
  monthly_errors <- numeric(length = nmonths)
  tmp <- bnds
  removal_amounts <- prep_amounts(suspensions, removalamnts, nmonths, doremoval = removeyesno)
  influxamountvector <- prep_influxvector(influxamounts, nmonths, doinflux)
  # loop to calculate monthly errors
  for (i in 1:nmonths) {
    amount <- sum(tmp[, 2])
    tmp <- remove_engines(tmp,
                          rem = removaldist, density = removalmethod, type = removaltype, amnt = removal_amounts[i], insmatrix = inspectionmatrix,
                          betaalpha = betaalpha, betabeta = betabeta
    )
    initial_errors <- calc_errors_fast(bnds = tmp, tht = theta, bet = beta, efperiod = errorfreeperiod, dist = simdist, maxlyf = maxlyf)
    aged_fleet <- inspect_engines_pod(bnds = tmp, hmonthly = hmonth, insmatrix = inspectionmatrix)
    aged_errors <- calc_errors_fast(bnds = aged_fleet, tht = theta, bet = beta, efperiod = errorfreeperiod, dist = simdist, maxlyf = maxlyf)
    monthly_errors[i] <- aged_errors - initial_errors
    # monthly_errors[i] <- rel_errors
    tmp <- inspect_engines_pod(bnds = tmp, hmonthly = hmonth, insmatrix = inspectionmatrix)
    # influx and removals
    monthly_influx_vector <- get_influx_age(influxamountvector[i], influxmethod, influxelps)
    tmp <- add_influx(tmp, monthly_influx_vector, inspectionmatrix)
    if (removefaulty == TRUE) {
      faultyweights <- get_dist_weights(tmp, bet = beta, tht = theta, dist = simdist)
      tmp <- remove_sample(tmp, amnt = round(monthly_errors[i]), faultyweights)
    }
    if (replacefaulty == TRUE) {
      replacementages <- get_influx_age(round(monthly_errors[i]), "constant", list(age = 0))
      tmp <- add_influx(tmp, replacementages, inspectionmatrix)
    }
  }
  # if inspection intervals cause monthly errors less than 0
  monthly_errors[monthly_errors < 0] <- 0
  return(monthly_errors)
}

