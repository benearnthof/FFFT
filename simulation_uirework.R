simulation <- function(hmonth = 150, errorfreeperiod = 0, nmonths = 1, nweibull = 100, failures,
                       beta = 2.51, theta = 14400, simdist = "weibull", simple = TRUE, suspensions, fast = T,
                       removaldist, removalamnts, removalmethod, removaltype, removeyesno = T, removefaulty = F,
                       inspectionmatrix, doinflux, influxelps, influxamounts, influxmethod, betaalpha = 1,
                       betabeta = 1, replacefaulty = F, maxlyf, ...) {
  # initial data processing
  suspensions_rba <- sort(c(suspensions, failures, failures))
  suspensions <- sort(c(suspensions, failures))
  bnds <- get_hbands(suspensions, simple, inspectionmatrix)
  bnds_rba <- get_hbands(suspensions_rba, simple, inspectionmatrix)
  amount <- sum(bnds[, 2])
  original_errors <- calc_errors_fast(bnds = bnds, tht = theta, bet = beta, efperiod = errorfreeperiod, dist = simdist, maxlyf = maxlyf)
  original_errors_rba <- calc_errors_fast(bnds_rba, tht = theta, bet = beta, efperiod = errorfreeperiod, dist = simdist, maxlyf = maxlyf)
  monthly_errors <- numeric(length = nmonths)
  monthly_rel_errors <- numeric(length = nmonths)
  monthly_errors_lower <- numeric(length = nmonths)
  monthly_errors_upper <- numeric(length = nmonths)
  tmp <- bnds
  removal_amounts <- prep_amounts(suspensions, removalamnts, nmonths, doremoval = removeyesno)
  fleetcount <- numeric(length = nmonths + 1)
  fleetcount[1] <- nrow(tmp)
  influxamountvector <- prep_influxvector(influxamounts, nmonths, doinflux)
  # lower and upper bounds for confidence interval
  params_hilow <- MRRw2p(failures, suspensions, bounds = TRUE)[[2]][17,]
  # loop to calculate monthly errors
  for (i in 1:nmonths) {
    amount <- sum(tmp[, 2])
    if (fast == T) {
      tmp <- remove_engines(tmp, rem = removaldist, density = removalmethod, type = removaltype, amnt = removal_amounts[i], insmatrix = inspectionmatrix,
                            betaalpha = betaalpha, betabeta = betabeta)
      initial_errors <- calc_errors_fast(bnds = tmp, tht = theta, bet = beta, efperiod = errorfreeperiod, dist = simdist, maxlyf = maxlyf)
      initial_errors_low <- calc_errors_fast(bnds = tmp, tht = params_hilow$Lower, bet = beta, efperiod = errorfreeperiod, dist = simdist, maxlyf = maxlyf)
      initial_errors_high <- calc_errors_fast(bnds = tmp, tht = params_hilow$Upper, bet = beta, efperiod = errorfreeperiod, dist = simdist, maxlyf = maxlyf)
      aged_fleet <- inspect_engines_pod(bnds = tmp, hmonthly = hmonth, insmatrix = inspectionmatrix)
      aged_errors <- calc_errors_fast(bnds = aged_fleet, tht = theta, bet = beta, efperiod = errorfreeperiod, dist = simdist, maxlyf = maxlyf)
      aged_errors_low <- calc_errors_fast(bnds = aged_fleet, tht = params_hilow$Lower, bet = beta, efperiod = errorfreeperiod, dist = simdist, maxlyf = maxlyf)
      aged_errors_high <- calc_errors_fast(bnds = aged_fleet, tht = params_hilow$Upper, bet = beta, efperiod = errorfreeperiod, dist = simdist, maxlyf = maxlyf)
      rel_errors <- aged_errors / amount
    } else {
      tmp <- remove_engines(tmp, rem = removaldist, density = removalmethod, type = removaltype, amnt = removal_amounts[i], insmatrix = inspectionmatrix,
                            betaalpha = betaalpha, betabeta = betabeta)
      initial_errors <- calc_errors(ndraws = nweibull, amnt = amount, bnds = tmp, tht = theta, bet = beta, efperiod = errorfreeperiod, dist = simdist, maxlyf = maxlyf)
      aged_fleet <- inspect_engines_pod(bnds = tmp, hmonthly = hmonth, insmatrix = inspectionmatrix)
      aged_errors <- calc_errors(ndraws = nweibull, amnt = amount, bnds = aged_fleet, tht = theta, bet = beta, efperiod = errorfreeperiod, dist = simdist, maxlyf = maxlyf)
      rel_errors <- aged_errors / amount
    }
    monthly_errors[i] <- aged_errors - initial_errors
    monthly_errors_lower[i] <- aged_errors_low - initial_errors_low
    monthly_errors_upper[i] <- aged_errors_high - initial_errors_high
    # monthly_errors[i] <- rel_errors
    monthly_rel_errors[i] <- rel_errors
    # age fleet, this time for real
    tmp <- inspect_engines_pod(bnds = tmp, hmonthly = hmonth, insmatrix = inspectionmatrix)
    # influx and removals
    monthly_influx_vector <- get_influx_age(influxamountvector[i], influxmethod, influxelps)
    tmp <- add_influx(tmp, monthly_influx_vector, inspectionmatrix)
    # removal moved to the start of the loop to correctly handle error estimation upon removal
    # handling the possibility of removing a sample the size of the faulty engines
    if (removefaulty == TRUE) {
      faultyweights <- get_dist_weights(tmp, bet = beta, tht = theta, dist = simdist)
      tmp <- remove_sample(tmp, amnt = round(monthly_errors[i]), faultyweights)
    }
    if (replacefaulty == TRUE) {
      replacementages <- get_influx_age(round(monthly_errors[i]), "constant", list(age = 0))
      tmp <- add_influx(tmp, replacementages, inspectionmatrix)
    }
    fleetcount[i + 1] <- nrow(tmp)
  }
  # if inspection intervals cause monthly errors less than 0
  monthly_errors[monthly_errors < 0] <- 0
  monthly_errors_lower[monthly_errors_lower < 0] <- 0
  monthly_errors_upper[monthly_errors_upper < 0] <- 0
  total_errors <- sum(monthly_errors)
  # total_errors_rel <- monthly_rel_errors * fleetcount[2:length(fleetcount)]
  # generating plot of errors over time
  # return a seperate data frame for the confidence bound plot.
  df <- data.frame(month = 1:nmonths, Cumulative = cumsum(monthly_errors), Monthly = monthly_errors)
  df_bounds <- data.frame(month = 1:nmonths, lower = cumsum(monthly_errors_lower) + original_errors, upper = cumsum(monthly_errors_upper), mean = cumsum(monthly_errors))
  errorplot <- plt_errors(df)
  errorplot_bounds <- plt_errors_bounds(df_bounds)
  fleetplot <- plt_fleet(fleetcount)
  relerrorplot <- plt_relerrors(monthly_rel_errors)
  resfleet <- tmp
  # generating the after simulation data.frame that can be downloaded in the last panel
  cumerrors <- cumsum(monthly_errors)
  efffsize <- fleetcount[-length(fleetcount)] - length(failures)
  effmonfhours <- efffsize * hmonth
  exportframe <- data.frame(
    Monate = 1:nmonths,
    `Initial Errors` = rep(original_errors, times = nmonths),
    Failures = monthly_errors,
    `Agg Failures` = cumerrors,
    `Raw Fleetsize` = fleetcount[-length(fleetcount)] - length(failures),
    `Effective Fleetsize` = efffsize,
    EFH = effmonfhours
  )
  exportframe <- exportframe %>% plyr::mutate(AccEFH = cumsum(EFH))
  return(list(original_errors, total_errors, errorplot, fleetplot, relerrorplot,
              resfleet, exportframe, original_errors_rba, errorplot_bounds))
}
