###### do multiple weibull runs with multiple failure modes (up to 3 as a limit)
###### pack that shit into a shiny app
# step 1: collect input information
# parameters given as list

# shapes and scales must be named lists with the exact amount of parameters,
# not more not less
shapes <- list(one = 5.401, two = 3.01)
scales <- list(one = 8, two = 11770)
n = 1000

failures <- scan("I:\\airworthiness.p\\01_Mitarbeiter\\Benedikt Arnthof\\WeibullApp\\Failure1.csv")
suspensions <- scan("I:\\airworthiness.p\\01_Mitarbeiter\\Benedikt Arnthof\\WeibullApp\\Suspensions_Iststand.csv")

# assume we use the reduced bias adjustment to construct the fleet
fleet <- sort(c(failures, failures, suspensions))

# split this fleet into proportional amounts given by the multimode sampling
# function to do the trick
# returns data frame with all necessary information
get_proportions <- function(shapes, scales, n = 50000) {
  check <- (shapes > 0) & (scales > 0)
  if (sum(check) == 0) {
    warning("Invalid Shapes & Scales entered, defaulting to 1, 1.")
    shapes <- list(1)
    scales <- list(1)
  }
  # do sampling to find proportion of engines that succumb to the modes
  samples <- purrr::pmap(list(shapes, scales), rweibull, n = n)
  samples <- do.call(cbind, samples)
  # the minimum value corresponds to failing first
  mincol <- max.col(-samples)
  proportions <- as.data.frame.table(table(mincol))
  proportions$rel <- proportions$Freq/sum(proportions$Freq)
  proportions$names <- colnames(samples)
  return(proportions)
}

# proportions from only parameter inputs
proportions <- get_proportions(shapes, scales, n = 25000)
##### should probably return these proportions, as the approximate values
##### give intuition about the situation in the fleet.

# wrap fleet generator in nice function
sample_fleets <- function(fleet, proportions) {
  # split fleet into proportional parts
  fleets <- list()
  tmp <- fleet
  # fill the list with proportional parts
  for (j in seq_len(nrow(proportions))) {
    # preserve original size to make sure we sample the whole fleet
    # round down to avoid larger sample size than original fleet
    samplesize <- floor(proportions$rel[j] * length(fleet))
    # reconversion for sample_frac
    fleets[[j]] <- sample_n(as.data.frame(tmp), size = samplesize)[,1]
    sample <- fleets[[j]]
    unq <- unique(sample)
    # handle sampling like god intended
    while (length(unq) > 0) {
      tmp <- tmp[-match(unq, tmp)]
      sample <- sample[-match(unq, sample)]
      unq <- unique(sample)
    }
  }
  return(fleets)
}

# the next bits are going to be run multiple times to get a nice multiline plot
nruns <- 50
inspectionmatrix <- matrix(c(Inf, 1), nrow = 1)
nmonths <- 120
hmonth <- 200

# wrap this in function too, will be a lot easier to maintain

mrmr_sim <- function(nruns, fleet, proportions, inspectionmatrix, shapes, scales,
                     efperiod = 0, maxlyf = Inf, hmonth, removaldist, removalmethod,
                     removaltype, removal_amounts, betaalpha, betabeta,
                     influxamountsvector, influxmethod, influxepls, simdist,
                     replacefaulty = FALSE, removefaulty = FALSE, ...) {
  results <- list()
  for (i in seq_len(nruns)) {
    fleets <- sample_fleets(fleet, proportions)
    fleets <- purrr::map(fleets, sort)
    bnds <- purrr::map(fleets, get_hbands, inspectionmatrix = inspectionmatrix)
    monthly_errors <- list()
    for (j in seq_len(nmonths)) {
      bnds <- purrr::pmap(list(bnds = bnds, amnt = get_rem_amnts_lst(bnds, removal_amounts[j])),
                          remove_engines, rem = removaldist, density = removalmethod,
                          type = removaltype, insmatrix = inspectionmatrix,
                          betaalpha = betaalpha, betabeta = betabeta)
      initial_errors <- purrr::pmap(list(bnds, as.list(shapes), as.list(scales)),
                                    calc_errors_fast, efperiod = efperiod, maxlyf = maxlyf)
      initial_errors$total <- do.call(sum, initial_errors)
      aged_fleet <- purrr::pmap(list(bnds), inspect_engines_pod, hmonthly = hmonth,
                                insmatrix = inspectionmatrix)
      aged_errors <- purrr::pmap(list(aged_fleet, as.list(shapes), as.list(scales)),
                                 calc_errors_fast, efperiod = efperiod, maxlyf = maxlyf)
      aged_errors$total <- do.call(sum, aged_errors)
      monthly_errors[[j]] <- map2(aged_errors, initial_errors, `-`)
      bnds <- purrr::pmap(list(bnds), inspect_engines_pod, hmonthly = hmonth,
                          insmatrix = inspectionmatrix)
      proportional_influx <- round(influxamountsvector[j] * proportions$rel)
      monthly_influx <- purrr::pmap(list(as.list(proportional_influx)), get_influx_age,
                                    meth = influxmethod, elps = influxepls)
      # finally add the influx to the list of fleets
      bnds <- purrr::pmap(list(bnds = bnds, agevector = monthly_influx), add_influx,
                          insmatrix = inspectionmatrix)
      
      if (removefaulty == TRUE) {
        faultyweights <- purrr::pmap(list(bnds = bnds, bet = shapes, tht = scales),
                                     get_dist_weights, dist = simdist)
        partial <- purrr::map(monthly_errors[[j]][1:(length(monthly_errors[[j]]) - 1)], round)
        bnds <- purrr::pmap(list(bnds = bnds, amnt = partial, wgts = faultyweights),
                            remove_sample)
      }
      if (replacefaulty == TRUE) {
        partial <- purrr::map(monthly_errors[[j]][1:(length(monthly_errors[[j]]) - 1)], round)
        replacementages <- purrr::pmap(list(partial), get_influx_age, meth = "constant", elps = list(age = 0))
        bnds <- purrr::pmap(list(bnds = bnds, agevector = replacementages), add_influx,
                            insmatrix = inspectionmatrix)
      }
    }
    # get rid of one layer of nesting
    monthly_errors <- lapply(monthly_errors, unlist)
    results_i <- do.call(rbind, monthly_errors)
    colnames(results_i) <- c(1:nrow(proportions), "total")
    # results now consists of a list with the individual results of every run.
    results[[i]] <- results_i
  }
  return(results)
}

mlt <- reshape2::melt(results)

# also wrap in plotting function to directly plot from the returned results list
ggplot(data = mlt, aes(x = Var1, y = value, color = Var2, group = Var2)) +
  geom_line() +
  #stat_smooth() +
  ggtitle("Number of Events by Failuremode") +
  ylab("Events") +
  xlab("Months") +
  labs(color = "Modus") +
  theme_bw()


# as of now there is very little variation in the results. May as well average
# and then just report the mean values of all the runs seperately for clarity.


fleets
test <- c(fleets[[1]], fleets[[2]], fleets[[3]])
# differs by 2 because we round down should be an acceptable margin.
length(test)

# verify that that shit is correct
purrr::pmap(list(bnds, as.list(shapes), as.list(scales)), calc_errors_fast, efperiod = 0, maxlyf = Inf)
calc_errors_fast(bnds[[3]], shapes$three, scales$three, efperiod = 0, maxlyf = Inf)
# it seems to be correct indeed.

# wrapping everything into a single simulation function like in simulation.R
# should return plots and data outputs in a list
# shapes and scales inputs must be given as a list
mm_simulation <- function(hmonth, errorfreeperiod, nmonths, failures, shapes, scales,
                          simdist = "weibull", simple = TRUE, suspensions, removaldist,
                          removalamnts, removalmethod, removaltype, removeyesno = TRUE,
                          removefaulty = FALSE, inspectionmatrix, doinflux, influxelps,
                          influxamounts, influxmethod, betaalpha, betabeta, replacefaulty,
                          maxlyf, nruns, ...) {
  # preprocess inputdata
  fleet <- sort(c(suspensions, failures, failures))
  # bnds <- get_hbands(fleet, simple, inspectionmatrix)
  # original_errors <- calc_errors_fast(bnds = bnds, tht = theta, bet = beta,
  #                                    efperiod = errorfreeperiod, dist = simdist, maxlyf = maxlyf)
  removal_amounts <- prep_amounts(fleet, removalamnts, nmonths, doremoval = removeyesno)
  influxamountsvector <- prep_influxvector(influxamounts, nmonths, doinflux)
  # preprocessing data necessary for multiruns
  proportions <- get_proportions(shapes, scales, n = 25000)
  # calculate results
  results <- mrmr_sim(nruns = nruns, fleet = fleet, proportions = proportions,
                      inspectionmatrix = inspectionmatrix, shapes = shapes, scales = scales,
                      efperiod = errorfreeperiod, maylyf = maxlyf, hmonth = hmonth,
                      removaldist = removaldist, removalmethod = removalmethod,
                      removal_amounts = removal_amounts, betaalpha = betaalpha,
                      betabeta = betabeta, influxamountsvector = influxamountsvector,
                      influxmethod = influxmethod, influxelps = influxelps, simdist = simdist,
                      replacefaulty = replacefaulty, removefaulty = removefaulty
  )
  mlt <- reshape2::melt(results)
  # also wrap in plotting function to directly plot from the returned results list
  plt <- ggplot(data = mlt, aes(x = Var1, y = value, color = Var2, group = Var2)) +
    geom_line() +
    #stat_smooth() +
    ggtitle("Number of Events by Failuremode") +
    ylab("Events") +
    xlab("Months") +
    labs(color = "Modus") +
    theme_bw()
  return(list(plt = plt, mlt = mlt))
}
# test the function thoroughly
# finish rewriting and adding to documentation
# todo: design ui and server for this with correct prefixing
# todo: rewrite multiruns to use purrr instead of fucking loops
# fix bugs from yesterday
# get on the clustering problem with t-SNE and diffusionmapping fckyea
#

# function to evenly split the removal amounts based on the length of the list
get_rem_amnts_lst <- function(bnds, amnt) {
  fcnt <- sum(unlist(purrr::map(bnds, nrow)))
  prps <- purrr::map(purrr::map(bnds, nrow), `/`, fcnt)
  temp <- purrr::map(purrr::map(prps, `*`, amnt), round)
  return(temp)
}

# this whole ordeal could be done a lot faster if we didnt have to explicitly
# forecast the number of expected events.
# the resulting plot is simply the proportionally scaled density functions of the
# shape and scale parameters and the resulting sum of these functions.
# we could simply find the proportions depending on the parameters and multiply
# the value of the scaled density functions by the fleet size.
# This approach no longer works under non constant fleet sizes so perhaps it is
# best to leave it as it is.
