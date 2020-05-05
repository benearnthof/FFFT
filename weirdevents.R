# calculating expected events from weird lifetime simulation
library(readr)
weirdevents <- read_csv("I:/airworthiness.p/01_Mitarbeiter/Benedikt Arnthof/Next Steps/PoF/PoF_Test/PW1500_EIS_IBR4_B_LE_N9807_smallest_ath.out", skip = 2)
head(weirdevents)
# Per Engine Over Life; Per Engine Per Cycle;
names(weirdevents) <- c("PEOL", "cycles", "PEPC", "infleet")
plot(infleet ~ cycles, data = weirdevents)
weirdevents <- mutate(weirdevents, nofleet = infleet / 5060)
plot(nofleet ~ cycles, data = weirdevents)

library(pspline)
library(splines)
splinefit <- sm.spline(weirdevents$cycles, weirdevents$PEPC)
test <- predict(splinefit, weirdevents$cycles, 1)
spline <- interpSpline(weirdevents$cycles, weirdevents$nofleet)
test2 <- predict(spline, weirdevents$cycles)
plot(test ~ weirdevents$cycles, xlab = "Cycles", ylab = "PoF")
plot(test2)

# procedure:
## relative failure per engine = predict(splinefit, age, 0)
## relative failure per month = sum(relfalperengine)
## age fleet by hmonth
## proceed to loop over all months with 104 hours per month

nmonths <- 240
hmonth <- 246
fleet <- rep(0, times = 6700)
names(weirdevents) <- c("pe", "cycles", "PEPC", "infleet")
splinefit <- sm.spline(weirdevents$cycles, weirdevents$pe)
test <- predict(splinefit, weirdevents$cycles, 0)
plot(test)
predict(splinefit, c(25000)) * 5060
monthly_errors <- numeric(length = nmonths)
for (i in 1:nmonths) {
  monthly_errors[i] <- sum(predict(splinefit, fleet + i * hmonth))
}
res <- sum(monthly_errors)
res * 5060

# lets wrap that shit in a function

dta <- read_csv("I:/airworthiness.p/01_Mitarbeiter/Benedikt Arnthof/Next Steps/PoF/PoF_Test/PW1500_EIS_IBR4_B_LE_N9807_smallest_ath.out", skip = 2)
dta <- read_csv("I:/airworthiness.p/01_Mitarbeiter/Benedikt Arnthof/Next Steps/PoF/Actual Test PoF/PW1524_EC_IBR2_A_MCPS_N26820_nominal_insp_2200.out", skip = 2)
names(dta) <- c("pe", "cycles", "pepc", "infleet")
get_events <- function(nmonths, hmonth, fleetsize, data = dta) {
  fleet <- rep(0, times = fleetsize)
  splinefit <- sm.spline(data$cycles, data$pepc)
  monthly_errors <- numeric(length = nmonths)
  for (i in 1:nmonths) {
    preds <- predict(splinefit, fleet + i * hmonth, 1)
    preds[preds < 0] <- 0
    monthly_errors[i] <- sum(preds)
  }
  res <- sum(monthly_errors)
  res
}

get_events(240, 246, 6700, dta)

`?` <- function(x, y) {
  xs <- as.list(substitute(x))
  if (xs[[1]] == as.name("<-")) x <- eval(xs[[3]])
  r <- eval(sapply(strsplit(deparse(substitute(y)), ":"), function(e) parse(text = e))[[2 - as.logical(x)]])
  if (xs[[1]] == as.name("<-")) {
    xs[[3]] <- r
    eval.parent(as.call(xs))
  } else {
    r
  }
}

x <- FALSE
y <- x ? 1:2
y
x <- TRUE
z <- x ? 1:2
z

# okay here is the plan how to handle these data sets:
# initialize the fleet with the current ages
# get baseline risk in first month
# in every discrete time step (month) do the following:
# age the current fleet
# add the influx to the fleet
# calculate increase in risk through influx and aging
# save monthly differences and absolute risk to a data.frame

# run this for the specified amount of time and with the specified cycles in every
# month; and influx of course.
# That should yield results close to the values in the original power point
# I:\airworthiness.p\01_Mitarbeiter\Benedikt Arnthof\Next Steps\PoF\Actual Test PoF
# for example.

# variables needed:
fleet <- scan("I:/airworthiness.p/01_Mitarbeiter/Benedikt Arnthof/Next Steps/PoF/Actual Test PoF/Suspensions_02_Dec_2019_PW15_19.csv")

influx <- scan("I:/airworthiness.p/01_Mitarbeiter/Benedikt Arnthof/Next Steps/PoF/Actual Test PoF/Amount_Influx.csv")

dta <- read_csv("I:/airworthiness.p/01_Mitarbeiter/Benedikt Arnthof/Next Steps/PoF/Actual Test PoF/PW1524_EIS_IBR2_A_MCPS_N33116_nominal.out", skip = 2)
# dta <- read_csv("I:/airworthiness.p/01_Mitarbeiter/Benedikt Arnthof/Next Steps/PoF/Actual Test PoF/PW1524_EC_IBR2_A_MCPS_N26820_nominal_insp_2200.out", skip = 2)
names(dta) <- c("pe", "cycles", "pepc", "infleet")

# cycles per month for pw1500g
cycles1500 <- 127
cycles1900 <- 114

# stop counting the risk at 25000 cycles.
# simulation needs to be split in accordance to the engine types.
# risk seems to stem from the same curve though
# we may also interpolate the per engine risk to the hour values of the engines in the simulation
# this way we can avoid rounding and may get better results.

# terminate loop as soon as all engines reached an age of 25000 hours.
#

maxage <- 25000
minmonths <- ceiling(minmonths <- maxage / min(cycles1500, cycles1900))
# use dta$cycles to get risk factor per engine
x <- dta$pe
y <- dta$cycles
yright <- dta$cycles[length(dta$cycles)]
approx(x, y, xout = fleet)
fleetrisk <- numeric(length = minmonths)
cycles <- 127
for (i in seq_len(minmonths)) {
  # filter out engines that are older than max age of interest
  fleet <- fleet[fleet < maxage]
  # correct influx vector
  if (length(influx) < minmonths) {
    influx <- c(influx, rep(0, times = minmonths - length(influx)))
  }
  # calc fleet risk
  fleetrisk[i] <- sum(approx(x, y, xout = fleet)$y)
  # add influx to fleet
  fleet <- c(rep(0, times = influx[i]), fleet)
  # age the fleet by the desired hours
  fleet <- fleet + cycles
}

# separate entries of both fleets and age them accordingly ====
# variables needed:
pw1500 <- scan("I:/airworthiness.p/01_Mitarbeiter/Benedikt Arnthof/Next Steps/PoF/Actual Test PoF/Suspensions_02_Dec_2019_PW1500G.csv")
pw1900 <- scan("I:/airworthiness.p/01_Mitarbeiter/Benedikt Arnthof/Next Steps/PoF/Actual Test PoF/Suspensions_02_Dec_2019_PW1900G.csv")

influx_1500 <- scan("I:/airworthiness.p/01_Mitarbeiter/Benedikt Arnthof/Next Steps/PoF/Actual Test PoF/Amount_Influx_PW1500G.csv")
influx_1900 <- scan("I:/airworthiness.p/01_Mitarbeiter/Benedikt Arnthof/Next Steps/PoF/Actual Test PoF/Amount_Influx_PW1900G.csv")

dta <- read_csv("I:/airworthiness.p/01_Mitarbeiter/Benedikt Arnthof/Next Steps/PoF/PoF weitere Testings/PW1524_EIS_IBR4_B_LE_N9823_nominal.out", skip = 2)
# dta <- read_csv("I:/airworthiness.p/01_Mitarbeiter/Benedikt Arnthof/Next Steps/PoF/Actual Test PoF/PW1524_EC_IBR2_A_MCPS_N26820_nominal_insp_2200.out", skip = 2)
names(dta) <- c("pe", "cycles", "pepc", "infleet")

# cycles per month for pw1500g
cycles_1500 <- 127
cycles_1900 <- 114

# stop counting the risk at 25000 cycles.
# simulation needs to be split in accordance to the engine types.
# risk seems to stem from the same curve though
# we may also interpolate the per engine risk to the hour values of the engines in the simulation
# this way we can avoid rounding and may get better results.

# terminate loop as soon as all engines reached an age of 25000 hours.
#

maxage <- 25000
minmonths_1500 <- ceiling(maxage / cycles1500)
minmonths_1900 <- ceiling(maxage / cycles1900)

# use dta$cycles to get risk factor per engine

x <- dta$pe
y <- dta$cycles
yright <- dta$cycles[length(dta$cycles)]
approx(x, y, xout = fleet)
fleetrisk_1500 <- numeric(length = minmonths_1500)
fleetrisk_1900 <- numeric(length = minmonths_1900)

# cycles <- 127
fleet_1500 <- pw1500
fleet_1900 <- pw1900
for (i in seq_len(minmonths_1500)) {
  # filter out engines that are older than max age of interest
  fleet_1500 <- fleet_1500[fleet_1500 < maxage]
  # correct influx vector
  if (length(influx_1500) < minmonths_1500) {
    influx_1500 <- c(influx_1500, rep(0, times = minmonths_1500 - length(influx_1500)))
  }
  # calc fleet risk
  fleetrisk_1500[i] <- sum(approx(x, y, xout = fleet_1500)$y)
  # add influx to fleet
  fleet_1500 <- c(rep(0, times = influx_1500[i]), fleet_1500)
  # age the fleet by the desired hours
  fleet_1500 <- fleet_1500 + cycles_1500
}

fleetrisk_1500

for (i in seq_len(minmonths_1900)) {
  # filter out engines that are older than max age of interest
  fleet_1900 <- fleet_1900[fleet_1900 < maxage]
  # correct influx vector
  if (length(influx_1900) < minmonths_1900) {
    influx_1900 <- c(influx_1900, rep(0, times = minmonths_1900 - length(influx_1900)))
  }
  # calc fleet risk
  fleetrisk_1900[i] <- sum(approx(x, y, xout = fleet_1900)$y)
  # add influx to fleet
  fleet_1900 <- c(rep(0, times = influx_1900[i]), fleet_1900)
  # age the fleet by the desired hours
  fleet_1900 <- fleet_1900 + cycles_1900
}

fleetrisk_1900

total <- fleetrisk_1500 + fleetrisk_1900

length(fleetrisk_1500)
length(fleetrisk_1900)

total <- numeric(length = max(length(fleetrisk_1500), length(fleetrisk_1900)))
total[1:length(fleetrisk_1500)] <- fleetrisk_1500 + fleetrisk_1900[1:length(fleetrisk_1500)]
total[(1 + length(fleetrisk_1500)):length(total)] <- tail(fleetrisk_1500, n = 1) + fleetrisk_1900[(1 + length(fleetrisk_1500)):length(total)]

# assume all data is preprocessed to fit into the lists (names of dta etc)
fleetdata <- list(fleet_1500, fleet_1900)
influxdata <- list(influx_1500, influx_1900)
maxage <- list(25000, 25000)
cycles <- list(cycles_1500, cycles_1900)
riskdata <- list(dta, dta)

library(purrr)
# function that takes in vector values and will be mapped to according lists
get_risk <- function(fleet, minmonth, maxage, influx, x, y, fleetrisk, cycles) {
  for (i in seq_len(minmonth)) {
    # perform agecheck
    fleet <- agecheck(fleet, maxage)
    # correct influx vector
    if (length(influx) < minmonth) {
      influx <- c(influx, rep(0, times = minmonth - length(influx)))
    }
    # calc fleet risk
    fleetrisk[i] <- sum(approx(x, y, xout = fleet)$y)
    # add influx to fleet
    fleet <- c(rep(0, times = influx[i]), fleet)
    # age the fleet by the desired cycles
    fleet <- fleet + cycles
  }
  fleetrisk
}

# helper function for quick age check
agecheck <- function(fleet, maxage) {
  fleet[fleet < maxage]
}

########## outline for compact function that does it all in one go ====
get_risks <- function(fleetdata, influxdata, maxage, cycles, riskdata) {
  # fleetdata: list containing all the fleet cycle values
  # influxdata: list containing all the influxdata
  # maxage: list or vector containing the specified maximum ages for all fleets
  # cycles: list or vector containing the cycles per discrete time step for all fleets
  # riskdata: list containing the risk data for each fleet
  # step 1: get minmonths
  minmonths <- map2(maxage, cycles, `/`)
  minmonths <- map(minmonths, ceiling)
  # step 2: initialize fleetrisk vectors
  fleetrisks <- map(minmonths, numeric)
  # step 3: initialize x and y for interpolation later on
  # naming is inconsistent with the actual data represented in the data frame => TODO
  x <- map(riskdata, `$`, pe)
  y <- map(riskdata, `$`, cycles)
  
  # step 4: apply all parts of the loop over fleet entries
  fleetrisks <- pmap(list(fleetdata, minmonths, maxage, influxdata, x, y, fleetrisks, cycles), get_risk)
  return(fleetrisks)
}

get_risks <- function(fleetdata, influxdata, maxage, cycles, riskdata) {
  minmonths <- map2(maxage, cycles, `/`)
  minmonths <- map(minmonths, ceiling)
  fleetrisks <- map(minmonths, numeric)
  x <- map(riskdata, `$`, pe)
  y <- map(riskdata, `$`, cycles)
  fleetrisks <- pmap(list(fleetdata, minmonths, maxage, influxdata, x, y, fleetrisks, cycles), get_risk)
  return(fleetrisks)
}

risk <- get_risks(fleetdata, influxdata, maxage, cycles, riskdata)
df <- plyr::ldply(risk, rbind)
df <- t(df)

for (i in 1:ncol(df)) {
  if (sum(is.na(df[,i])) > 0) {
    df[(1 + max(which(!is.na(df[,i])))):nrow(df), i] <- df[max(which(!is.na(df[,i]))), i]
  }
}
# final step: add everything together in a sensible manner

as.vector(rowSums(df))

install.packages("NCmisc")
library(NCmisc)
list.functions.in.file("weirdevents.R")

# gimme a risks data frame and ill plot that motherfucker like one of your french girls

# step one
# add all of those juicers together in one full data frame whose columns can be
# added together
# loop the loop
# fill that shit with the last value that was returned in the timeframe
for (i in 1:ncol(df)) {
  if (sum(is.na(df[,i])) > 0) {
    df[(1 + max(which(!is.na(df[,i])))):nrow(df), i] <- df[max(which(!is.na(df[,i]))), i]
  }
}

one <- runif(100)
two <- runif(100)
one <- sort(one)
two <- sort(two)

tre <- rowSums(cbind(one))

df <- matrix(nrow = 100, ncol = 2)
df <- as.data.frame.matrix(df)
df$V1 <- one; df$V2 <- two; df$V3 <- tre

# find method to visualize data frames of this or similar structure

df$time <- seq_len(nrow(df))

mlt <- reshape2::melt(df, id.vars = "time")

ggplot(mlt, aes(x = time, y = value, colour = variable)) +
  geom_line() +
  ggtitle("Expected Events") +
  xlab("Discrete Time Steps") +
  ylab("Events") +
  if (length(unique(mlt$variable)) == 3) {
    scale_color_hue(labels = c("Fleet 1", "Fleet 2", "Sum"), name = "")
  } else {
    scale_color_hue(name = "", labels = c("Fleet 1"))
  }

plt_weirdevents <- function(mlt) {
  if (length(unique(mlt$variable)) == 3) {
    plt <- ggplot(mlt, aes(x = time, y = value, colour = variable)) +
      geom_line() +
      ggtitle("Expected Events") +
      xlab("Discrete Time Steps") +
      ylab("Events") +
      scale_color_hue(labels = c("Fleet 1", "Fleet 2", "Sum"), name = "")
  } else {
    tmp <- mlt[mlt$variable == "V1",]
    plt <- ggplot(tmp, aes(x = time, y = value, colour = variable)) +
      geom_line() +
      ggtitle("Expected Events") +
      xlab("Discrete Time Steps") +
      ylab("Events") +
      scale_color_hue(labels = c("Fleet 1"), name = "")
  }
  plt
}
