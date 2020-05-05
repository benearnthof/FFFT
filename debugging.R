# debugging inspection once and for all
testcase <- suspensions
testcase <- get_hbands(testcase, T, inspectionmatrix = matrix(c(500, 1), nrow = 1))
insmatrix = matrix(c(500, 1), nrow = 1)

debug(inspect_engines_pod)
for (i in 1:10) {testcase <- inspect_engines_pod(testcase, 100, insmatrix)}

# try and understand why it behaves the way it does with the real data

influx <- 0
influx_age <- 0
influx_distr <- "none"
hmonth <- 150
errorfreeperiod <- 0
age_amnt <- 0
age_total <- T
nmonths <- 480
nweibull <- 100
beta <- 2.51
theta <- 14400
simple <- TRUE
# suspensions <- seq(from = 50, to = 1500, by = 50)
fast <- T
removalmethod <- T
removaltype <- "dunif"
removeyesno <- F
inspectionmatrix <- matrix(c(1000, 1), nrow = 1)

debug(simulation)
undebug(simulation)
ls <- simulation(nmonths = 150, suspensions = suspensions, removaldist = removaldist,
                 removalmethod = T, removaltype = "dunif", removeyesno = F,
                 removalamnts = removalamnts, inspectionmatrix = inspectionmatrix)

influxmethod = "Constant"

saveRDS(input, file = "debugginginput.RDS")
str(input)
