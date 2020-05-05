# bayesian shenanigans
# using package icenReg
# Response variable should either be of the form cbind(1, u) or Surv(1, u, type = "interval2")
# where 1 and u are the lower and upper ends of the interval known to contain the event of
# interest. Uncensored data can be included by setting 1 == u, right censored data can be
# included by setting u == Inf or u == NA and left censored data can be included by setting 1 == 0

test <- ic_bayes(cbind(1, u), data = miceData)
flat_prior_model <- ic_bayes(cbind(l, u), data = miceData)


simData <- simCS_weib(model = "aft", shape = 2.51, scale = 14400, n = 1000)
head(cs2ic(simData$time, simData$event))

fit <- ic_bayes(cs2ic(time, event) ~ x1, data = simData)

# it yields parameters very close to the real ones with
# > exp(0.922)
# [1] 2.514314
# > exp(9.595)
# [1] 14691.14

# all that's needed is lower upper group

fal <- scan("I:\\airworthiness.p\\01_Mitarbeiter\\Benedikt Arnthof\\WeibullApp\\Failure1.csv")
sus <- scan("I:\\airworthiness.p\\01_Mitarbeiter\\Benedikt Arnthof\\WeibullApp\\Suspensions_Iststand.csv")

# fal = uncensored => l == u,
# sus = right censored => u == Inf

total <- c(fal, sus)
l <- total
u <- numeric(length = length(l))
u[1:length(fal)] <- fal
u[(length(fal)+1):length(u)] <- Inf
g <- character(length = length(l))
g[1:length(fal)] <- "fal"
g[(length(fal)+1):length(g)] <- "sus"

dta <- cbind(l, u, g)
dta <- as.data.frame.matrix(dta)
dta$l <- as.numeric(dta$l)
dta$u <- as.numeric(dta$u)

dta <- data.frame(l)
dta$event <- FALSE
dta$event[1:length(fal)] <- TRUE

ic <- cs2ic(dta$l, dta$event)
ic <- as.data.frame.matrix(ic)
ic$g <- g

ic$x <- rnorm(nrow(ic))

mle_fit <- ic_par(cbind(l, u) ~ g,
                  model = "aft",
                  dist = "weibull",
                  data = dta)

# shape parameter is reasonable, scale parameter is way off.
frame <- mleframe(fal, s = sus)
mlefit(frame)

bayes_fit <- ic_bayes(cbind(l, u) ~ g,
                      data = dta,
                      model = "aft",
                      dist = "weibull")

# this only works for grouped data => different airlines or manufacturers for example
# allows to plot different survival curves etc.
