# lets get done with the confidence intervals for the simulation, how hard can it be?
# if the checkbox KI? is ticked we simply run the simulation again with the "optimal" and "worst case" parameters.
# The fitdistrplus package allows us to fit not only the parameters to the data, but also obtains KIs while doing so.
#

library(fitdistrplus)
?fitdistcens
# need to hand the data over in a censdata dataframe with left and right columns
#
# hand the params over to simulation and just run calcerrorsfast with the new params in each step

# censdata = a dataframe of two columns respectively named left and right, describing each observed value as
# an interval.
# The left column contains either NA for left censored observations, the left bound of the interval for
# interval censored observations, or the observed value for non-censored observations.
# The right column contains either NA for right censored observations, the right bound of the interval
# for interval censored observations, or the observed value for non-censored observations.

# lets write a function to construct such a censdata frame out of failure and suspension data.
failures <- scan("I:\\airworthiness.p\\01_Mitarbeiter\\Benedikt Arnthof\\WeibullApp\\Failure1.csv")
suspensions <- scan("I:\\airworthiness.p\\01_Mitarbeiter\\Benedikt Arnthof\\WeibullApp\\Suspensions_Iststand.csv")

get_censdata <- function(fal, sus) {
  ret <- data.frame(left = c(fal, sus), right = c(fal, rep(NA, times = length(sus))))
}

test <- get_censdata(failures, suspensions)

res <- fitdistcens(test, distr = "weibull")

bootstrapsample1 <- test[sample(nrow(test), replace = TRUE),]
bootres <- fitdistcens(bootstrapsample1, dist = "weibull")

# one can then use the quantile method for an object of class "fitdistcens"
# to return bootstrap confidence intervals

quantile(res)
boot <- bootdistcens(res)
plot(boot)
summary(boot)

# the weibullr package calculates confidence bounds by default.
?WeibullR::LRbounds()

mframe <- mleframe(failures, s = suspensions)

LRbounds(mframe)
WeibullR::MRRw2p(failures, suspensions, bounds = TRUE, show = TRUE)

# use MRRw2p to calculate bounds, then use b50 life bounds as parameter for KI
?MRRw2p

test <- MRRw2p(failures, suspensions, bounds = TRUE)[[2]][17,]
dta <- test[[2]]
params <- dta[17, ]
params
# the structure of dta is always the same since we use the quickfit method of weibullR
# the params in the 17th row are the lower and upper bounds

# do this quick estimation and save the expected errors in corresponding vectors
# then return them and or add them to the plot as lines. simple as that
#
