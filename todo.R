# TODO:
# add unit tests
# remove "single function dependencies"
# refactor code for app => multiruns is fully purrrified, consider adding it
# add multifailuremode total plots
# add shopvisits to errorcalc in weirdevents
# fix bug when filtering single column input xlsx files
# hmonth as interval input => upper middle lower
# rewrite documentation

#https://stats.stackexchange.com/questions/220/how-is-the-minimum-of-a-set-of-random-variables-distributed

# oh dear

# inspections monatlich einlesen
# removal influx and inspections with date

# all that is left to do now:
# finalize confidence bound calculation
# maybe add different levels of variational inputs
# there may be a more elegant way of approaching this problem however.
# take the monthly flight hours of every engine as an example.
# Any variation we introduce to the aging of those engines is just a variation
# in inputs for the pweibull function we use to calculate error contributions
# in every discrete timestep. This function only depends on the shape and scale
# parameters so it should be possible to take a "shortcut" and approximate the
# amount of variation introduced directly by a random walk
#
# All of the introduced variation is independent from eachother
# so one may calculate the total variation directly and plot variation intervals
# directly instead of having to simulate everything an arbitrary number of runs
# this saves resources and increases performance. But may require a little extra
# brain power.
# We shall see if i can hold this promise on monday! Until then
