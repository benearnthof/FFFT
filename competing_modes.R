# competing modes
require(weibulltools)

rinternals <- file.path(R.home("include"), "Rinternals.h")
file.show(rinternals)

require(WeibullR)

# mixmod_em requires a numeric vector consisting of lifetime data aswell as
# a vector of binary data indicating whether unit i is a right censored
# observation (0) or a failure (1)
hours <- c(
  2, 28, 67, 119, 179, 236, 282, 317, 348, 387, 3, 31, 69, 135,
  191, 241, 284, 318, 348, 392, 5, 31, 76, 144, 203, 257, 286,
  320, 350, 412, 8, 52, 78, 157, 211, 261, 298, 327, 360, 446,
  13, 53, 104, 160, 221, 264, 303, 328, 369, 21, 64, 113, 168,
  226, 278, 314, 328, 377
)

state <- c(
  1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1,
  1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0,
  1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  0, 1, 1, 1, 1, 1, 1
)

id <- 1:length(hours)

mix_mod_em <- mixmod_em(
  x = hours, event = state, distribution = "weibull",
  conf_level = 0.95, k = 2, method = "EM", n_iter = 150
)

plot_weibull_em <- plot_prob_mix(
  x = hours,
  event = state,
  id = id,
  distribution = "weibull",
  mix_output = mix_mod_em,
  title_main = "Weibull Mixture EM",
  title_x = "Time in Hours",
  title_y = "Probability of Failure",
  title_trace = "Subgroup"
)

plot_weibull_emlines <- plot_mod_mix(
  p_obj = plot_weibull_em,
  x = hours,
  event = state,
  mix_output = mix_mod_em,
  distribution = "weibull",
  title_trace = "Fitted Line"
)

john <- johnson_method(x = hours, event = state)
mix_mod_reg <- mixmod_regression(
  x = john$characteristic,
  y = john$prob,
  event = john$status,
  distribution = "weibull"
)

plot_weibull_reg <- plot_prob_mix(
  x = hours,
  event = state,
  id = id,
  distribution = "weibull",
  mix_output = mix_mod_reg,
  title_main = "Weibull Mixture Regression",
  title_x = "Time in Hours",
  title_y = "Probability of Failure",
  title_trace = "Subgroup"
)

plot_weibull_reglines <- plot_mod_mix(
  p_obj = plot_weibull_reg,
  x = hours,
  event = state,
  mix_output = mix_mod_reg,
  distribution = "weibull",
  title_trace = "Fitted Line"
)

hours <- read.csv("https://raw.githubusercontent.com/openrelia/WeibullR.gallery/master/data/acid_gas_compressor.csv", header = T)$agc
state <- rbinom(n = length(hours), size = c(0, 1), prob = 0.6)
id <- 1:length(hours)

mix_mod_em <- mixmod_em(
  x = hours, event = state, distribution = "weibull",
  conf_level = 0.95, k = 3, method = "EM", n_iter = 200
)

plot_weibull_em <- plot_prob_mix(
  x = hours,
  event = state,
  id = id,
  distribution = "weibull",
  mix_output = mix_mod_em,
  title_main = "Weibull Mixture EM",
  title_x = "Time in Hours",
  title_y = "Probability of Failure",
  title_trace = "Subgroup"
)
plot_weibull_em

plot_weibull_emlines <- plot_mod_mix(
  p_obj = plot_weibull_em,
  x = hours,
  event = state,
  mix_output = mix_mod_em,
  distribution = "weibull",
  title_trace = "Fitted Line"
)
plot_weibull_emlines

john <- johnson_method(x = hours, event = state)
mix_mod_reg <- mixmod_regression(
  x = john$characteristic,
  y = john$prob,
  event = john$status,
  distribution = "weibull"
)

plot_weibull_reg <- plot_prob_mix(
  x = hours,
  event = state,
  id = id,
  distribution = "weibull",
  mix_output = mix_mod_reg,
  title_main = "Weibull Mixture Regression",
  title_x = "Time in Hours",
  title_y = "Probability of Failure",
  title_trace = "Subgroup"
)
plot_weibull_reg

plot_weibull_reglines <- plot_mod_mix(
  p_obj = plot_weibull_reg,
  x = hours,
  event = state,
  mix_output = mix_mod_reg,
  distribution = "weibull",
  title_trace = "Fitted Line"
)
plot_weibull_reglines

# testing plotly graphs in shiny
library(shiny)
library(plotly)
options(shiny.launch.browser = .rs.invokeShinyWindowViewer)
ui <- fluidPage(
  plotlyOutput("plot")
  # verbatimTextOutput("event")
)

server <- function(input, output) {
  hours <- c(
    2, 28, 67, 119, 179, 236, 282, 317, 348, 387, 3, 31, 69, 135,
    191, 241, 284, 318, 348, 392, 5, 31, 76, 144, 203, 257, 286,
    320, 350, 412, 8, 52, 78, 157, 211, 261, 298, 327, 360, 446,
    13, 53, 104, 160, 221, 264, 303, 328, 369, 21, 64, 113, 168,
    226, 278, 314, 328, 377
  )
  
  state <- c(
    1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1,
    1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0,
    1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    0, 1, 1, 1, 1, 1, 1
  )
  
  id <- 1:length(hours)
  john <- johnson_method(x = hours, event = state)
  mix_mod_reg <- mixmod_regression(
    x = john$characteristic,
    y = john$prob,
    event = john$status,
    distribution = "weibull"
  )
  
  plot_weibull_reg <- plot_prob_mix(
    x = hours,
    event = state,
    id = id,
    distribution = "weibull",
    mix_output = mix_mod_reg,
    title_main = "Weibull Mixture Regression",
    title_x = "Time in Hours",
    title_y = "Probability of Failure",
    title_trace = "Subgroup"
  )
  
  plot_weibull_reglines <- plot_mod_mix(
    p_obj = plot_weibull_reg,
    x = hours,
    event = state,
    mix_output = mix_mod_reg,
    distribution = "weibull",
    title_trace = "Fitted Line"
  )
  # renderPlotly() also understands ggplot2 objects!
  output$plot <- renderPlotly({
    plot_weibull_reglines
  })
  
  # output$event <- renderPrint({
  #   d <- event_data("plotly_hover")
  #   if (is.null(d)) "Hover on a point!" else d
  # })
}

shinyApp(ui, server)

# convolution of two distributions
require(distr)
N <- Norm(mean = 3, sd = 3)
L <- Lnorm(meanlog = 1.5, sdlog = 0.75)
conv <- convpow(L + N, 1)
f.Z <- d(conv)

X <- rnorm(10000, 3, 3)
Y <- rlnorm(10000, 1.5, 0.75)
Z <- X + Y

hist(Z, freq = F, breaks = 50, xlim = c(0, 30))
z <- seq(0, 50, 0.01)
lines(z, f.Z(z), lty = 2, col = "purple")
lines(z, dnorm(z, mean = 3, sd = 3), lty = 2, col = "blue")
lines(z, dlnorm(z, meanlog = 1.5, sdlog = 0.75), lty = 2, col = "red")

w1 <- Weibull(shape = 2.51, scale = 14400)
w2 <- Weibull(shape = 5, scale = 15000)
conv <- convpow(w1 + w2, 1)
f.conv <- d(conv)
r.conv <- r(conv)
p.conv <- p(conv)

x <- rweibull(10000, shape = 2.51, scale = 14400)
y <- rweibull(10000, shape = 5, scale = 15000)
z <- x + y

hist(z, freq = F, breaks = 50, xlim = c(0, 50000))
s <- seq(0, 50000, 10)
lines(s, f.conv(s), lty = 2, col = "purple")
lines(s, dweibull(s, shape = 2.51, scale = 14400), lty = 2, col = "blue")
lines(s, dweibull(s, shape = 5, scale = 15000), lty = 2, col = "red")

one <- rweibull(10000, shape = 2.51, scale = 14000)
two <- rweibull(10000, shape = 5, scale = 15000)
tre <- r.conv(10000)

one.fit <- wblr.fit(wblr(one), col = "blue")
two.fit <- wblr.fit(wblr(two), col = "red")
tre.fit <- wblr.fit(wblr(tre), col = "purple")
plot.wblr(list(one.fit, two.fit, tre.fit), is.plot.pp = FALSE)

# convolution produces correct, but unfitting results

w1 <- Unif(Min = 0, Max = 6)
w2 <- Unif(Min = 0, Max = 6)
conv <- convpow(w1 + w2, 1)
f.conv <- d(conv)
x <- runif(10000, min = 0, max = 6)
y <- runif(10000, min = 0, max = 6)
z <- x + y

hist(z, freq = F, breaks = 50, xlim = c(0, 14))
s <- seq(0, 12, 0.01)
lines(s, f.conv(s), lty = 2, col = "purple")
lines(s, dunif(s, min = 0, max = 6), lty = 2, col = "blue")
lines(s, dunif(s, min = 6, max = 12), lty = 2, col = "red")

# technically we are only interested in whatever failure mode happens first
# idea: draw 10000 samples from each underlying distribution as two vectors
# x <- rweibull(10000, shape1, scale1); y <- rweibull(10000, shape2, scale2)
# compare both samplevectors by minimum
# define z <- min(x,y)
# estimate parameters of total distribution with weibullr
# lets give it a shot
x <- rweibull(10000, shape = 2.51, scale = 14400)
y <- rweibull(10000, shape = 5, scale = 16000)
z <- pmin(x, y)

x.fit <- wblr.fit(wblr(x), col = "blue")
y.fit <- wblr.fit(wblr(y), col = "red")
z.fit <- wblr.fit(wblr(z), col = "purple")

plot.wblr(list(x.fit, y.fit, z.fit), is.plot.pp = FALSE)

require(FAdist)

x <- rweibull3(10000, shape = 1.5, scale = 11240, thres = 3000)
y <- rweibull(10000, shape = 2.38, scale = 24260)
z <- pmin(x, y)

x.fit <- wblr.fit(wblr(x, dist = "weibull3p"), col = "blue", method.fit = "mle")
y.fit <- wblr.fit(wblr(y, dist = "weibull"), col = "red")
z.fit <- wblr.fit(wblr(z, dist = "weibull3p"), col = "purple", method.fit = "mle")

plot.wblr(list(x.fit, y.fit), is.plot.pp = F, is.plot.legend = F)

test <- wblr.fit(wblr(x, dist = "weibull3p"), method.fit = "mle", col = "green")
plot.wblr(test, is.plot.pp = T)
points <- test$data$dpoints

ti <- points$time
pos <- points$ppp

p2y <- function(p, canvas = "weibull") {
  if (canvas == "weibull") {
    ret <- log(qweibull(p, 1, 1))
  }
  if (canvas == "lognormal") {
    ret <- qlnorm(p, 0, 1)
  }
  ret
}
### what does curve return? Can we leverage that to finally put an end to this
### endeavour? lmfao
fit <- y.fit$fit
tz <- x.fit$fit[[1]]$t0
fit <- x.fit$fit
test <- curve(p2y(pweibull(x - tz, fit[[1]]$beta, fit[[1]]$eta), "weibull"),
              add = TRUE, n = 1001, col = "purple",
              from = 100, to = 50000
)
tz <- y.fit$fit[[1]]$t0
if (is.null(tz)) tz <- 0
fit <- y.fit$fit
test2 <- curve(p2y(pweibull(x - tz, fit[[1]]$beta, fit[[1]]$eta), "weibull"),
               add = TRUE, n = 1001, col = "purple",
               from = 100, to = 60000
)
# insert range getter function here because we have to adjust the range dynamically
lines(ti, p2y(pos), col = "blue", lty = 2)
points.default(x = 5000, y = -9.9, col = "black", pch = 3)
# y max = 2, y min = -10
# canvas = weibull

debugonce(plot.wblr)
plot.wblr(test, is.plot.pp = FALSE)

# lets take a look at how exactly points are rescaled on the plot.
# our goal is to find out the relationship of x to y. It seems y values are rescaled
# from -something to
exp(test$y[436])
exp(test$y[1001])
points(test$x[631], test$y[631])
exp(test$y[])
# the plot only renders points from -10 to +2
# -10 as y value is equal to a unreliability of near 0, +2 is equal to almost 100%
# unreliability
yplot <- test$y[test$y >= -10 & test$y <= 2]
xplot <- test$x[test$y >= -10 & test$y <= 2]
points(test$x, test$y)
unrel <- scales::rescale(yplot, to = c(0, 1))

yplot2 <- test2$y[test2$y >= -10 & test2$y <= 2]
xplot2 <- test2$x[test2$y >= -10 & test2$y <= 2]
points(xplot2, yplot2)
unrel2 <- scales::rescale(yplot2, to = c(0, 1))

rel1 <- 1 - unrel
rel2 <- 1 - unrel2

xrange <- c(min(xplot[1], xplot2[1]), min(xplot[length(xplot)], xplot2[length(xplot2)]))

# choose whatever rel vector is longer and interpolate the values of the shorter one
# using sequential x coordinates
# rel2 is longer than rel 1
# the vector of the values to be interpolated at has to have length of length(xplot2)
xinterp <- seq(from = xrange[1], to = xrange[2], length.out = length(xplot2))
yinterp <- approx(x = xplot, y = yplot, xout = xinterp)
yinterp$y[is.na(yinterp$y)] <- -10

length(unrel)
length(unrel2)
unrelinterp <- scales::rescale(yinterp$y, to = c(0, 1))
relinterp <- 1 - unrelinterp

# system reliability is equal to the product of unreliability of the two failure modes
relsys <- relinterp * rel2
unrelsys <- 1 - relsys
unrelsysplt <- scales::rescale(unrelsys, to = c(-10, 2))
lines(x = xinterp, y = unrelsysplt, col = "magenta")
lines(x = xinterp, y = unrelsysplt, col = "lightblue")
# where do the lines cross?
unrelsysplt < unrel2
unrelfinal <- unrelsysplt
unrelfinal[unrelsysplt < unrel2] <- unrel2[unrelsysplt < unrel2]
xfinal <- xinterp
xfinal[unrelsysplt < unrel2] <- xplot[unrelsysplt < unrel2]
lines(x = xfinal, y = unrelfinal, col = "red", lty = 2)
lines(x = xinterp, y = yplot2, col = "green")

# we need a fixed range for everything to properly work @_@
# lets wrap everything in a function and see fro there!

p2y <- function(p, canvas = "weibull") {
  if (canvas == "weibull") {
    ret <- log(qweibull(p, 1, 1))
  }
  if (canvas == "lognormal") {
    ret <- qlnorm(p, 0, 1)
  }
  ret
}

# recursive function to obtain product of all list entries
# that thing is beautiful as a motherfucker
listproduct <- function(input) {
  if (length(input) == 1) {
    return(input[[1]])
  }
  input[[1]] * Recall(input[-1])
}

# params to test the function
params <- list(beta = c(1.883461, 2.356974), eta = c(14047.15, 24415.83), t0 = c(1490.698, 0))

# function to return system unreliability
get_system_unrel <- function(params = list()) {
  curves <- list()
  # reliability values are given by pweibull(params) at the specified time
  for (i in seq_along(params$beta)) {
    curves[[i]] <- weibull_curve(
      pweibull(
        x - params[["t0"]][i], params[["beta"]][i], params[["eta"]][[i]]
      ),
      n = 20001, from = 0, to = 5e+05
    )
  }
  # convert unreliability values to reliability values
  rels <- list()
  for (i in seq_along(params$beta)) {
    rels[[i]] <- 1 - curves[[i]]$y
    rels[[i]][rels[[i]] < 0] <- 0
  }
  # figure out system reliability by getting the listproduct and transform back
  # to the unreliability scale
  sysunrel <- 1 - listproduct(rels)
  xseq <- seq(from = 0, to = 5e+5, length.out = 20001)
  list(x = xseq, y = sysunrel)
}

lines(x = curves[[1]]$x, curves[[1]]$y)


twopar <- rweibull(1000, shape = 5.8, scale = 6000)
twopar <- wblr.fit(wblr(twopar, dist = "weibull"), col = "blue", method.fit = "mle")
threepar <- rweibull3(1000, shape = 1, scale = 34000, thres = 400)
threepar <- wblr.fit(wblr(rweibull3(1000, shape = 1, scale = 34000, thres = 400), dist = "weibull3p"), col = "red", method.fit = "mle")

params <- list(beta = c(1.0, 5.8), eta = c(34000, 6000), t0 = c(400, 0))
sysunrel <- get_system_unrel(params)
plot.wblr(twopar, is.plot.legend = F)

lines(x = sysunrel$x, y = p2y(sysunrel$y, canvas = "weibull"), col = "black")

twopar <- rweibull(1000, shape = 2.51, scale = 14400)
twopar <- wblr.fit(wblr(twopar, dist = "weibull"), col = "darkgreen", method.fit = "rr")
params <- list(beta = c(2.51), eta = c(14400), t0 = c(0))
sysunrel <- get_system_unrel(params)
plot.wblr(twopar, is.plot.pp = F, is.plot.legend = F)

lines(x = sysunrel[1] , y = p2y(sysunrel[2], canvas = "weibull"), col = "magenta")

# todo: check the minimum! the green line should always be on top.
# we need to exponentiate to get the reliabilities
(1 - exp(curves[[1]]$y[601])) * (1 - exp(curves[[2]]$y[601]))


# we need to manually construct the canvas first, then add the curves for the chosen
# parameters one after another
# and THEN we add the line for the competing modes to the plot.
# lets construct an empty canvas and wrap everything into a function!

# debugonce(plot.wblr)
plot.wblr(twopar, is.plot.pp = F, is.plot.legend = F)
# finding max data range
plotweibullcanvas(twopar)

crv <- list()
for (i in seq_along(params$beta)) {
  curve(
    p2y(
      pweibull(
        x - params[["t0"]][i], params[["beta"]][i], params[["eta"]][[i]]
      ),
      "weibull"
    ),
    n = 10001, from = 1, to = 5e+05, add = TRUE, lwd = 2, col = "blue"
  )
}

lines(x = crv[[1]]$x, y = crv[[1]]$y)
lines(x = crv[[2]]$x, y = crv[[2]]$y)

lines(x = sysunrel$x , y = p2y(sysunrel$y, canvas = "weibull"), lwd = 2, col = "red")

min(which(sysunrel$y == 1))
x <- sysunrel$x
y <- sysunrel$y
x[3038:length(x)] <- NA
y[3038:length(y)] <- NA
plot(y~x)

lines(x, y = p2y(y, canvas = "weibull"), lwd = 2, col = "red")

rel1 <- seq(from = 1, to = 0, length.out = 100)
rel2 <- seq(from = 1, to = 0, length.out = 100)
sysr <- rel1 * rel2
x <- seq(from = 0, to = 1, length.out = 100)
plot(rel1 ~ x)
plot(sysr ~ x, add = TRUE)
