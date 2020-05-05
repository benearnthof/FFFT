plotweibullcanvas <- function(x, ...) {
  if (identical(class(x), "wblr"))
    x <- list(x)
  if (!all(sapply(x, function(x) identical(class(x), "wblr")))) {
    stop("Argument \"x\" is not of class \"wblr\" or ",
         "a list of \"wblr\" objects.")
  }
  arg <- list(...)
  if (!is.null(arg$log)) {
    warning("log option is to be depreciated in favor of canvas")
    if (arg$log == "xy" || arg$log == "yx") {
      arg$canvas <- "lognormal"
      arg$log <- "xy"
    }
    else {
      if (arg$log == "x") {
        arg$canvas <- "weibull"
      }
      else {
        stop("if used, log argument must be \"x\", \"xy\", or \"yx\" ")
      }
    }
  }
  else {
    if (!is.null(arg$canvas)) {
      if (tolower(arg$canvas) == "lognormal") {
        arg$log <- "xy"
      }
      else {
        arg$log <- "x"
        if (tolower(arg$canvas) != "weibull") {
          warning("canvas option not recognized, default \"weibull\" is assumed")
          arg$canvas <- "weibull"
        }
      }
    }
  }
  opa <- x[[1]]$options
  opa <- modifyList(opa, arg)
  dotargs <- arg
  ra <- WeibullR:::findMaxDataRange(x, opa$log)
  xlimits <- range(ra$xrange, na.rm = TRUE)
  ylimits <- range(ra$yrange, na.rm = TRUE)
  if (is.null(opa$xlim)) {
    opa$xlim <- c(10^(log10(xlimits[1]) - 0.5), 10^(log10(xlimits[2]) +
                                                      1))
  }
  if (is.null(opa$ylim)) {
    if (ylimits[1] < 0.01)
      opa$ylim <- c(signif(ylimits[1], 1), 0.99)
    else opa$ylim <- c(0.01, 0.99)
  }
  opanames <- names(opa)
  plotargs <- c(list(x = NA, axes = FALSE), opa[opanames %in%
                                                  WeibullR:::plot_default_args()])
  if (!is.null(plotargs$ylim)) {
    plotargs$ylim <- WeibullR:::p2y(plotargs$ylim, opa$log)
  }
  plotargs$main <- NULL
  if (!is.null(opa$mar))
    par(mar = opa$mar)
  if (!is.null(opa$mai))
    par(mai = opa$mai)
  do.call(plot.default, plotargs)
  if (opa$is.plot.grid) {
    abline(h = WeibullR:::p2y(WeibullR:::seq.wb(opa$ylim[1]/10, 1 - (1 - opa$ylim[2])/10),
                              opa$log), v = WeibullR:::seq.log(opa$xlim[1]/10, opa$xlim[2] *
                                                                 10, seq(0, 10, 1)), col = opa$col.grid)
  }
  r <- WeibullR:::seq.log(opa$xlim[1]/10, opa$xlim[2] * 10, c(1, 5))
  for (t in c(1, 3)) {
    axis(t, at = WeibullR:::seq.log(opa$xlim[1]/10, opa$xlim[2] * 10,
                                    seq(0, 10, 0.2)), labels = NA, tcl = -0.25)
    axis(t, at = r, labels = r, tcl = -0.75)
  }
  r <- c(WeibullR:::seq.wb(opa$ylim[1]/10, 1 - (1 - opa$ylim[2])/10,
                           c(1, 2, 5)), 0.9)
  for (t in c(2, 4)) {
    axis(t, at = WeibullR:::p2y(WeibullR:::seq.wb(opa$ylim[1]/10, 1 - (1 - opa$ylim[2])/10),
                                opa$log), labels = NA, tcl = -0.25)
    axis(t, at = WeibullR:::p2y(r, opa$log), labels = r * 100, tcl = -0.75)
  }
  abline(h = 0, lty = 3, col = opa$col.grid)
  title(main = "System Unreliability", line = 3)
  invisible()
}
