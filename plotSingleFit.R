plotSingleFit <- function(fit, opadata, dotargs) {
  opafit <- opadata
  if (opafit$is.plot.fit) {
    tz <- 0
    if (!is.null(fit$t0) && !fit$modified) {
      tz <- fit$t0
    }
    if (!is.null(fit$beta) && !is.null(fit$eta)) {
      x <- NULL
      rm(x)
      cret <- curve(p2y(pweibull(x - tz, fit$beta, fit$eta), opafit$log), add = FALSE, n = 1001, col = opafit$col,
                    lwd = opafit$lwd, lty = opafit$lty, xlim = WeibullR:::getPlotRangeX(opafit$log),
                    log = opafit$log)
      cret$y[is.infinite(cret$y)] <- NA
      cret$y[cret$y == 0] <- NA
      imin <- which.min(cret$y)
      lines(rep(cret$x[imin], 2), y = c(cret$y[imin], getPlotRangeY(opafit$log)[1]),
            col = opafit$col, lwd = opafit$lwd, lty = opafit$lty)
    }
    if (!is.null(fit$meanlog) && !is.null(fit$sdlog)) {
      x <- NULL
      rm(x)
      cret <- curve(p2y(plnorm(x - tz, fit$meanlog, fit$sdlog),
                        opafit$log), add = TRUE, col = opafit$col, lwd = opafit$lwd,
                    lty = opafit$lty, xlim = getPlotRangeX(opafit$log),
                    log = opafit$log)
      cret$y[is.infinite(cret$y)] <- NA
      cret$y[cret$y == 0] <- NA
      imin <- which.min(cret$y)
      lines(rep(cret$x[imin], 2), y = c(cret$y[imin], getPlotRangeY(opafit$log)[1]),
            col = opafit$col, lwd = opafit$lwd, lty = opafit$lty)
    }
    if (!is.null(fit$rate)) {
      x <- NULL
      rm(x)
      curve(p2y(pexp(x + tz, fit$rate), opafit$log), add = TRUE,
            col = opafit$col, lwd = opafit$lwd, lty = opafit$lty,
            xlim = getPlotRangeX(opafit$log), log = opafit$log)
    }
  }
  invisible()
}
dotargs <- NULL
fit <- x.fit$fit
plt <- plotSingleFit(fit, opadata = opadata, dotargs = NULL)
