weibayesfix <- function(x, s = NULL, beta) {
  expand_qty <- function(x, q) {
    outvec <- NULL
    for (line in 1:length(x)) {
      outvec <- c(outvec, rep(x[line], q[line]))
    }
    outvec
  }
  suspensions <- NULL
  if (is.vector(x) || length(x) == 0) {
    if (length(x) > 0) {
      if (anyNA(x)) {
        stop("NA in failure data")
      }
      if (any(x <= 0)) {
        stop("non-positive values in failure/occurrence data")
      }
      fail_vec <- sort(x)
    }
    else {
      fail_vec <- NULL
    }
    if (length(s) > 0) {
      if (anyNA(s)) {
        stop("NA  in suspension data")
      }
      if (any(s <= 0)) {
        stop("non-positive values in suspension data")
      }
      susp_vec <- sort(s)
    }
    times <- c(fail_vec, susp_vec)
    nfail <- length(fail_vec)
  }
  else {
    if (class(x) == "data.frame") {
      if (is.null(x$time) || is.null(x$event)) {
        stop(": Argument \"x\" is missing $time and/or \",\"$event columns...")
      }
      if (anyNA(x$time)) {
        stop("NA in failure or suspension data")
      }
      if (any(x$time <= 0)) {
        stop("non-positive values in failure or suspension data")
      }
      ev_info <- levels(factor(x$event))
      if (identical(ev_info, c("0", "1")) || identical(
        ev_info,
        "1"
      ) || identical(ev_info, "0")) {
      }
      else {
        stop("event column not '1' or '0' ")
      }
      if (length(s) > 0) {
        warning("argument 's' ignored when time-event dataframe provided")
      }
      if (!is.null(x$qty)) {
        if (any(!is.integer(x$qty))) {
          x$qty <- ceiling(x$qty)
        }
        times <- expand_qty(x$time, x$qty)
        events <- expand_qty(x$event, x$qty)
      }
      else {
        times <- x$time
        events <- x$event
      }
      nfail <- sum(events)
    }
  }
  if (!exists("nfail")) {
    nfail <- 1
  }
  if (nfail == 0) {
    nfail <- 1
  }
  t_eta <- (times^beta) / nfail
  out_val <- sum(t_eta)^(1 / beta)
  out_val
}
