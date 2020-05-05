weibull_curve <- function(expr, from = NULL, to = NULL, n = 101, add = FALSE,
                          type = "l", xname = "x", xlab = xname, ylab = NULL, log = NULL,
                          xlim = NULL, ...) {
  sexpr <- substitute(expr)
  if (is.name(sexpr)) {
    expr <- call(as.character(sexpr), as.name(xname))
  }
  else {
    expr <- sexpr
  }
  x <- seq.int(from, to, length.out = n)
  
  ll <- list(x = x)
  names(ll) <- xname
  y <- eval(expr, envir = ll, enclos = parent.frame())
  if (length(y) != length(x)) {
    stop("'expr' did not evaluate to an object of length 'n'")
  }
  list(x = x, y = y)
}

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

# function to return system unreliability
get_system_unrel <- function(params = list()) {
  curves <- list()
  minmax <- list()
  # fill the list with an arbitrary amount of failuremode points
  for (i in seq_along(params$beta)) {
    curves[[i]] <- weibull_curve(
      p2y(
        pweibull(
          x - params[["t0"]][i], params[["beta"]][i], params[["eta"]][[i]]
        ),
        "weibull"
      ),
      n = 100001, from = 0, to = 5e+05
    )
    minmax[[i]] <- c(which(curves[[i]]$y > -10)[1], which(curves[[i]]$y > 2)[1])
  }
  xrange <- c(min(sapply(minmax, `[[`, 1)), min(sapply(minmax, `[[`, 2)))
  
  rels <- list()
  for (i in seq_along(params$beta)) {
    rels[[i]] <- 1 - exp(curves[[i]]$y)
    rels[[i]][rels[[i]] < 0] <- 0
  }
  
  sysunrel <- 1 - listproduct(rels)
  sysunrel <- sysunrel[xrange[1]:xrange[2]]
  xseq <- seq(from = 0, to = 5e+5, length.out = 100001)
  xseq <- xseq[xrange[1]:xrange[2]]
  # before we rescale we need to extract the correct domain, so rescaling
  # gets done correctly.
  list(x = xseq, y = sysunrel)
}
