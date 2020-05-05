walk <- function(n) sample(c(1L, -1L), n, replace = TRUE)
w <- cumsum(fnc(1000L))

r_walk <- function(n) cumsum(walk(n))

simwalks <- function(start, end, lengthofwalks, nwalks = 25) {
  results <- numeric(length = lengthofwalks)
  totalresults <- matrix(ncol = lengthofwalks, nrow = nwalks)
  # initiate main loop
  for (k in 1:reps) {
    for (i in start:end) {
      # tmp saves the individual walks
      tmp <- matrix(0, nrow = length, ncol = i)
      colnames(tmp) <- 1:ncol(tmp)
      for (j in 1:ncol(tmp)) {
        tmp[, j] <- cumsum(walk(length))
      }
      maxi <- unique(colnames(tmp)[apply(tmp, 1, which.max)])
      mini <- unique(colnames(tmp)[apply(tmp, 1, which.min)])
      extrema <- unique(c(maxi, mini))
      # results stores how many runs hit an extreme value
      results[i] <- length(extrema)
    }
    totalresults[k, ] <- results
    print(k)
  }
  return(totalresults)
}

means <- colMeans(test)
plot(means)
maxi <- colMax(test)
wot <- pmax(as.data.frame.matrix(test))
wot <- colMax(as.data.frame.matrix(test))
plot(wot)

# we are now interested in keeping the length constant and only increasing the
# amount of randomwalks
length <- 50
nruns <- c(1:50)
run <- cumsum(walk(length))


# hand this function a matrix of random walks
get_extrema <- function(randomwalks) {
  max <- unique(colnames(randomwalks)[apply(randomwalks, 1, which.max)])
  min <- unique(colnames(randomwalks)[apply(randomwalks, 1, which.min)])
  ext <- unique(c(max, min))
  res <- length(ext)
  return(res)
}

for (currentrun in seq_along(nruns)) {
  randomwalks <- matrix(0, nrow = nruns[currentrun], ncol = currentrun)
  colnames(randomwalks) <- 1:ncol(randomwalks)
  randomwalks <- get_walks(randomwalks, lengthofwalks = length)
  numberextrema <- get_extrema(randomwalks)
}

################## lets try and use purrr::map

library(purrr)

length <- 10
nwalks <- 1000

walklist <- list()

get_walks <- function(length, numberwalks) {
  ret <- matrix(0, nrow = length, ncol = numberwalks)
  for (i in 1:numberwalks) {
    ret[,i] <- r_walk(length)
  }
  return(ret)
}

for (i in 1:nwalks) {
  walklist[[i]] <- get_walks(length, i)
}

assign_colnames <- function(matrix) {
  colnames(matrix) <- 1:ncol(matrix)
  return(matrix)
}

walklist <- map(walklist, assign_colnames)
get_extrema(walklist[[4]])

numberextremewalks <- unlist(map(walklist, get_extrema))
plot(numberextremewalks)

plot(1:10, ylim = c(-10, 10))
points(-1:-10)

for (i in 1:length(max)) {
  lines(randomwalks[ , as.integer(max[i])], col = "red")
}

for (i in 1:length(min)) {
  lines(randomwalks[ , as.integer(min[i])], col = "green")
}

for (i in 1:1000) {
  points(walklist[[1000]][,i], pch = 16, col = rgb(red = 0, green = 0, blue = 0, alpha = 1))
}

# numberextremewalks is a single realization.

reps <- 50
realizations <- matrix(nrow = nwalks, ncol = reps)
for (i in 1:reps) {
  walklist <- list()
  for (j in 1:nwalks) {
    walklist[[j]] <- get_walks(length, j)
  }
  walklist <- map(walklist, assign_colnames)
  realizations[, i] <- unlist(map(walklist, get_extrema))
  print(i)
}

means <- rowMeans(realizations)
maxs <- apply(realizations, 1, function(x) max(x))
mins <- apply(realizations, 1, function(x) min(x))

plot(maxs)

# there seems to be a hard limit (as expected)
# every minimum or maximum reached decreases the amount of possible subsequent
# extreme values reachable

# what happens in the continuous case?

# we're interested in the amount of "extreme" paths
# so all the paths that hit an extreme value first

# the first step contains at most 2 extreme paths
# the second step contains at most 3 extreme paths
# the third step contains at most 4 extreme paths
# and so on.
# They can only be extreme paths if they are the first ones to reach a certain
# value, so the number of paths is directly related to the probabilities of
# taken steps.

# this seems to be a geometric problem
# consider for example the probability of there being exactly two extreme values
# after the first step.
