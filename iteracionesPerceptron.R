DistanceFromPlane = function(z, w, b) {
  sum(z * w) + b
}

ClassifyLinear = function(x, w, b) {
  distances <- apply(x, 1, DistanceFromPlane, w, b)
  return(ifelse(distances < 0, -1, +1))
}

EuclideanNorm <- function(x) {
  return(sqrt(sum(x * x)))
}

PerceptronFunction <- function(x, y, learning.rate = 1) {
  w <- vector(length = ncol(x)) # initialize w
  b <- 0 # Initialize b
  # initialize empty dataframe
  errorEvolutions <- data.frame(cycles = numeric(), error = numeric()) 
  # initialize cycles
  cycles <- 0
  iterations <- 0 # count iterations (changes)
  R = max(apply(x, 1, EuclideanNorm))
  convergence <- FALSE # to enter the while loop
  while (!convergence) {
    convergence <- TRUE # hopes luck
    yc <- ClassifyLinear(x, w, b)
    # update cycles
    cycles <- cycles + 1
    # update error for this cycle
    errorEvolutions <- rbind(errorEvolutions, c(cycles, sum(abs(y - yc))))
    for (i in 1:nrow(x)) {
      if (y[i] != yc[i]) {
        convergence <- FALSE
        w <- w + learning.rate * y[i] * x[i,]
        b <- b + learning.rate * y[i] * R^2
        iterations <- iterations + 1
      }
    }
  }
  s = EuclideanNorm(w)
  return(list(w = w/s, b = b/s, steps = iterations, errorEvolution = errorEvolutions))
}


# very easy
# x2 = x1 + 1/2
x1 <- runif(50,-1,1)
x2 <- runif(50,-1,1)
x <- cbind(x1,x2)
y <- ifelse(x2 > 0.5 + x1, +1, -1)

PlotData <- function(x, y) {
  plot(x, pch = ifelse(y > 0, "+", "-"), xlim = c(-1,1), ylim = c(-1,1), cex = 2)
  abline(0.5,1)
  points(c(0,0), c(0,0), pch = 19)
  lines(c(0,-0.25), c(0,0.25), lty = 2)
  arrows(-0.3, 0.2, -0.4, 0.3)
  text(-0.45, 0.35, "w", cex = 2)
  text(-0.0, 0.15, "b", cex = 2)
}

PlotData(x, y)

the_perceptron <- PerceptronFunction(x,y)
predicted_y <- ClassifyLinear(x, the_perceptron$w, the_perceptron$b)
# error
print(sum(abs(y - predicted_y)))

