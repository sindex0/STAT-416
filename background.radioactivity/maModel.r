# This function calculates and returns a moving average model.
# ts = time series data
# theta =
# step.ahead = whether to calculate a one-step ahead forecast at time t.n
maModel <- function(ts, theta, step.ahead = FALSE)
{
  n <- length(ts)
  Y.t <- rep(0, n)
  y.bar <- mean(ts)
  eplison.i <- replicate(n, qnorm(runif(1), mean = 0, sd = 2))

  if (step.ahead){
    Y.t <- y.bar + (theta * eplison.i)
  }else{
    Y.t[1] <- y.bar + eplison.i[1]
    i <- 2:n
    Y.t[i] <- y.bar + (theta * eplison.i[i-1]) + eplison.i[i]
  }

  return(Y.t)
}