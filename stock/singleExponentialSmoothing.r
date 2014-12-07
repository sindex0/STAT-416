# This function returns a single exponential smoothing
# With an estimate of the time series
# ts = Time Series data
# N = Most recent N periods contain 86% of weights
# show.mape = Calculate MAPE if set to TRUE
singleExponentialSmoothing <- function(ts, N, theta = 0, show.mape = FALSE)
{

  ts <- ts[!is.na(ts)]

  if (theta == 0)
    theta <- 2 / (N + 1)

  y.i <- rep(0, length(ts))
  y.i[1] <- ts[1]

  for (i in 2:length(ts))
    y.i[i] <- (theta * ts[i]) + ((1 - theta) * y.i[i - 1])

  if (show.mape)
  {
    mape <- 1 / length(ts) * sum(abs((ts - y.i) / ts)) * 100
    print(mape)
  }

  return(y.i)
}