# This function returns a vector of lagged values
# ts = time series data
# show.plot = display a lagged scatterplot to discover pattern
# in which it'll indicate autocorrelation
laggedData <- function(ts, show.plot = FALSE)
{
  n <- length(ts)
  lag <- rep(0, n)
  lag <- ts[2:n]
  ts <- ts[1:(n-1)]

  if (show.plot)
  {
    plot(ts, lag, xlab = "Yi", ylab = "Yi+1",
      main = "Lagged Scatterplot", pch = 20)
    mod <- lm(ts ~ lag)
    abline(mod, col = "red", lwd = 2)
    paste.intercept <- unname(round(mod$coef[1], 3))
    paste.slope <- unname(round(mod$coef[2], 3))
    SSE <- sum((ts[1:(n-1)] - mod$fitted.values)^2)
    SSTO <- sum((ts[1:(n-1)] - mean(ts))^2)
    r.sq <- 1 - (SSE / SSTO)
    paste.r.sq <- paste("R^2 =", round(r.sq, 3))
    paste.model <- paste("y = ", paste.intercept, " - ", paste.slope,
      "x", sep = "")
    legend("topright", c(paste.model, paste.r.sq), cex = .8)
  }

  return(lag)
}