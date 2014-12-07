# This function calculates an autoregressive model.
# ts = time series data
# p =  Maximum order of model to fit
arModel <- function(ts, p, step.ahead = FALSE)
{

  n <- length(ts)
  phi <- arima(ts, order = c(p,0,0))$coef

  if (step.ahead){
    mu.y <- mean(ts)
    epilson.t <- replicate(n, qnorm(runif(1), mean = 0, sd = 2))
    Y.t <- rep(0, n)
    i = 3:n
    Y.t[1] <- mu.y + (phi[1] * -mu.y + phi[2] * -mu.y) + epilson.t[1]
    Y.t[2] <- mu.y + (phi[1] * (ts[1] - mu.y) +
      (phi[2] * -mu.y)) + epilson.t[2]
    Y.t[i] <- mu.y + (phi[1] * (ts[i-1] - mu.y) +
      phi[2] * (ts[i-2] - mu.y)) + epilson.t[i]

    Y <- Y.t
  }else{

    y.hat <- rep(0, n)
    y.bar <- mean(ts)
    i = 2:n
    y.hat[1] <- y.bar + phi[1] * (ts[1] - y.bar) +
      phi[1] * - y.bar
    y.hat[i] <- y.bar + phi[1] * (ts[i] - y.bar) +
      phi[2] * (ts[i-1] - y.bar)

    Y <- y.hat
  }

  return(Y)
}