# This function calculates an autoregressive moving average model.
# For the class purpose, this will return only when p < 3 and q < 2.
#
# ts = time series data
# p = autoregressive terms
# q = moving average terms
# mu.y = population mean of y
# sd = population standard derivation
# phi1/phi2 = parameters of the AR model
# beta1/beta2 = parameters of the MA model
# show.plot = show ts plot, acf and pacf plots with maximum lags of 50
armaModel <- function(ts, p, q, mu.y, sd, phi1, phi2, beta1, beta2, show.plot = FALSE)
{

  stopifnot((p >= 0 && p < 3) || (q >= 0 && q < 2))

  n = length(ts)
  epilson <- replicate(n, qnorm(runif(1), mean = 0, sd = 5))
  arma <- rep(0, n)

  if (p == 1){
    i <- 2:n
    arma[1] <- mu.y + 
      (phi1 * - mu.y) +
      epilson[1]
    arma[i] <- mu.y + 
      (phi1 * (ts[i - 1] - mu.y)) +
      epilson[i]

  }else if (p > 1){
    i <- 3:n
    arma[1] <- mu.y + (phi1 * -mu.y + 
      phi2 * -mu.y) + epilson[1]
    arma[2] <- mu.y + (phi1 * 
      (ts[1] - mu.y)) + 
      (phi2 * -mu.y) + epilson[2]
    arma[i] <- mu.y + (phi1 * 
      (ts[i-1] - mu.y) +
      phi2 * (ts[i-2] - mu.y)) + epilson[i]

  }else if (q == 1){
    i = 2:n
    arma[1] <- mu.y + epilson[1]
    arma[i] <- mu.y + 
      (beta1 * epilson[i - 1]) + epilson[i]
  }else if (q > 1){
    i = 3:n
    arma[1] <- mu.y + epilson[1]
    arma[2] <- mu.y + 
      (beta1 * epilson[2]) + epilson[2]
    arma[i] <- mu.y + 
      (beta1 * epilson[i-1]) + 
      (beta2 * epilson[i-2]) + epilson[i]

  }else if (p == 1 && q == 1){
    arma[1] <- mu.y +
      (phi1 * -mu.y) +
      (phi2 * -mu.y) + epilson[1]

    i = 2:n
    arma[i] <- mu.y + 
      (phi1 * (ts[1] - mu.y)) +
      (phi2 * - mu.y) +
      (beta1 * epilson[1]) + epilson[2]

  }else if (p == 2 && q == 1){
    arma[1] <- mu.y +
      (phi1 * -mu.y) +
      (phi2 * -mu.y) + epilson[1]

    i = 3:n
    arma[2] <- mu.y + 
      (phi1 * (ts[1] - mu.y)) +
      (phi2 * - mu.y) +
      (beta1 * epilson[1]) + epilson[2]

    arma[i] <- mu.y + 
      (phi1 * (ts[i - 1] - mu.y)) +
      (phi2 * (ts[i - 2]- mu.y) +
      (beta1 * epilson[i - 1]) + 
      (beta2 * epilson[i - 2])) + epilson[i]
  }

  if (show.plot)
  {
    paste.arma.text <- paste("ARMA(", p, ",", q, ")", sep = "")
    par(mfrow= (c(3,1)))
    plot.ts(arma, type = "b", pch = 20,  col = "grey3", 
      ylab = "Y.t", main = paste.arma.text)
    acf <- acf(arma, lag.max = 50, main = paste("ACF:",paste.arma.text))
    pacf <- pacf(arma, lag.max = 50, main = paste("PACF:",paste.arma.text))
  }

  return(arma)

}