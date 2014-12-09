source('dampedSinusoid.r')
source('arModel.r')
source('armaModel.r')
source('laggedData.r')

library(lmtest) # for DW Test & Box.test

pend <- read.csv("pend_data.csv", header = T)
names(pend) <- c("time", "x.pixels", "y.pixels")

##############################
## Damped Sinusoid Model
##############################

# Convert the data to an angular position at each point in time
theta.i <- (180 / pi) * 
  atan2(pend$y.pixels - 1152.57606607263,
  pend$x.pixels - 394.773399557239) + 90

# Plot time series with theta.i
plot.ts(pend$time, theta.i, type = "b", lwd = .5, cex = .2,
  pch = 19, col = "blue",
  xlab = "Time (seconds)",
  ylab = "Pendulum Angular Position (Degrees)",
  main = "Pendulum Position Time Series",)
legend("bottomright", "Theta (degrees)", pch = 19, col = "blue") # Add legend

# Create t from 1 to n (n = length of the dataset)
t <- 1:nrow(pend)

# Call a function called dampedSinusoid
theta.hat <- dampedSinusoid(t, .002, 20, 1.137, pi/2, 0)

# Plot time series with theta.hat
plot.ts(pend$time, theta.hat, type="b", lwd = .5, cex = .2,
  pch = 19, col = "red",
  xlab = "Time (seconds)",
  ylab = "Pendulum Angular Position (Degrees)",
  main = "Predicted Pendulum Position Time Series")
  legend("bottomright", "Theta (degrees)", pch = 19, col = "red") # Add legend

# Estimate SSE, SSR, SSTO, R^2, and residuals
SSR <- sum((theta.hat - mean(pend$y.pixels))^2)
SSE <- sum((theta.i - theta.hat)^2)
SSTO <- SSR + SSE
(r_sq <- SSR / SSTO)
residuals <- theta.i - theta.hat

# Plot time series with residuals
plot.ts(pend$time, residuals, type="b", lwd = .5, cex = .2,
  pch = 19, col = "red",
  xlab = "Time (seconds)",
  ylab = "Model residuals (degrees)",
  main = "Residuals Plot in Time Series")
legend("bottomright", "Theta (degrees)", pch = 19, col = "red") # Add legend

##############################
## Autocorrelation
##############################

lag <- laggedData(theta.i, TRUE)

acf <- acf(lag, lag.max = 75, main = "")

mod <- lm(theta.i[1:(length(theta.i)-1)] ~ lag)

# Test for serial autocorrelation in lag-1
dwtest(mod)$p.value

# Test for serial autocorrelation in lag 1 to 75
Box.test(acf$acf, lag = 75, type = "Ljung-Box")$p.value

##############################
## Auto-Regressive Models and Forecasts
##############################

### Create a AR(2) model

Y.t <- arModel(theta.i, 2)

plot.ts(pend$time, Y.t, type="b", lwd = .5, cex = .2,
  pch = 20, col = "blue3",
  xlab = "Time (seconds)",
  ylab = "Pendulum Angular Position (Degrees)",
  main = "Pendulum Position Time Series")

### Create a AR(2) 1-step ahead forecast

Y.t <- arModel(theta.i, 2, TRUE)
  
plot.ts(pend$time, Y.t, type="b", lwd = .5, cex = .2,
  pch = 20, col = "lightblue4",
  xlab = "Time (seconds)",
  ylab = "Pendulum Angular Position (Degrees)",
  main = "Pendulum Position Time Series")

##############################
## Auto-Regressive Moving Average Models
##############################

### Create a ARMA model
# armaModel(ts, p, q, mu.y, sd, phi1, phi2, beta1, beta2, show.plot = FALSE)
# ts = time series data
# p = autoregressive terms
# q = moving average terms
# mu.y = population mean of y
# sd = population standard derivation
# phi1/phi2 = parameters of the AR model
# beta1/beta2 = parameters of the MA model

arma1.0 <- armaModel(theta.i, 1, 0, 5, 5, .9, -.7, .75, -.5, TRUE)
arma2.0 <- armaModel(theta.i, 2, 0, 5, 5, .9, -.7, .75, -.5, TRUE)
arma0.1 <- armaModel(theta.i, 0, 1, 5, 5, .9, -.7, .75, -.5, TRUE)
arma0.2 <- armaModel(theta.i, 0, 2, 5, 5, .9, -.7, .75, -.5, TRUE)
arma2.1 <- armaModel(theta.i, 2, 1, 5, 5, .9, -.7, .75, -.5, TRUE)

arma2.1.acf <- acf(arma2.1, lag.max = 50)
arma2.1.pacf <- pacf(arma2.1, lag.max = 50)
arma2.1.pacf <- as.vector(arma2.1.pacf$acf)

# To test whether there is a serial autocorrelation from lag 1 to 50,
# we use Ljung-Box test.
Box.test(as.vector(arma2.1.pacf), type="Ljung-Box")