library(TTR) # for SWMA
library(lmtest) # for DW Test
library(forecast) # for forecast.ARIMA & forecast.plot

source('movingAverage.r')
source('laggedData.r')
source('maModel.r')

geiger.data <- read.table("source_radioactivity.txt")

names(geiger.data) = c("date", "time", "detection")

# Make a time series plot
plot.ts(geiger.data$time, geiger.data$detection, 
  type="b", lwd = .5, cex = .2, pch = 19, col = "black",
  xlab = "Time (Per 5 seconds)", ylab = "Detections (#)",
  main = "Geiger Counter Time Series")
legend("topleft", c("# of Detections", "Simple Moving Average",
  "Centered Moving Average"), pch = 19, 
  cex = .5, col = c("black", "red", "purple"), lty = 1) # Add legend

##############################
## Moving Average Smoothings
##############################

n = 24

# Calculate SMA
sma = movingAverage(geiger.data$detection, n)

# Calculate SWMA
swma = WMA(geiger.data$detection, n, wts=1:n)

# Calculate CMA
cma <- movingAverage(geiger.data$detection, 24, TRUE)

# Subset data from 07:00 to 08:00
sub.geiger.data = geiger.data[9764:10483,] 
plot.ts(sub.geiger.data$time, sub.geiger.data$detection , 
  type="b", lwd = .5, cex = .2,
  pch = 19, col = "black",
  xlab = "Time (Per 5 seconds)",
  ylab = "Detections (#)",
  main = "Geiger Counter Time Series")

legend("topleft", c("# of Detections", "Simple Moving Average",
  "Weighted Moving Average",
  "Centered Moving Average"), lwd = 2, 
  cex = .65, col = c("black", "red", "purple", "blue")) # Add legend

lines(sma, col = "red", lwd = 2)
lines(swma, col = "blue", lwd = 2)
lines(cma, col = "purple", lwd = 2)

##############################
## Lagged Scatterplot
##############################

set.seed(47)

# Subset 300-point conservative data values
geiger.data.300 <- geiger.data[500:799,]
plot.ts(geiger.data.300$time, geiger.data.300$detection, 
  xlab = "Time (secs)", ylab = "# of Detection", type = "b", pch = 20)
abline(h = mean(geiger.data.300$detection), col = "red", lwd = 2)

# Create a lag variable using laggedData function
lag <- laggedData(geiger.data.300[,3], TRUE)

##############################
## Autocorrelations & Hypotheses Tests
##############################

# Create a correlogram from lag 1 to 75
acf <- acf(lag, lag.max = 75,main = "")

n <- dim(geiger.data.300)[1]
mod <- lm(geiger.data.300$detection[1:(n-1)] ~ lag)

# Test a Durbon-Watson test for the first lag
dwtest(mod)$p.value # p-value = .4812

# Test a Ljung-Box test from lags 1 to 75.
Box.test(acf$acf, lag = 75, type = "Ljung-Box")$p.value # p-value = .988

##############################
## Moving Average Models
##############################

# Create a MA(1) model
Y.t <- maModel(geiger.data.300$detection, .5, FALSE)

# Create a MA(1) 1-step ahead forecast
Y.t.2 <- maModel(geiger.data.300$detection, .5, TRUE)

plot.ts(geiger.data.300$detection, 
  xlab = "Time (secs)", ylab = "Detections in 5 Seconds",
  type = "b", pch = 20, lwd = 1)
lines(Y.t, type = "l", col = "red", lwd = 2)
lines(Y.t.2, type = "l", col = "navy", lwd = 2)

legend("topright", c("MA(1) Model", "MA(1) 1-Step \n Ahead Forecast"),
  col = c("red", "navy"), lwd = 2, cex = .7,  bty = "n")

##############################
## Selecting an ARIMA Model
##############################

par(mfrow=c(3,1))
plot.ts(geiger.data$time, geiger.data$detection, 
  xlab = "Time (secs)", ylab = "# of Detection", 
  main = "Geiger Counter Data", type = "b", pch = 20)
abline(h = mean(geiger.data$detection), lwd = 1.5, col = "red3")
acf(geiger.data$detection, 150, main = "")
pacf(geiger.data$detection, 150,main = "")

# Calculate first-differenced signals
diff1 <- diff(geiger.data$detection, differences=1)

# Calculate second-differenced signals
diff2 <- diff(geiger.data$detection, differences=2)

par(mfrow=c(2,1))
acf(diff1, 150, main = "First-Differenced Signal")
pacf(diff1, 150,main = "")

par(mfrow=c(3,1))
plot.ts(diff2,
  xlab = "Time (secs)", ylab = "# of Detection", 
  main = "2nd Difference Geiger Counter Data", type = "b", pch = 20)
abline(h = mean(diff2), lwd = 1.5, col = "red3")
legend("bottomright", c("SD = ", paste(round(sd(diff2),2))), cex = .45)
acf(diff2, 150, main = "")
pacf(diff2, 150,main = "")

# Selecting my model in ARIMA
my.arima <- arima(geiger.data$detection, order = c(0, 0, 1),
      seasonal = list(order = c(0, 0, 0), period = 0))

## Write model residuals plots for ACF and PACF
t = time(geiger.data$time)
mod <- lm(detection ~ t, data = geiger.data)
par(mfrow=c(2,1))
acf(my.arima$res, 150, main = "Model Residuals")
pacf(my.arima$res, 150,main = "")

# Write an ARIMA model for the residuals and compare 
# well-known criteria
arima.mod <- arima(mod$res, order = c(0, 0, 1),
      seasonal = list(order = c(0, 0, 0), period = 300))

dev.off()

# Forecast 100 point ahead
forecast = forecast.Arima(my.arima, h=100)

# plot the forecasts
# The forecast in the next 100 points will be 7.26 and
# we are 95% confident that a next detection that is recorded
# in the next five seconds is between 1.85 and 12.68.
plot.forecast(forecast, type = "b", pch = 19, cex = .2,
  ylab = "# of Detections", xlab = "Time (s)")