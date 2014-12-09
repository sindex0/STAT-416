library(TTR) # for SWMA
library(forecast) # for forecast.ARIMA & forecast.plot

setwd("G:\\F-\\Fall 2014\\Stat 416\\Project\\Datasets")

stock.data <- read.table("http://hopey.netfonds.no/tradedump.php?date=20141205&paper=FLIR.O&csv_format=txt",
  skip = 2)[,1:2]

# Reformat the dataset with cleaner date and time
stock.data <- stock.data[,1:2]
names(stock.data) <- c("Time", "Stock")
year <- substr(stock.data$Time, 1, 4)
month <- substr(stock.data$Time, 5, 6)
day <- substr(stock.data$Time, 7, 8)
stock.data$Date <- paste(year, "-", month, "-", day, sep = "")
hours <- substr(stock.data$Time, 10,11)
minutes <- substr(stock.data$Time, 12,13)
seconds <- substr(stock.data$Time, 14,15)
stock.data$Time <- paste(hours, ".", minutes, seconds, sep = "")
stock.data <- data.frame(stock.data$Date, stock.data$Time, stock.data$Stock)
names(stock.data) <- c("Date", "Time", "Stock")

setwd("G:\\F-\\Fall 2014\\Stat 416\\Project\\Functions")

source("singleExponentialSmoothing.r")

start <- as.numeric(levels(stock.data$Time))[1]
finish <- tail(as.numeric(levels(stock.data$Time)),1)

time.period <- seq(start, finish, length = length(stock.data$Time))

# Label the x-axis with an appropriate time
time.labels <- seq(start, finish, 1)

##############################
## Plot Stock Data
##############################

five.mins.avg <- WMA(stock.data$Stock, 5, wts = 1:5)
five.mins.avg <- ts(five.mins.avg)
plot.ts(five.mins.avg, ylab = "5-Minutes Weighted Average (# of Stock)",
  xlab = "Time (hrs.mins.seconds)",
  main = "Stock Data (2014-12-05)", axes = FALSE)
axis(2); box()
par(new = TRUE)

plot(0,0, cex = .01, col = "white", axes = FALSE, ylab = "",
  xlim = c(time.period[1], time.period[length(time.period)]),
  xlab = "")
axis(1, at = time.labels)

par(new = TRUE)
plot.ts(five.mins.avg, xlab = "", ylab = "", axes = FALSE)

##############################
## Moving Average Smoothings
##############################

ewma.50 <- singleExponentialSmoothing(five.mins.avg, 50, 0, TRUE)
ewma.5 <- singleExponentialSmoothing(five.mins.avg, 5, 0, TRUE)
alpha <- unname(ets(five.mins.avg[!is.na(five.mins.avg)])$par[1])
ewma.optimal <- singleExponentialSmoothing(five.mins.avg, 5, alpha, TRUE)

lines(ewma.50, col = "red", lty = "solid")
lines(ewma.5, col = "blue", lty = "solid")
lines(ewma.optimal, col = "limegreen", lty = "solid")

legend("topleft", c("5-Minutes Average", "EWMA (N = 5)", "EWMA (N = 50)",
  paste("Optimal Value =", round(alpha,2))), lwd = 1, cex = .75,
  col = c("black", "red", "blue", "limegreen"), bty = "n")

##############################
## Forecasting
##############################

forecast.ewma.50 <- HoltWinters(ewma.50, beta=FALSE, gamma=FALSE)
forecast.ewma.50.2 <- forecast.HoltWinters(forecast.ewma.50, h=15)
plot.forecast(forecast.ewma.50.2, col = "red", axes = FALSE)
axis(2); box()

legend("topleft", c("5-Minutes Average", "EWMA (N = 50)",
  "forecast", "95% Prediction Interval"),
  lwd = 1, cex = .75,col = c("black", "red", "midnightblue",
  "lightskyblue4"), bty = "n")

par(new = TRUE)

plot.ts(five.mins.avg, ylab = "5-Minutes Weighted Average (# of Stock)",
  xlab = "Time (hrs.mins.seconds)",
  main = "", axes = FALSE)

par(new = TRUE)

plot(0,0, cex = .01, col = "white", axes = FALSE, ylab = "",
  xlim = c(time.period[1], time.period[length(time.period)]),
  xlab = "")
axis(1, at = time.labels)

##############################
## Double Exponential Smoothing
##############################

# Calculate Double Exponential Smoothing
dexps <- HoltWinters(five.mins.avg[!is.na(five.mins.avg)],
  alpha = .2, beta = .2, gamma=FALSE)

# Get optimal values for alpha and beta
optimal.values <- ets(five.mins.avg[!is.na(five.mins.avg)])

# Calculate Double Exponential Smoothing when gamma = FALSE
optimal.dexps <- HoltWinters(five.mins.avg[!is.na(five.mins.avg)],
  alpha = optimal.values$par[1], 
  beta = optimal.values$par[2], gamma=FALSE)

plot.ts(five.mins.avg, ylab = "5-Minutes Weighted Average (# of Stock)",
  xlab = "Time (hrs.mins.seconds)", axes = FALSE)
axis(2); box()

legend("topleft", c("5-Minutes Average", "Double Exp Smoothing",
  paste("Optimal Values: alpha =", 
    round(optimal.dexps$alpha, 3), "\n& beta =", 
    round(optimal.dexps$beta, 3))),
  lwd = 1, cex = .7, col = c("black", "red", "green3"), bty = "n")

par(new = TRUE)

plot.ts(dexps$fit[,1], 
  ylab = "", xlab = "", col = "red", axes = FALSE)

par(new = TRUE)

plot.ts(optimal.dexps$fit[,1], 
  ylab = "", xlab = "", col = "darkgreen", axes = FALSE)

par(new = TRUE)

plot(0,0, cex = .01, col = "white", axes = FALSE, ylab = "",
  xlim = c(time.period[1], time.period[length(time.period)]),
  xlab = "")
axis(1, at = time.labels)

##############################
## Forecasting
##############################

forecast.dexps <- forecast.HoltWinters(dexps, h=15)
plot.forecast(forecast.dexps, col = "black", axes = FALSE)
axis(2); box()

legend("topleft", c("5-Minutes Average", "Double Exp Smoothing",
  "forecast", "95% Prediction Interval"),
  lwd = 1, cex = .75,col = c("black", "red", "midnightblue",
  "lightskyblue4"), bty = "n")

par(new = TRUE)

plot(0,0, cex = .01, col = "white", axes = FALSE, ylab = "",
  xlim = c(time.period[1], time.period[length(time.period)]),
  xlab = "")
axis(1, at = time.labels)

##############################
## Selecting an ARIMA model
##############################

par(mfrow=c(3,1))
plot.ts(five.mins.avg, type = "b", pch = 20, cex = .5,
  xlab = "Time (hrs.mins.sec)", 
  ylab = "",
  main = "Stock Data (2014-12-05)", axes = FALSE)
axis(2); box()
acf(five.mins.avg[!is.na(five.mins.avg)], 
  lag.max = 150, main = "")
pacf(five.mins.avg[!is.na(five.mins.avg)], lag.max = 150, main = "")

# Calculate the first-differenced signals
diff1 <- diff(five.mins.avg[!is.na(five.mins.avg)], differences=1)

# Calculate the second-differenced signals
diff2 <- diff(five.mins.avg[!is.na(five.mins.avg)], differences=2)

par(mfrow=c(2,1))
acf(diff1, 150, main = "First-Differenced Signal")
pacf(diff1, 150,main = "")

my.arima <- arima(five.mins.avg[!is.na(five.mins.avg)], order = c(1, 1, 0))

par(mfrow=c(2,1))
acf(my.arima$res, 150, main = "Model Residuals")
pacf(my.arima$res, 150, main = "")

forecast.stock <- forecast.Arima(my.arima, h=200)

dev.off()

# plot the forecasts
plot.forecast(forecast.stock, type = "b", pch = 19, cex = .1,
  ylab = "# of Stocks", xlab = "Time (hrs/mins/secs)",
  axes = FALSE)
axis(2); box()

# The market closes at 00:00
market.close <- 24

# Label the x-axis with an appropriate time
time.labels <- seq(start, market.close, length = length(stock.data$Time))
time.period <- seq(start, market.close, 2)

par(new = TRUE)

plot(0,0, cex = .01, col = "white", axes = FALSE, ylab = "",
  xlim = c(time.labels[1], market.close),
  xlab = "")
axis(1, at = time.period)

result <- c(tail(forecast.stock$x,1), 
  tail(forecast.stock$lower,1)[2], tail(forecast.stock$upper,1)[2])

names(result) <- c("Stock @ 00:00hrs", "95% Lower", "95% Upper")

# At the end of the day, I expect to make $31.85. I am
# 95% confident that a number of stock I will make at the end
# of the day is between $31.63 & $32.07.