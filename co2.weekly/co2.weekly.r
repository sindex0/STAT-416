library(forecast) # for forecast.ARIMA & forecast.plot

co2.weekly.data <- read.csv("co2_weekly_1974_to_2012.csv", header = T)

co2.weekly.data$date <- as.Date(co2.weekly.data$date, "%m/%d/%Y")

year <- substr(co2.weekly.data$date, 1, 4)
month <- substr(co2.weekly.data$date, 6, 7)
day <- substr(co2.weekly.data$date, 9, 10)
date <- paste(year, ".", month, day, sep = "")

start <- as.numeric(date)[1]
finish <- tail(as.numeric(date),1)

time.period <- seq(start, finish, length = length(date))

# Label the x-axis with an appropriate time
time.labels <- seq(start, finish, 2)

plot.ts(co2.weekly.data[,2], type = "l", pch = 19, lty = 1, axes = FALSE,
  xlab = "Time (hrs.mins.secs)", 
  ylab = "Atmospheric CO2 Concentration (ppmv)", 
  main = "Time Series of Weekly CO2 Data")
axis(2); box()
legend("bottomright", "ppmv", bty = "n", col = "black", lty = 1)

par(new = TRUE)

plot(0,0, cex = .01, col = "white", axes = FALSE, ylab = "",
  xlim = c(start, finish),
  xlab = "")
axis(1, at = time.labels)

##############################
## Holt-Winters Method
##############################

t <- dim(co2.weekly.data)[1]
co2.2005 <- co2.weekly.data[1600:t,]
year <- substr(co2.2005$date, 1, 4)
year <- as.numeric(year)

# Calculate holt-winters to forecast 15-periods ahead
hw.co2 <- holt(co2.2005[,2], 
  h = 30, alpha = .2, beta = .2)


plot(hw.co2, axes = FALSE,
  xlab = "Time (years)", 
  ylab = "Atmospheric CO2 Concentration (ppmv)", 
  main = "Time Series of Weekly CO2 Data")

axis(2); box()
legend("topleft", c("ppmv","forecast", "95% Prediction Interval"),
  lwd = 1, cex = .75,col = c("black", "midnightblue",
  "lightskyblue4"), bty = "n")
par(new = TRUE)

plot(0,0, cex = .01, col = "white", axes = FALSE, ylab = "",
  xlim = c(year[1], 2015),
  xlab = "")
axis(1, at = seq(year[1], 2015, 1))

##############################
## Selecting an ARIMA Model
##############################

par(mfrow=c(3,1))
plot.ts(co2.weekly.data$average, 
  xlab = "Time", ylab = "Atmospherical CO2 Concentration (ppmv)", 
  main = "CO2 Weekly Concentration", axes = FALSE)
axis(2); box()
acf(co2.weekly.data $average, 150, main = "")
pacf(co2.weekly.data$average, 150,main = "")

# Calculate first-difference signals
diff1 <- diff(co2.weekly.data$average, differences=1)

# Calculate second-difference signals
diff2 <- diff(co2.weekly.data$average, differences=2)

par(mfrow=c(2,2))
acf(diff1, 150, main = "First-Difference Signal")
pacf(diff1, 150,main = "First-Difference Signal")

# Calculate first-seasonal difference signals
seasonal1 <- diff1[1:52] - diff1[52]

acf(seasonal1, lag.max = 150, main = "First-Seasonal Difference")
pacf(seasonal1, lag.max = 150, main = 'First-Seasonal Difference')

arima <- arima(co2.weekly.data$average, order = c(2, 1, 2),
      seasonal = list(order = c(0, 0, 0), period = 52))

par(mfrow=c(2,1))
acf(arima$res, 150, main = "Model Residuals")
pacf(arima$res, 150,main = "")

dev.off()
forecast = forecast.Arima(arima, h=104) 
 
# plot the forecasts
plot.forecast(forecast,
  ylab = "Atmospherical CO2 Concentration (ppmv)", xlab = "Time")