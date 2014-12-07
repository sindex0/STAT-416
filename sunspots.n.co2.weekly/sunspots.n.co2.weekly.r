library(TSA) # for periodogram

co2.weekly <- read.csv("co2_weekly_1974_to_2012.csv", header = T)
co2.weekly$date <- as.Date(co2.weekly$date, "%m/%d/%Y")

ssn.weekly <- read.csv("ssn_weekly_1974_to_2012.csv", header = T)
ssn.weekly$date <- as.Date(ssn.weekly$date, "%m/%d/%y")

t <- 1:length(ssn.weekly[,1])

# Write a least-squared regression model for both datasets
ssn.mod <- lm(ssn.weekly$weekly.average.ssn ~ t)
co2.mod <- lm(co2.weekly$average ~ t)

#####################################
## Plot Time Series for SSN Weekly 1974 TO 2012
#####################################

par(mar=c(5, 4, 4, 6) + 0.1)

pred.ssn <- predict(ssn.mod)

plot(ssn.weekly$weekly.average.ssn, 
  xlab = "Date (in years)", type = "b", cex = .5, col = "midnightblue",
  pch = 20, lwd = .2, ylab = "Sun Spot Number", axes = FALSE, xaxt="n")
axis(2); box()
lines(pred.ssn, col = "firebrick", lwd = 2)

par(new = TRUE)

#####################################
## Plot Time Series for CO2 Weekly 1974 TO 2012
#####################################

pred.co2 <- predict(co2.mod)

plot(pred.co2, xlab = "", type = "b", cex = .5, col = "firebrick",
  pch = 20, lwd = .2, ylab = "", axes = F)
mtext("Atmospheric CO2 Concentration",side=4,line=4) 
axis(4, ylim=c(320,400), las=1)

lines(co2.weekly$average, col = "royalblue")

# This add a "third plot" to tick the x-axis limits
par(new = TRUE)
plot(1,1, cex = .00003, axes = FALSE, xlab = "", ylab = "", col = "white",
  xlim = c(1974, 2012))

x.labels = seq(1974,2012, 6)
axis(1, at = x.labels)

legend("topleft", c("SSN Values", "CO2 Values"), cex = .9,
  pch = 20, lty = 1, col = c("midnightblue", "royalblue"), bty = "n")

#####################################
## Detrend each signal using linear least-squares regression
#####################################

par(mar=c(5, 4, 4, 6) + 0.1)

plot(ssn.mod$res, type = "b", cex = .75, pch = 20, col = "midnightblue",
  axes = FALSE, ylab = "Sun Spot Number", xlab = "")
axis(2); box()

par(new = TRUE)

plot(co2.mod$res, type = "b", cex = .75, pch = 20, col = "darkred",
  axes = FALSE, ylab = "", xlab = "")
mtext("Atmospheric CO2 Concentration",side=4,line=4)
axis(4, ylim=c(-6,10), las=1)

par(new = TRUE)
plot(1,1, cex = .00003, axes = FALSE, xlab = "Date (in years)", ylab = "",
  xlim = c(1974, 2012))

x.labels = seq(1974,2012, 6)
axis(1, at = x.labels)
legend("topright", c("Detrended SSN", "Detrended CO2"),
  pch = 20, lty = 1, col = c("midnightblue", "darkred"))

#####################################
## Pad each de-trended signal appropriately for CO2 weekly data
#####################################

dates <- seq(as.Date(min(co2.weekly$date)), as.Date(max(co2.weekly$date)),
  by = "month")
dates.frame.co2 <- data.frame(list(time = dates))

average.res.frame <- data.frame(list(average = co2.mod$res))

### Padding the time series
filled.co2 <- merge(dates.frame.co2, average.res.frame, all = TRUE)

#####################################
## Pad each de-trended signal appropriately for SSN weekly data
#####################################

dates <- seq(as.Date(min(ssn.weekly$date)), as.Date(max(ssn.weekly$date)),
  by = "month")
dates.frame.ssn <- data.frame(list(time = dates))

average.res.frame <- data.frame(list(average = co2.mod$res))

### Padding the time series
filled.ssn <- merge(dates.frame.ssn, average.res.frame, all = TRUE)

#####################################
## Apply a Hanning window to each signal
#####################################

n <- length(t) # n = sample size
j <- t # j = 1, 2, ... , 2016
w <- 1/2 * (1 - cos((2 * pi * j) / n))

# Multply hanning window by detrended signals of CO2/SSN weekly data
hann.co2 <- co2.mod$res  * w
hann.ssn <- ssn.mod$res  * w

par(mar=c(5, 4, 4, 6) + 0.1)
plot(hann.co2, type = "b", pch = 20, axes = FALSE,
  xlab = "", ylab = "Atmospheric CO2 Concentration",
  col = "firebrick2", ylim = c(-10,10))
axis(2); box()

par(new = TRUE)

plot(hann.ssn, type = "b", pch = 20, axes = FALSE, ylab = "",
  xlab = "", col = "midnightblue")
mtext("Sun Spot Number",side=4,line=4)
axis(4, las=1)

par(new = TRUE)
plot(1,1, cex = .00003, axes = FALSE, xlab = "Date (in years)", ylab = "",
  xlim = c(1974, 2012))

x.labels = seq(1974,2012, 6)
axis(1, at = x.labels)

legend("topright", c("Windowed SSN", "Windowed CO2"),
  pch = 20, lty = 1, bty = "n", cex = .75,
  col = c("midnightblue", "firebrick2"))

#####################################
## Calculate and plot FFT Periodograms 
## for the de-trended signals
#####################################

par(mar=c(5, 4, 4, 6) + 0.1)

periodogram(ssn.mod$res[1:60], col = "midnightblue")

par(new = TRUE)

periodogram(co2.mod$res[1:60], col = "darkred", axes = FALSE,
  ylab = "", xlab = "")

mtext("Periodogram",side=4,line=4)
axis(4, las=1)

legend("topright", c("Windowed SSN", "Windowed CO2"),
  lty = 1, bty = "n", col = c("midnightblue", "darkred"))

#####################################
## Calculate and plot FFT Periodograms 
## for the windowed + de-trended signals
#####################################

periodogram(hann.ssn[1:60], col = "midnightblue")

par(new = TRUE)

periodogram(hann.co2[1:60], col = "darkred", axes = FALSE,
  ylab = "", xlab = "")

mtext("Periodogram",side=4,line=4)
axis(4, las=1)

legend("topright", c("PSD Windowed CO2", "PSD Windowed SSN"),
  col = c("navyblue", "darkred"), lty = 1)

#####################################
## Plot the cross-correlation between the 
## de-trended signals over -1,000 to +1,000 lags
#####################################

ccf(ssn.weekly$weekly.average.ssn, co2.weekly$average, lag.max = 1000,
  main = "Cross Correlation for Weekly Average SSN &
  Weekly average CO2 (ppmV)", col = "deepskyblue")
