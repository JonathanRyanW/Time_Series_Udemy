x = as.POSIXct("2019-12-25 11:45:34")
y = as.POSIXlt("2019-12-25 11:45:34")
z = as.POSIXlt("1900-12-25 11:45:34")
x;y

unclass(x)
unclass(y)

z$year

round(0.375, digits =2)
ceiling(0.35)
floor(0.35)

trunc(2.45)
trunc(2.76)
trunc(-2.8)


ymd(20160101) + dmonth(2)

# There is no dmonth function!

x <- strptime("2014-04-12 23:12", format = "%Y-%m-%d %H:%M", tz = "CET" )
x

class(x) #x is POSIXlt
unclass(x)

x <- ymd_hm("2014-04-12 23:12", tz = "CET")
class(x) #x is POSIXct
unclass(x)

x <- ymd_hm("2014-04-12 12:12", tz = "CET")
unclass(x)

#creating a random walk
set.seed(1000)
x <- cumsum(rnorm(1000)) 
plot(x, type = "l")

library(tseries)
adf.test(x) #Random walk is not stationary.
Box.test(x, lag = 1, type = "Ljung-Box") 
acf(x)

#x is autocorrelated. A random walk is autocorrelated.

library(lmtest)
dwtest(x[-1000] ~ x[-1]) #DW result is no autocorrelation

set.seed(1001)
rnorm(2)

set.seed(1001)
rnorm(5)

# Testing the ses function
set.seed(101)
n <- rnorm(500)

plot(n, type = "l")
lines(ses(n)$fitted, col = "red")
lines(ses(n)$mean, col = "red")
lines(SMA(n, 10), col = "blue")

# ETS model for Airpassengers
ets(AirPassengers)

# Multiplicative error, Additive trend, Multiplicative seasonality.

plot(ets(AirPassengers)$residuals)

# THe residuals look random. Compared to the Seasonal Decomposition model, it
# does not exhibit any seasonal pattern at the beginning and the end of the plot

# Checking for normality

library(nortest)
ad.test(ets(AirPassengers)$residuals)

# The residuals are normally distributed. This is a very good model compared to
# the seasonal decomposition model with both additive and multiplicative model.

set.seed(123) 
simul <- arima.sim(model = list(order = c(2,0,1), ar = c(0.4, 0.5), ma = c(0.3)),
          n = 1000)
tsdisplay(simul)
auto.arima(simul)

# Stationarity
adf.test(simul)

Box.test(simul, lag = 1, type = "Ljung-Box")

#There is autocorrelation.

set.seed(1234)
acf(rnorm(1000))

set.seed(123) 
simul <- arima.sim(model = list(order = c(2,0,0), ar = c(0.4, 0.5)),
                   n = 1000)
tsdisplay(simul)
auto.arima(simul)

adf.test()
mean(simul)


















