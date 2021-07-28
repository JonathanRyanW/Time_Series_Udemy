### Decomposing Time Series (U)

plot(nottem)

frequency(nottem)

length(nottem)

decompose(nottem, type = "additive")

plot(decompose(nottem, type = "additive"))

library(forecast)

library(ggplot2)

# library(ggplot2 and forecast)
autoplot(decompose(nottem, type = "additive"))

# alternatively the function stl could be used
plot(stl(nottem, s.window="periodic"))

stl(nottem, s.window="periodic")

# seasonal adjustment
mynottem = decompose(nottem, "additive")

class(mynottem)

# we are subtracting the seasonal element
nottemadjusted = nottem - mynottem$seasonal

# getting a plot
plot(nottemadjusted)

plot(mynottem$seasonal)

# a stl forecast from the package forecast
library(forecast)
plot(stlf(nottem, method = "arima"))

### Exercise Decomposition

plot(AirPassengers)
AirPassengers
frequency(AirPassengers)

mymodel1 = decompose(AirPassengers, type = "additive")

mymodel2 = decompose(AirPassengers, type = "multiplicative")

plot(mymodel1)

plot(mymodel2)


plot(mymodel1$x - mymodel1$seasonal)

plot(mymodel1$trend + mymodel1$random)
plot(mymodel2$trend + mymodel2$random)

# This is interesting. the seasonally adjusted series using the multiplicative
# method resembles a simple line very closely compared to the additive method
# This is actually expected since we do see that the seasonal factor seems to
# vary with time. 

plot(mymodel2$seasonal)

### SMOOTHING SMA
install.packages("TTR")
library("TTR")

# in order to identify trends, we can use smoothers
# like a simple moving avg

# n identfies the order or the SMA - you can experiment with this parameter

x = c(1,2,3,4,5,6,7)

SMA(x, n = 3) # SMA fro TTR package, 3rd order
plot(x, type = "l")
lines(SMA(x, n = 3))

plot(lynx)
lines(SMA(lynx, n = 3), col = 'red')
lines(SMA(lynx, n = 10), col = "blue")


lynxsmoothed = SMA(lynx, n = 9); lynxsmoothed

# we can compare the smoothed vs the original lynx data

plot(lynx)

plot(lynxsmoothed)




# Exponential Smoothing with ets

## ets

library(forecast)

plot(nottem)

# Using function ets
etsmodel = ets(nottem); etsmodel

# The result is an ETS(A,N,A), which means additive error, no trend, additive
# seasonality

# Plotting the model vs original
plot(nottem)
lines(etsmodel$fitted, col = "red")

# Plotting the forecast
plot(forecast(etsmodel, h = 12))

# Changing the prediction interval
plot(forecast(etsmodel, h = 12, level = 95))

# Manually setting the ets model
etsmodmult = ets(nottem, model ="MZM"); etsmodmult

# Plot as comparison
plot(nottem, lwd = 3)
lines(etsmodmult$fitted, col = "red")