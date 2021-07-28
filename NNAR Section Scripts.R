## Import the APTelectricity.csv file as APTelectric
library(tseries)
library(forecast)
library(ggplot2)
library(nortest)
library(moments)
library(fitdistrplus)
library(logspline)
library(MTS)

APTelectric <- read.csv("C:/Users/jonat/Downloads/Online Course/Time Series/APTelectricity.csv")
myts = ts(APTelectric$watt, frequency = 288)



plot(myts)

adf.test(myts) # Non-stationary time series


set.seed(34)


# Creating NN
fit = nnetar(myts)
fit

# The chosen parameters for the NN are (14,1,8) which means we have 14 lags and
# 1 seasonal lag as the input layer and we have 8 nodes at the hidden layer.
# We only have 1 hidden layer.


# Evaluating the residuals
mean(fit$residuals, na.rm = T)
sd(fit$residuals, na.rm = T)

# The mean is very close to 0, a very good result.

hist(fit$residuals) #Histogram looks normal-like

skewness(na.omit(fit$residuals))
kurtosis(na.omit(fit$residuals))

# The skewness and kurtosis are approximately normal-like

plotdist(as.numeric(fit$residuals))
descdist(as.numeric(na.omit(fit$residuals)), discrete = F, boot = 1000)

# As can be seen in the skewness and kurtosis graph, the residuals are
# approximately normally distributed

# Performing the Anderson-Darling test for normality
ad.test(fit$residuals)

# We cannot reject the null hypothesis that the residuals are normally
# distributed. This is a very good result.

# Testing for autocorrelation
acf(na.omit(fit$residuals))
pacf(na.omit(fit$residuals))

# So far so good. There is no autocorrelation even in the first lags.

# Performing Ljung-Box test for autocorrelation
Box.test(na.omit(fit$residuals), lag = 1, type = "Ljung-Box")
Box.test(na.omit(fit$residuals), lag = 2, type = "Ljung-Box")
Box.test(na.omit(fit$residuals), lag = 3, type = "Ljung-Box")
Box.test(na.omit(fit$residuals), lag = 4, type = "Ljung-Box")
Box.test(na.omit(fit$residuals), lag = 5, type = "Ljung-Box")

# None of the lags tested are showing autocorrelation. The residuals are not
# serially correlated. A very good result.

# Testing for heteroscedasticity
plot(na.omit(fit$residuals))

# Based on the residual plot, it does not look like a heteroscedastic series
archTest(na.omit(fit$residuals))

# The p-value is > 0.05. We cannot reject the null hypothesis that the variance
# of the residuals is constant. Thus, the residuals are homoscedastic. Good.

#Creating a forecast
nnetforecast <- forecast(fit, h = 400, PI = F)

# Plotting the forecast
autoplot(nnetforecast)



## Using an external regressor in a neural net
set.seed(1001)
fit2 = nnetar(myts, xreg = APTelectric$appliances)

# Evaluating the residuals
#
#
#
#


# Defining the vector which we want to forecast


# Creating a forecast for the number of appliances
y =rep(2, times = 12*10)

# Creating a forecast for the watts
nnetforecast <- forecast(fit2, xreg = y, PI = F)

# Plotting
autoplot(nnetforecast)
