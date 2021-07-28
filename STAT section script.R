lynx

time(lynx)

length(lynx)

tail(lynx) # last 6 observations

mean(lynx); median(lynx)

plot(lynx)

sort(lynx)

sort(lynx)[c(57,58)]

quantile(lynx)

quantile(lynx, prob = seq(0, 1, length = 11), type = 5)

### simple forecast methods

set.seed(95)
myts <- ts(rnorm(200), start = (1818))
plot(myts)

library(forecast)
meanm <- meanf(myts, h=20)
mean(myts)
naivem <- naive(myts, h=20)

rwm <- rwf(myts, h = 20, drift = F) #Error if drift=F
driftm <- rwf(myts, h=20, drift = T)

plot(meanm, plot.conf = F, main = "")
lines(naivem$mean, col=123, lwd = 2)
lines(driftm$mean, col=22, lwd = 2)
legend("topleft",lty=1,col=c(4,123,22),
       legend=c("Mean method","Naive method","Drift Method"))

plot(driftm)
plot(meanm)
plot(naivem)
###### accuracy and model comparison

set.seed(95)
myts <- ts(rnorm(200), start = (1818))
mytstrain <- window(myts, start = 1818, end = 1988)
plot(mytstrain)

meanm <- meanf(mytstrain, h=30)
naivem <- naive(mytstrain, h=30)
driftm <- rwf(mytstrain, h=30, drift = T)

mytstest <- window(myts, start = 1988)

accuracy(meanm, mytstest)
accuracy(naivem, mytstest)
accuracy(driftm, mytstest)

###### Residuals

set.seed(95)
myts <- ts(rnorm(200), start = (1818))
plot(myts)

library(moments)
skewness(myts)
kurtosis(myts)

meanm <- meanf(myts, h=20)
naivem <- naive(myts, h=20)
driftm <- rwf(myts, h=20, drift = T)

var(meanm$residuals)
mean(meanm$residuals)



library(nortest)
ad.test(myts)

mean(naivem$residuals)
mean(naivem$residuals, na.rm=T)
var(naivem$residuals, na.rm=T)

naivwithoutNA <- naivem$residuals
naivwithoutNA <- naivwithoutNA[2:200]
var(naivwithoutNA)
mean(naivwithoutNA)


mean(driftm$residuals, na.rm=T)
var(driftm$residuals, na.rm=T)

driftwithoutNA <- driftm$residuals
driftwithoutNA <- driftwithoutNA[2:200]
var(driftwithoutNA)
mean(driftwithoutNA)

hist(driftm$residuals)


library(nortest)
ad.test(meanm$residuals)
ad.test(naivem$residuals)
ad.test(driftm$residuals)

#All 3 models' residuals are normally distributed.

acf(meanm$residuals)
acf(na.omit(driftm$residuals))
acf(na.omit(rwm$residuals))
acf(driftwithoutNA)



### Stationarity

set.seed(2020)
x <- rnorm(1000) # no unit-root, stationary

library(tseries)

adf.test(x) # augmented Dickey Fuller Test



plot(nottem) # Let s see the nottem dataset

plot(decompose(nottem))

adf.test(nottem)



y <- diffinv(x) # non-stationary

plot(y)

adf.test(y)





### Autocorrelation

# Durbin Watson test for autocorrelation
# check the required traits for the test
length(lynx); head(lynx); head(lynx[-1]); head(lynx[-114]) 

library(lmtest)

dwtest(lynx[-114] ~ lynx[-1])


set.seed(2222)
x = rnorm(700) # Lets take a look at random numbers

dwtest(x[-700] ~ x[-1]) # No autocorrelation



length(nottem) # and the nottem dataset

dwtest(nottem[-240] ~ nottem[-1])



### ACF and PACF

acf(lynx, lag.max = 20); pacf(lynx, lag.max =20, plot = F)

# lag.max for numbers of lags to be calculated

# plot = F to suppress plotting
set.seed(1212)
acf(rnorm(500), lag.max = 20)

tsdisplay(germaninfl)

## Exercise messy data

set.seed(54)
myts <- ts(c(rnorm(50, 34, 10), 
             rnorm(67, 7, 1), 
             runif(23, 3, 14)))

plot(myts)

# THere are clear change in the series' characteristics marked by each
# subsequent end of the random variables. The variance is clearly non-constant.
# The mean is also non-constant. The series is not stationary. There seems to
# be no trend or seasonal component

library(tseries)
adf.test(myts)

# As expected, the series is not stationary.

adf.test(myts[1:50]) #Stationary
adf.test(myts[51:117]) # Stationary
adf.test(myts[118:140]) # Non-stationary

# The random uniform variable result is not trustworthy since the number of
# samples is too small
set.seed(1001)
adf.test(runif(1000))

# See? If the number of sample is larger, the test is of course more accurate
# We can see that a non-stationary series can have a subseries that is
# stationary.

# Checking for autocorrelation
acf(myts)
pacf(myts)

# The series exhibit autocorrelation. We need to model the series with ARIMA

# Clear autocorrelation based on the ACF plot
for(i in 1:10){
  print(Box.test(myts, lag = i, type = "Ljung-Box")$p.value)
}

# The p-values are all 0, there are autocorrelation for every lag from 1 to 10

# Creating the first differenced series
myts_1 <- myts[2:140] - myts[1:139]

# Creating time series plot for the first differenced series
plot(myts_1, type = "l")

# Checking for stationarity
adf.test(myts_1) # The first differenced series is stationary.

acf(myts_1)
pacf(myts_1)

# Based on the ACF plot, we might need at least MA(1)
# Based on the PACF plot, we might need AR(3)

aic <- matrix(nrow = 4, ncol = 4)
for(i in 0:3){
  for (j in 0:3){
    aic[i+1, j+1] <- arima(myts_1, order = c(i,0,j))$aic
  }
}

aic; which.min(aic)

# Since the minimum is AIC is in index 15, then the ARMA model with the minimum
# AIC is the ARMA(2,3)
arima(myts_1, order = c(2,0,3))$aic

# Another way to find the minimum AIC
aic <- matrix(nrow = 4, ncol = 4)
for(i in 0:3){
  for (j in 0:3){
    aic[i+1, j+1] <- arima(myts, order = c(i,1,j))$aic
  }
}

aic; which.min(aic)

# The result is the same. The best ARMA model is the ARMA(2,3)

arima_myts <- arima(myts, order = c(2,1,3))
arima_myts$aic

library(forecast)
forecast_myts <- forecast(arima_myts, h = 5)

forecast_myts$mean
forecast_myts$x
forecast_myts$fitted

# Considering that the last part of the original series is a uniform random
# variable with the parameters 3 and 14 (thus mean = 8.5), our forecast is
# slightly off the optimal forecast, that is, 8.5 for every observation.

plot(forecast_myts$x, ylab = "myts")
lines(forecast_myts$fitted, col = "red")
lines(forecast_myts$mean, col = "blue")

#Plotting the confidence intervals
lines(c(rep(NA, 140), forecast_myts$upper[6:10]), col = "grey")
lines(c(rep(NA, 140), forecast_myts$lower[6:10]), col = "grey")

# The 95% CI is too wide to make any meaningful use.

# Creating the mean model, naive model, and the RW with drift model
mean_myts <- meanf(myts)
naive_myts <- naive(myts)
rwd_myts <- rwf(myts, drift = T)

# Comparing ARIMA with the mean model, naive model, and the RW with drift
# in terms of RMSE
mean(mean_myts$residuals**2)
mean(naive_myts$residuals**2, na.rm = T)
mean(rwd_myts$residuals**2, na.rm = T)
mean(arima_myts$residuals**2)

# The difference is conclusive. The ARIMA model is undoubtedly the superior of
# all 4 models.

# Plotting the residuals
plot(mean_myts$residuals, type = "l")
plot(naive_myts$residuals, type = "l")
plot(rwd_myts$residuals, type = "l")
plot(arima_myts$residuals, type = "l")

# The residuals does not have equal variance.

# Comparing the models based on the residuals' distribution
mean(mean_myts$residuals)
mean(naive_myts$residuals, na.rm = T)
mean(rwd_myts$residuals, na.rm = T)
mean(arima_myts$residuals)

# All models' residuals have mean near 0. However the mean model and the RW with
# drift model is definitely superior in this metric, their mean of residuals is
# virtually 0

var(mean_myts$residuals)
var(naive_myts$residuals, na.rm = T)
var(rwd_myts$residuals, na.rm = T)
var(arima_myts$residuals)

# The ARIMA model has the lowest residuals variance of all models. This is good.
# The lower the variance of the residuals, the more our predictions closely
# resembles the real values

library(nortest)
ad.test(mean_myts$residuals)
ad.test(naive_myts$residuals)
ad.test(rwd_myts$residuals)
ad.test(arima_myts$residuals)

# Every single p-value is virtually zero. We reject the null hypothesis that
# the residuals are normally distributed. None of the models have a normally
# distributed residuals.

library(moments)
skewness(mean_myts$residuals)
skewness(naive_myts$residuals, na.rm = T)
skewness(rwd_myts$residuals, na.rm = T)
skewness(arima_myts$residuals)

kurtosis(mean_myts$residuals)
kurtosis(naive_myts$residuals, na.rm = T)
kurtosis(rwd_myts$residuals, na.rm = T)
kurtosis(arima_myts$residuals)

# The kurtosis are not normal-like (normal kurtosis = 3)

# Checking the residuals for heteroscedasticity
library(MTS)
archTest(mean_myts$residuals)
archTest(na.omit(naive_myts$residuals))
archTest(na.omit(rwd_myts$residuals))
archTest(arima_myts$residuals)

# All the p-values are virtually zero. We reject the null hypothesis that the
# variance is constant. The residuals are heteroscedastic.

# We will need to use GARCH model to account for the heteroscedasticity.
library(rugarch)
spec_myts <- ugarchspec(variance.model = list(model ="sGARCH",garchOrder = c(1,1)),
                        mean.model = list(armaOrder = c(2,3)),
                        fixed.pars = list(mu=mean(myts_1)))
garch_myts <- ugarchfit(spec_myts, data = myts_1)

str(garch_myts)
infocriteria(garch_myts)
infocriteria(garch_myts)[1]

# We are going to create a loop and find the smallest AIC
aic <- matrix(nrow=3, ncol = 3)
for(i in 1:3){
  for(j in 1:3){
    if(i != 0 | j != 0){
      s
      aic[i, j] <- infocriteria(ugarchfit(spec_myts, data = myts_1))[1]
    }
  }
}
aic; which.min(aic)

# The minimum AIC is in index 3, thus the GARCH model with the minimum AIC is
# the GARCH(3,1) model. Therefore, the ARIMA-GARCH model that we will use is
# ARIMA-GARCH(2,1,3)-(3,1)
spec_myts <- ugarchspec(variance.model = list(model ="sGARCH",
                                             garchOrder = c(3,1)),
                       mean.model = list(armaOrder = c(2,3)),
                       fixed.pars = list(mu=mean(myts_1)))
arima_garch_myts <- c()

# I have not yet known how to create an ARIMA-GARCH model in R yet

# We are asked to perform the whole thing to the log transformed data
log_myts <- log(myts)

# Creating the time series plot
plot(log_myts)

# The log transformed series does not look stationary. The mean is changing.
adf.test(log_myts)

# The ADF test result is non-stationary. As expected.

# Creating the first differenced series
log_myts_1 <- log_myts[2:140] - log_myts[1:139]

# Creating the time series plot
plot(log_myts_1, type = "l")

#Checking the first differenced series for stationarity
adf.test(log_myts_1)

# The first differenced series of log_myts is stationary
mean_log_myts <- meanf(log_myts_1)
naive_log_myts <- naive(log_myts_1)
rwd_log_myts <- rwf(log_myts_1, drift = T)

mean(mean_log_myts$residuals**2)
mean(naive_log_myts$residuals**2, na.rm = T)
mean(rwd_log_myts$residuals**2, na.rm = T)

var(mean_log_myts$residuals**2)
var(naive_log_myts$residuals**2, na.rm = T)
var(rwd_log_myts$residuals**2, na.rm = T)

ad.test(mean_log_myts$residuals)
ad.test(naive_log_myts$residuals)
ad.test(rwd_log_myts$residuals)

# All the residuals are still not normally distributed

archTest(mean_log_myts$residuals)
archTest(na.omit(naive_log_myts$residuals))
archTest(na.omit(rwd_log_myts$residuals))



# The residuals of the mean model is homoscedastic, while the residuals for the
# other 2 models are still heteroscedastic.

library(forecast)
meanm <- meanf(myts, h=10)
naivem <- naive(myts, h=10)
driftm <- rwf(myts, h=10, drift = T)

plot(meanm, main = "", bty = "l")
lines(naivem$mean, col=123, lwd = 2)
lines(driftm$mean, col=22, lwd = 2)
legend("bottomleft",lty=1,col=c(4,123,22), bty = "n", cex = 0.75,
       legend=c("Mean method","Naive method","Drift Method"))

length(myts)
mytstrain <- window(myts, start = 1, end = 112 )
mytstest <- window(myts, start = 113)

meanma <- meanf(mytstrain, h=28)
naivema <- naive(mytstrain, h=28)
driftma <- rwf(mytstrain, h=28, drift = T)

accuracy(meanma, mytstest)
accuracy(naivema, mytstest)
accuracy(driftma, mytstest)

plot(naivem$residuals)

mean(naivem$residuals[2:140])

hist(naivem$residuals) # normal distribution

shapiro.test(naivem$residuals) # test for normal distribution, normal distr can be rejected

acf(naivem$residuals[2:140]) # autocorrelation test, autocorrelation present

