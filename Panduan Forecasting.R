# Untuk ilustrasi, akan digunakan data lynx yang ada di semua aplikasi R.

# Langkah 1: Lihat datanya
lynx
plot(lynx)

# Kelihatannya tidak ada faktor trend dan ada faktor seasonality (musiman) tapi
# periodenya lama banget (Karena ada 1 gunung tinggi diikuti dengan 3 gunung
# pendek). Tebakan sih tidak stasioner.

# Langkah 2: Lihat karakteristik dari data tersebut, yaitu:
# 1. Stationer atau tidak
# 2. Ada autokorelasi atau tidak
# 3. Ragam homogen (homoscedastic) atau nggak (heteroscedastic)

# Uji stationer dengan Augmented Dickey Fuller
install.packages("tseries") 
library(tseries)
adf.test(lynx)

# Karena p-valuenya kurang dari 0.05 maka tolak H0, deret waktunya stationer,
# Karena stasioner, tidak perlu ada differencing.

# Uji autokorelasi dengan uji Ljung-Box
Box.test(lynx, lag = 1, type = "Ljung-Box")
Box.test(lynx, lag = 2, type = "Ljung-Box")
Box.test(lynx, lag = 3, type = "Ljung-Box")
Box.test(lynx, lag = 4, type = "Ljung-Box")
Box.test(lynx, lag = 5, type = "Ljung-Box")
Box.test(lynx, lag = 6, type = "Ljung-Box")

# Karena p-valuenya kurang dari 0.05, berarti ada autokorelasi. Bisa dicek untuk
# berbagai tingkat lag

# Bikin plot ACF dan PACF
acf(lynx)
pacf(lynx)

# Ada banyak yang diluar garis biru putus2, berarti signifikan autokorelasinya

# Karena ada autokorelasi maka pake ARMA, kalau tidak ada autokorelasi tidak
# perlu pakai ARMA

# Uji heteroskedastisitas dengan ARCH-LM
library(MTS)
archTest(lynx)

# Karena p-valuenya kurang daripada 0.05 maka deret waktunya heteroscedastic.
# Variansnya berubah-ubah.

# Langkah 3: bikin model yang sesuai
# Karena stasioner dan ada autokorelasi maka pake ARMA. Coba bikin ARMA dengan
# berbagai parameter, lihat AICnya, yang terendah adalah yang terbaik.

arima(lynx, order = c(1,0,1))

# Model ARMA(1,1) AICnya 1891.07. Bandingkan sama model2 yang lain, pakai loop
# saja supaya tidak makan waktu. Kita bandingin semua model ARMA yang
# parameternya <= 3

aic <- matrix(nrow = 4, ncol = 4)
for(i in 0:3){
  for(j in 0:3){
    aic[i+1,j+1] <- arima(lynx, order = c(i,0,j))$aic
  }
}

# Lihat hasilnya
aic

# Cari yang paling kecil
which.min(aic)

# R itu ngitung elemennya dari atas ke bawah, kiri ke kanan. 15 berarti ada di
# baris ke-3 kolom ke-4. itu ARMA(2,3). Jadi kita pake ARMA(2,3) buat deret ini

arma_lynx <- arima(lynx, order = c(2,0,3))

# Langkah 4: Cek residual dari modelnya
# Model yang bagus residualnya tidak ada autokorelasi, homoscedastic, meannya 0
# Kalau bisa normal lebih bagus. Varians dari residual semakin kecil semakin
# bagus. Residual yang bagus itu peubah acak biasa. Tidak ada polanya.

# Bikin scatter plot dan histogram residual
plot(arma_lynx$residuals)
hist(arma_lynx$residuals)

# Scatter plotnya kelihatan cukup bagus meskipun ada 3 gunung tinggi itu.
# Histogramnya kelihatan cukup mirip sama normal.

# Cari tau apakah residualnya menyebar normal
# Hitung skewness dan kurtosisnya
library(moments)
skewness(arma_lynx$residuals)
kurtosis(arma_lynx$residuals)

# Skewness sama kurtosisnya kelihatan tidak normal. Skewness normal itu 0,
# kurtosis normal itu 3. Kemungkinan besar hal ini disebabkan karena 3 gunung
# tinggi itu mengacaukan polanya

# Uji kenormalan dengan anderson Darling
library(nortest)
ad.test(arma_lynx$residuals)

# Karena p-value kurang dari 0.05 maka residualnya tidak normal, sesuai hasil
# skewness dan kurtosis tadi. Tidak apa2 tidak normal asalkan autokorelasi
# tidak ada dan homoscedastic

# Uji autokorelasi dengan uji Ljung-Box
Box.test(arma_lynx$residuals, lag = 1, type = "Ljung-Box")
Box.test(arma_lynx$residuals, lag = 2, type = "Ljung-Box")
Box.test(arma_lynx$residuals, lag = 3, type = "Ljung-Box")
Box.test(arma_lynx$residuals, lag = 4, type = "Ljung-Box")
Box.test(arma_lynx$residuals, lag = 5, type = "Ljung-Box")
Box.test(arma_lynx$residuals, lag = 6, type = "Ljung-Box")

# Bagus, semua p-valuenya diatas 0.05, tidak ada autokorelasi

# Bikin plot ACF dan PACF
acf(arma_lynx$residuals)

# Kelihatan tidak ada garis yang ada di luar garis biru putus2. selain lag 0.
# autokorelasi di lag 0 pasti 1 untuk deret apapun.

pacf(arma_lynx$residuals)

# PACF juga bagus, sama sekali tidak ada yang diluar garis putus2, memang tidak
# ada autokorelasi

# Uji heteroskedastisitas dengan ARCH-LM
archTest(arma_lynx$residuals)

# Bagus, p-value diatas 0.05, residualnya homoscedastic
# Karena residualnya bagus, maka model kita sudah bagus

# Langkah 5: Bikin forecast
library(forecast)
plot(forecast(arma_lynx, h = 5, level = c(80,95)))

# argumen h itu untuk berapa langkah kedepan forecastnya. Argumen level itu 
# menentukan CI dari forecast kita. Karena ini 80 dan 95 maka ditunjukkin
# CI 80% dan Ci 95%.

# Kalau cuman mau yang CI 95%
plot(forecast(arma_lynx, h = 5, level = c(95)))

#Lihat nilai-nilai forecastnya
forecast(arma_lynx, h = 5)$mean

# Kalau dikasih argumen plot = F, dia nggak bikin plot, dia 

# Opsional, bandingin deret yang asli dengan Fitted Value dari modelnya
plot(lynx)
lines(fitted(arma_lynx), col = "red")

# Model kita cukup baik. Kalau diperhatikan, 3 gunung tinggi itu nggak
# sepenuhnya berhasil dimodelkan, tapi ini sudah ok.

# Tambahan, kasus data tidak stationer
# Akan digunakan data simulasi

set.seed(1001)
x <- cumsum(rnorm(1000))
plot(x, type = "l")

# Uji ADF
adf.test(x)

# Karena p-value > 0.05 maka dia tidak stasioner

# Uji ADF ke deret differencing pertama
adf.test(diff(x))

# Setelah di differencing, ternyata dia stasioner, maka kita cukup mengganti 0
# yang sebelumnya kita pakai dengan 1. Misalnya:

arima(x, order = c(1,1,1))

# Kemudian kita lanjutkan sesuai prosedur yang sama seperti sebelumnya.