gcrts = ts(Data$gdp_to_currency_ratio,start = c(1951,1),frequency = 1)
gcrts
plot.ts(gcrts)
library("TTR", lib.loc="~/R/win-library/3.5")
#Decomposition
gcrtssma3=SMA(gcrts,n=3)
plot.ts(gcrtssma3)
gcrtssma4=SMA(gcrts,n=4)
plot.ts(gcrtssma4)
gcrtssma5=SMA(gcrts,n=5)
plot.ts(gcrtssma5)
library("tseries", lib.loc="~/R/win-library/3.5")
#Stationarity
plot.ts(gcrts)
adf.test((gcrts),alternative="stationary",k=0)
pp.test(gcrts)
kpss.test(gcrts)
gcrtsdiff=diff(gcrts,differences = 1)
plot(gcrtsdiff)
adf.test((gcrtsdiff),alternative="stationary",k=0)
pp.test(gcrtsdiff)
kpss.test(gcrtsdiff)
library("forecast", lib.loc="~/R/win-library/3.5")
library("urca", lib.loc="~/R/win-library/3.5")
#Model selection
acf(gcrtsdiff,lag.max = 70)
acf(gcrtsdiff,lag.max = 70,plot = FALSE)
pacf(gcrtsdiff,lag.max = 70)
pacf(gcrtsdiff,lag.max = 70,plot = FALSE)
auto.arima(gcrts)
auto.arima(gcrtsdiff)
#Predictive model
gcrtsarima=arima(gcrts,order = c(0,1,0))
gcrtsarima
gcrtsfore=forecast(gcrtsarima,h=5)
gcrtsfore
plot(gcrtsfore)
#Checking the model
acf(gcrtsfore$residuals,lag.max = 50)
Box.test(gcrtsfore$residuals,lag=50,type="Ljung-Box")
plot(gcrtsfore$residuals)
hist(gcrtsfore$residuals)
mean(gcrtsfore$residuals)
