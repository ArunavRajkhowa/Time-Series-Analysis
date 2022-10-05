library(forecast)
library(fpp)

setwd("D:\\IITK Data Analytics\\R\\Time-Series-Analysis\\")

#number of observations are low because of time period
#Hence we should avoid complex algorithms coz overfitting occurs
flights=read.csv("international-airline-passengers.csv")

flights=flights$International.airline.passengers..monthly.totals.in.thousands..Jan.49...Dec.60
flights_ts=ts(flights,frequency=12,start=c(1949,1))
plot(flights_ts)


#checking for trend,seasonality and stationarity
plot(decompose(flights_ts))
flightforecast=HoltWinters(flights_ts)



#Arima model
auto.arima(flights_ts)
arimafit=arima(flights_ts,order=c(2,1,1),seasonal = c(0,1,0))

arimafuture=forecast:::forecast.Arima(arimafit,h=24,level=95)
plot(arimafuture)
