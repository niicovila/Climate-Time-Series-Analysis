##
library(astsa)
library(forecast)
library(ggplot2)

##data preparation: Anual Monthly Sequental Data
meantemp_monthly_totals = unname(meantemp_monthly_totals)
monthly = meantemp_monthly_totals[,-1]
monthly = monthly[,-13]
print(monthly[364,][11])
plot.ts(t(monthly[1,]))
an_m = c()

for(i in 1:364){
  an_m = c(an_m, t(monthly[i,]))

}
plot.ts(an_m)
an_m[4368] = an_m[4368-12]
an_m[4367] = an_m[4367-12]

##data preparation: anual
anual = meantemp_monthly_totals[[14]]
anual[364] = anual[363]

##ANUAL analysis
time=c(1:364)
plot.ts(time, anual, type="l",main ="Anual temperatures")
lines(lowess(time,anual, f=.05), lwd=2, col="#003399")
lines(smooth.spline(time, anual, spar= 1), lty=2, lwd=2, col="#A3081B")
legend("bottomright", legend=c("Lowess Smoothing", "Smoothing splines"),col=c("blue", "red"), lty=1:2, cex=0.8)

plot.ts(diff(log(anual)), main="Difference Anual Series")
acf2(diff(anual,lag=1)) ## Cuts off after lag 1
x = diff(anual)
reg_1 = arima(anual, order=c(0,1,1))
reg_2 = arima(anual, order=c(3,1,0))
reg_3 = arima(anual, order=c(3,1,1))
reg_4 = arima(anual, order=c(4,1,2))
reg_5 = arima(anual, order=c(2,1,1))
plot.ts(time, anual, type="l", main="Anual Temperature", xlab="Years", ylab="Temperature")
lines(time, fitted(reg_2), col="red", lwd=2, lty=2)
plot.ts(time, anual, type="l", main="Anual Temperature", xlab="Years", ylab="Temperature")
lines(time, fitted(reg_5), col="red", lwd=2, lty=2)
plot.ts(time, anual, type="l", main="Anual Temperature", xlab="Years", ylab="Temperature")
lines(time, fitted(reg_1), col="red", lwd=2, lty=2)
plot.ts(time, anual, type="l", main="Anual Temperature", xlab="Years", ylab="Temperature")
lines(time, fitted(reg_3), col="red", lwd=2, lty=2)

aicc = function(model){
  n = model$nobs
  p = length(model$coef)
  aicc = model$aic + 2*p*(p+1)/(n-p-1)
  return(aicc)
}

forecast::checkresiduals(reg_1)
summary(reg_1)
reg_1$aic
forecast::checkresiduals(reg_2)
summary(reg_2)
reg_2$aic
forecast::checkresiduals(reg_3)
summary(reg_3)
reg_3$aic
forecast::checkresiduals(reg_5)
summary(reg_5)
reg_5$aic

### Monthly Analysis

plot.ts(an_m)
acf2(an_m)
acf2((diff(an_m, lag=12)), main ="ACF and PACF of monthly differenced data from 1659-2022(lag = 12)")
fit1 = arima(diff(an_m, lag=12), c(0,0,12))
forecast::checkresiduals(fit1)

fit2 = arima(diff(an_m, lag=12), c(0,0,1))
forecast::checkresiduals(fit2)

fit3 = arima(diff(an_m, lag=12), c(1,0,13))
forecast::checkresiduals(fit3)

fit1$aic
fit2$aic
fit3$aic

#WINTER
jan <-meantemp_monthly_totals[[2]]
feb <-meantemp_monthly_totals[[3]]
dec <-meantemp_monthly_totals[[13]]
dec[364]=dec[363]
winter = (jan + feb+dec)/3
acf2(diff(winter))
ema=c(winter[1])
for(i in 2:364){
  ema[i]=(1-0.75)*winter[i]+0.75*ema[i-1]
}
par(mfrow=c(1,1))
plot.ts(time,winter, type="l",xlab="Year", ylab="Temperature", main="Winter Temperature Time Series")
lines(time,ema, col="green")
lines(lowess(time,winter, f=.40), lty=2, col="#003399")
legend("bottomright", legend=c("Lowess Smoothing", "Exponential moving average 75%"),col=c("blue", "green"), lty=2:1, cex=0.8)

#SPRING
mar <-meantemp_monthly_totals[[4]]
apr <-meantemp_monthly_totals[[5]]
may <-meantemp_monthly_totals[[6]]
spring = (mar+apr+may)/3
plot.ts(spring)

ema=c(spring[1])
for(i in 2:364){
  ema[i]=(1-0.75)*spring[i]+0.75*ema[i-1]
}
par(mfrow=c(1,1))
plot.ts(time,spring, type="l",xlab="Year", ylab="Temperature", main="Spring Temperature Time Series", lwd=1.25)
lines(time,ema, col="green", lwd= 2)
lines(lowess(time,spring, f=.40), lty=2, col="#003399", lwd=2)
legend("bottomright", legend=c("Lowess Smoothing", "Exponential moving average 75%"),col=c("blue", "green"), lty=2:1, cex=0.8)

#SUMMER
jun <-meantemp_monthly_totals[[7]]
jul <-meantemp_monthly_totals[[8]]
aug <-meantemp_monthly_totals[[9]]
summer = (jun+jul+aug)/3
plot.ts(summer)
ema=c(summer[1])
for(i in 2:364){
  ema[i]=(1-0.75)*summer[i]+0.75*ema[i-1]
}
par(mfrow=c(1,1))
plot.ts(time,summer, type="l",xlab="Year", ylab="Temperature", main="Summer Temperature Time Series", lwd=1.25)
lines(time,ema, col="green", lwd= 2)
lines(lowess(time,summer, f=.40), lty=2, col="#003399", lwd=2)
legend("bottomright", legend=c("Lowess Smoothing", "Exponential moving average 75%"),col=c("blue", "green"), lty=2:1, cex=0.8)


#FALL
nov <-meantemp_monthly_totals[[12]]
nov[364]=nov[363]
sep <-meantemp_monthly_totals[[10]]
oct <-meantemp_monthly_totals[[11]]
fall = (sep+oct+nov)/3
plot.ts(fall)
ema=c(fall[1])
for(i in 2:364){
  ema[i]=(1-0.75)*fall[i]+0.75*ema[i-1]
}
par(mfrow=c(1,1))
plot.ts(time,fall, type="l",xlab="Year", ylab="Temperature", main="Fall Temperature Time Series", lwd=1.25)
lines(time,ema, col="green", lwd= 2)
lines(lowess(time,fall, f=.40), lty=2, col="#003399", lwd=2)
legend("bottomright", legend=c("Lowess Smoothing", "Exponential moving average 75%"),col=c("blue", "green"), lty=2:1, cex=0.8)

par(mfrow=c(1,1))
acf2(diff(winter), main="First difference Winter Series")
acf2(diff(spring), main="First difference Spring Series")
acf2(diff(summer), main="First difference Summer Series")
acf2(diff(fall), main="First difference Fall Series")

fitwinter = auto.arima(winter, d=1)
par(mfrow=c(1,1))
plot.ts(type="l",time, winter, xlab="Year", main="Fit for the Winter Time Series", ylab="Temperature (ºC)")
lines(time,fitted(fitwinter), col='Red',lty=2)
legend("bottomright", legend=c("Winter Time Series", "Fitted Model"),col=c("black", "red"), lty=1:2, cex=0.8)
fc = (forecast(fitwinter, h = 4))
print(fc)
accuracy(fitwinter)
summary(fitwinter)
forecast::checkresiduals(fitwinter)
print(fitted(fitwinter)[364])

fitspring = arima(spring, c(3,1,0))
par(mfrow=c(1,1))
plot.ts(time, spring, main="Fit for the Spring Time Series", ylab="Temperature (ºC)", type="l")
lines(time, fitted(fitspring), col='Red',lty=2)
legend("bottomright", legend=c("Spring Time Series", "Fitted Model"),col=c("black", "red"), lty=1:2, cex=0.8)
fc = (forecast(fitspring, h = 4))
print(fc)
summary(fitspring)
print(fitted(fitspring)[364])
forecast::checkresiduals(fitspring)

fitsummer = arima(summer, c(2,1,0))
par(mfrow=c(1,1))
plot.ts(time,type="l",summer, main="Fit for the Summer Time Series", ylab="Temperature (ºC)")
lines(time, fitted(fitsummer), col='Red',lty=2)
legend("bottomright", legend=c("Summer Time Series", "Fitted Model"),col=c("black", "red"), lty=1:2, cex=0.8)
fc = (forecast(fitsummer, h = 4))
print(fc)
print(fitted(fitsummer)[364])
forecast::checkresiduals(fitsummer)
summary(fitsummer)

fitfall = arima(fall, c(3,1,0))
par(mfrow=c(1,1))
plot.ts(time, type='l',fall, main="Fit for the Fall Time Series", ylab="Temperature (ºC)")
lines(time, fitted(fitfall), col='Red',lty=2)
legend("bottomright", legend=c("Fall Time Series", "Fitted Model"),col=c("black", "red"), lty=1:2, cex=0.8)
fc = (forecast(fitfall, h = 4))
print(fc)
summary(fitfall)
forecast::checkresiduals(fitfall)
print(fitted(fitfall)[364])

time = c(1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999)
Years = c(time, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014)

plot(Years, winter[327:356], type="l", main="Winter temperatures from 1985-2014", ylab="Temperature")
abline(v=2000, col="red", lty=2)
lines(lowess(Years, winter[327:356]), col="blue")

plot(Years, fall[327:356], type="l", main="Fall temperatures from 1985-2014", ylab="Temperature")
abline(v=2000, col="red", lty=2)
lines(lowess(time2, fall[327:356]), col="blue")

plot(Years, spring[327:356],  type="l", main="Spring temperatures from 1985-2014", ylab="Temperature")
abline(v=2000, col="red", lty=2)
lines(lowess(time2, spring[327:356]), col="blue")

plot(Years, summer[327:356],  type="l", main="Summer temperatures from 1985-2014", ylab="Temperature")
abline(v=2000, col="red", lty=2)
lines(lowess(time2, summer[327:356]), col="blue")

##Spectrum
sp = spec.pgram(an_m, log="yes", main="Periodogram of the monthly temperature series from 1659-2022")

sorted = sort(sp$spec, decreasing = TRUE)[c(1,2,3,4,7)];sorted
#Largest peaks
p1 = sp$freq[sp$spec==sorted[1]];p1
p3 = sp$freq[sp$spec==sorted[5]];p3


