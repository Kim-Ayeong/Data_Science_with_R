#1. Getting started
tui <- read.csv("tui.csv", header=T, dec=",", sep=";")

plot(tui[,5],type="l")

plot(tui[,5], type="l",
 lwd=2, col="red", xlab="time", ylab="closing values",
 main="TUI AG", ylim=c(0,60) )

plot(diff(log(tui[,5])),type="l")

hist(diff(tui[,4]),prob=T,ylim=c(0,0.6),xlim=c(-5,5),col="red")
lines(density(diff(tui[,4])),lwd=2)

mu<-mean(diff(tui[,4]))
sigma<-sd(diff(tui[,4]))

x<-seq(-4,4,length=100)
y<-dnorm(x,mu,sigma)
lines(x,y,lwd=2,col="blue")

qqnorm(diff(tui[,4]))
abline(0,1)

x<-diff(log(tui[,5]))
ks.test(x,"pnorm",mean(x),sd(x))

shapiro.test(x)

#2. Simple Component Analysis
#library(ts)
#install.packages("timeSeries")
library(timeSeries)

plot(tui[,5],type="l")
 tui.1 <- filter(tui[,5],filter=rep(1/5,5))
 tui.2 <- filter(tui[,5],filter=rep(1/25,25))
 tui.3 <- filter(tui[,5],filter=rep(1/81,81))
lines(tui.1,col="red")
lines(tui.2,col="purple")
lines(tui.3,col="blue")

beer<-read.csv("beer.csv",header=T,dec=",",sep=";")
beer<-ts(beer[,1],start=1956,freq=12)
plot(stl(log(beer),s.window="periodic"))

lbeer<-log(beer)
t<-seq(1956,1995.2,length=length(beer))
t2<-t^2
plot(lbeer)
lm(lbeer~t+t2)
lines(lm(lbeer~t+t2)$fit,col=2,lwd=2)

lbeer<-log(beer)
t<-seq(1956,1995.2,length=length(beer))
t2<-t^2
sin.t<-sin(2*pi*t)
cos.t<-cos(2*pi*t)
plot(lbeer)
lines(lm(lbeer~t+t2+sin.t+cos.t)$fit,col=4)

summary(lm(lbeer~t+t2+sin.t+cos.t))

#3. Exponential Smoothing
beer<-read.csv("beer.csv",header=T,dec=",",sep=";")
beer<-ts(beer[,1],start=1956,freq=12)

HoltWinters(beer)

plot(beer)
lines(HoltWinters(beer)$fitted,col="red")

beer.hw<-HoltWinters(beer)

predict(beer.hw,n.ahead=12)

plot(beer,xlim=c(1956,1999))
lines(predict(beer.hw,n.ahead=48),col=2)

#4. ARIMA Models
sim.ar<-arima.sim(list(ar=c(0.4,0.4)),n=1000)
sim.ma<-arima.sim(list(ma=c(0.6,-0.4)),n=1000)
par(mfrow=c(2,2))
acf(sim.ar,main="ACF of AR(2) process")
acf(sim.ma,main="ACF of MA(2) process")
pacf(sim.ar,main="PACF of AR(2) process")
pacf(sim.ma,main="PACF of MA(2) process")

arima(data,order=c(p,d,q))

data(LakeHuron)
fit<-arima(LakeHuron,order=c(1,0,1))

fit<-arima(LakeHuron,order=c(1,0,1))
tsdiag(fit)

Box.test(fit$residuals,lag=1)

fit<-arima(LakeHuron,order=c(1,0,1))

LH.pred<-predict(fit,n.ahead=8)

plot(LakeHuron,xlim=c(1875,1980),ylim=c(575,584))
LH.pred<-predict(fit,n.ahead=8)
lines(LH.pred$pred,col="red")
lines(LH.pred$pred+2*LH.pred$se,col="red",lty=3)
lines(LH.pred$pred-2*LH.pred$se,col="red",lty=3)

