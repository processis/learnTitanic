# 24.1 Nicholson’s blowflies

blowfly <- read.table("/media/user/娱乐/learnTitanic/the R book-sec/blowfly.txt",header=T)
attach(blowfly)
names(blowfly)

flies <- ts(flies)
plot(flies)

length(flies)

par(mfrow=c(2,2))
sapply(1:4, function(x) plot(flies[-c(361: (361-x+1))], flies[-c(1:x)] ) )

sapply(7:10, function(x) plot(flies[-c((361-x+1):361)], flies[-c(1:x)] ) )
par(mfrow=c(1,1))


acf(flies,main="",col="red")

acf(flies,type="p",main="",col="red")

second <- flies[201:361]

summary(lm(second~I(1:length(second))))

detrended <- second- predict(lm(second~I(1:length(second))))
par(mfrow=c(2,2))
ts.plot(detrended)

acf(detrended,main="")

acf(detrended,type="p",main="")
par(mfrow=c(1,1))

# 24.2 Moving average

ma3 <- function (x) {
  y <- numeric(length(x)-2)
  for (i in 2:(length(x)-1)) {
    y[i] <- (x[i-1]+x[i]+x[i+1])/3
  }
  y }

temperature <- read.table("/media/user/娱乐/learnTitanic/the R book-sec/temp.txt",header=T)
attach(temperature)
tm <- ma3(temps)
plot(temps)
lines(tm[2:158],col="blue")

# 24.3 Seasonal data

weather <- read.table("/media/user/娱乐/learnTitanic/the R book-sec/SilwoodWeather.txt",header=T)
attach(weather)
names(weather)

plot(upper,type="l")

length(upper)

index <- 1:6940
6940/19

time <- index/365.2632

model <- lm(upper~sin(time*2*pi)+cos(time*2*pi))

plot(time, upper, pch=".")
lines(time, predict(model),col="red",lwd=2)


summary(model)

plot(model$resid,pch=".")

windows(7,4)
par(mfrow=c(1,2))
acf(model$resid,main="")
acf(model$resid,type="p",main="")

# 24.3.1 Pattern in the monthly means

temp <- ts(as.vector(tapply(upper,list(month,yr),mean)))
windows(7,7)
acf(temp,main="")

ytemp <- ts(as.vector(tapply(upper,yr,mean)))
acf(ytemp,main="")

# 24.4 Built-in time series functions

high <- ts(upper,start=c(1993,1),frequency=365)

plot(high)

# 24.5 Decompositions

up <- stl(high,"periodic")

plot(up)

#24.6 Testingforatrendinthetimeseries

ys <-factor(1+(yr>2002))
tapply(upper,ys,mean)

model1 <- lm(upper~index+sin(time*2*pi)+cos(time*2*pi))
summary(model1)

model2 <
  lmer(upper~index+sin(time*2*pi)+cos(time*2*pi)+(1 | factor(yr)),REML=FALSE)
model3 <
  lmer(upper~sin(time*2*pi)+cos(time*2*pi)+(1 | factor(yr)),REML=FALSE)
anova(model2,model3)

means <- as.vector(tapply(upper,yr,mean))
model <- lm(means~I(1:19))
summary(model)

model <- lm(means[-1]~I(1:18))
summary(model)

# 24.7 Spectral analysis

numbers <- read.table("/media/user/娱乐/learnTitanic/the R book-sec/lynx.txt",header=T)
attach(numbers)
names(numbers)

plot.ts(Lynx)

spectrum(Lynx,main="",col="red")


# 24.8 Multiple time series

twoseries <- read.table("/media/user/娱乐/learnTitanic/the R book-sec/twoseries.txt",header=T)

attach(twoseries)
names(twoseries)

plot.ts(cbind(x,y),main="")

par(mfrow=c(2,2))
acf(cbind(x,y),type="p",col="red")

#24.9 Simulated time series

Y <- rnorm(250,0,2)
windows(7,4)
par(mfrow=c(1,2))
plot.ts(Y)
acf(Y,main="")

Z <- rnorm(250,0,2)

Y <- numeric(250)
Y[1] <- Z[1]
for (i in 2:250) Y[i] <--0.5*Y[i-1]+Z[i]
plot.ts(Y)
acf(Y,main="")

Z <- rnorm(250,0,2)
Y[1] <- Z[1]
for (i in 2:250) Y[i] <- 0.5*Y[i-1]+Z[i]
plot.ts(Y)
acf(Y, main="")

Z <- rnorm(250,0,2)
Y[1] <- Z[1]
for (i in 2:250) Y[i] <- Y[i-1]+Z[i]
plot.ts(Y)
acf(Y, main="")

# 24.10 Time series models

Lynx<- read.table("/media/user/娱乐/learnTitanic/the R book-sec/Lynx.txt",header=T)


windows(7,4)
par(mfrow=c(1,2))
acf(Lynx,main="")
acf(Lynx,type="p",main="")

model10 <- arima(Lynx,order=c(1,0,0))
model20 <- arima(Lynx,order=c(2,0,0))
model30 <- arima(Lynx,order=c(3,0,0))
model40 <- arima(Lynx,order=c(4,0,0))
model50 <- arima(Lynx,order=c(5,0,0))
model60 <- arima(Lynx,order=c(6,0,0))
AIC(model10,model20,model30,model40,model50,model60)

model01 <- arima(Lynx,order=c(0,0,1))
model02 <- arima(Lynx,order=c(0,0,2))
model03 <- arima(Lynx,order=c(0,0,3))
model04 <- arima(Lynx,order=c(0,0,4))
model05 <- arima(Lynx,order=c(0,0,5))
model06 <- arima(Lynx,order=c(0,0,6))
AIC(model01,model02,model03,model04,model05,model06)

model40 <- arima(Lynx,order=c(4,0,0))
model41 <- arima(Lynx,order=c(4,0,1))
model42 <- arima(Lynx,order=c(4,0,2))
model43 <- arima(Lynx,order=c(4,0,3))
AIC(model40,model41,model42,model43)

model400 <- arima(Lynx,order=c(4,0,0))
model401 <- arima(Lynx,order=c(4,1,0))
model402 <- arima(Lynx,order=c(4,2,0))
model403 <- arima(Lynx,order=c(4,3,0))
AIC(model400,model401,model402,model403)