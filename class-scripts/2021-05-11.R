library(data4led)
bulb <- led_bulb(1,seed = 2021)

t <- bulb$hours
y1 <- bulb$percent_intensity

par(mfrow=c(1,1),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y1,xlab="Hour", ylab="Intensity(%) ", pch=16)





f0 <- function(x,a0=1){a0 + 0*x}
x <- seq(0,100000,5)
y2 <- f0(x,100)

par(mfrow=c(1,2),mar=c(2.5,2.5,1,0.25))
plot(t,y1,xlab="Hour", ylab="Intensity(%) ", pch=16)
lines(x,y2,col=2)
plot(t,y1,xlab="Hour", ylab="Intensity(%) ", pch=16, xlim = c(0,80000),ylim = c(-10,120))
lines(x,y2,col=2)




f2 <- function(x,a0=0,a1=0,a2=1){
  a0 + a1*x + a2*x^2
}

x <- seq(-10,800001,2)
yM <- f2(x,a0=100,a1=0.00043,a2=-0.00000005)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f2')
lines(x,yM,col=2)
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,yM,col=2)




f5 <- function(x,a0=100,a1=0,a2=1){
  (a0 + a1*x)*exp(-a2*x)
}

x <- seq(-10,800001,2)
yM <- f5(x,a0=100,a1=0.00487,a2=0.0000425)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f5')
lines(x,yM,col=2)
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,yM,col=2)






