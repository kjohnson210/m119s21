rm(list=ls())

L2 <- function(lambda,x1=4,x2=4){(lambda^(x1+x2)/(factorial(x1)*factorial(x2)))*exp(-2*lambda)}
lambda <- seq(0,20,0.1)
par(mar=c(2.5,2.5,0.25,0.25))
plot(lambda,L2(lambda),type='l')
L2(2)
L2(2,4,4)
points(2,L2(2),pch=16,col=4)

L2(5,7,13)
#notice this is not on the curve because when you change the parameters you change the model
points(5,L2(5,7,13),pch=16,col=3) 


#Solve 100−7t+0.0003ln(0.005t+1)=0
f <- function(t,a,b,c){
  a - b*t + c*log(0.005*t + 1)
}
x <- seq(-200,200,0.5) #The domain is t > -200
plot(x,f(x,a=100,b=7,c=0.003),type='l')
uniroot(f,c(-100,100),a=100,b=7,c=0.0003)$root
uniroot(f,c(-100,100),a=100,b=7,c=0.003)$root #incorrect there is missing zero in the parameter c

p <- function(x,lambda=1){
  # x must be a whole number
  (lambda^x/factorial(x))*exp(-lambda)
}

f2 <- function(x,lambda=1){
  # x must be positive
  lambda*exp(-lambda*x)
}

f3 <- function(x,mu=0,s=1){
  (1/sqrt(2*pi*s^2))*exp(-(x-mu)^2/(2*s^2))
}

LP <- function(lambda,x){
  # The element of x must be a whole numbers.
  prod(p(x,lambda))
}

LE <- function(lambda,x){
  # The elements of x must be positive.
  prod(f2(x,lambda))
}

# For simplicity assume sigma is 1.
LN <- function(mu,x){
  prod(f3(x,mu,sigma=1))
}


###Parameter Values###
p1 <- seq(0,10,0.001)
p2 <- seq(-10,10,0.001)



###Data Values###
# Florida Hurricane Data (2000-2020)
FL <- c(4,4,8,8,6,8,2,8,7,4,8,6,4,3,3,4,5,7,4,7,13)

# Some Exponential Data
dE <- c(0.45729967, 0.47156107, 1.21461705, 0.20539769, 1.78975399, 0.09095850, 0.64675475, 1.60109333, 1.57752679, 0.01238945)

# Some Normal Data
dN <- c(-3.77117676, -2.91429587, -2.02774901, -0.23984575, -1.41960740, -3.17490528, -3.21755276, -0.06442566, -1.92134953, -0.93160739)


y.LP <- as.numeric(lapply(p1,FUN=LP,x=FL))
y.LE <- as.numeric(lapply(p1,FUN=LE,x=dE))
y.LN <- as.vector(lapply(p2,FUN=LN,x=dN))

par(mar=c(2.5,2.5,3,0.25))
plot(p1,y.LP,type='l',main='Poisson Likelihood')
mean(FL)
abline(v=mean(FL),col='gray')

par(mar=c(2.5,2.5,3,0.25))
plot(p1,y.LE,type='l',main='Exponential Likelihood')
mean(dE)
abline(v=mean(dE),col=4)

par(mar=c(2.5,2.5,3,0.25))
plot(p2,y.LN,type='l',main='Normal Likelihood')





