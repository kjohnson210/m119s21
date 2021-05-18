p <- function(x,lambda=2){
  # x must be a whole number
  (lambda^x/factorial(x))*exp(-lambda)
}

#prob. of 4 huricanes when lambda is 2
p(4)
p(4,2)
#prob. of 5 huricanes when lambda is 2
p(5)
p(5,2)
#prob. of 2 huricanes when lambda is 2
p(2)
p(2,2)


#prob. of x=4,5,or 2 huricanes when lambda is 2
p(4,2)
p(5,2)
p(2,2)
#prob. of x=4,5,or 2 huricanes when lambda is 5
p(4,5)
p(5,5)
p(2,5)
#prob. of x=4,5,or 2 huricanes when lambda is 1
p(4,1)
p(5,1)
p(2,1)




p.3v1 <- function(x,lambda=2){
  # each element of x must be a whole number
  prod((lambda^x/factorial(x))*exp(-lambda))
}

p.3v2 <- function(x1,x2,x3,lambda=2){
  # x1, x2, and x3 must be whole numbers
  (lambda^(x1+x2+x3)/(factorial(x1)*factorial(x2)*factorial(x3)))*exp(-3*lambda)
}


p.3v1(c(4,4,8))
p.3v2(4,4,8)
p(4)*p(4)*p(8)


p.3v1(c(2,5,3))
p.3v1(c(4,4,8),5)
p.3v1(c(4,4,8),1)
p.3v1(c(4,4,8),10)




L <- function(lambda,x=4){
  # x must be a whole number
  (lambda^x/factorial(x))*exp(-lambda)
}

lambda <- seq(0,20,0.1)
par(mar=c(2.5,2.5,0.25,0.25))
plot(lambda,L(lambda,4),type='l')
abline(v=4,col=2)
abline(v=5,col=2)

lambda <- seq(0,20,0.1)
par(mar=c(2.5,2.5,0.25,0.25))
plot(lambda,L(lambda,2),type='l')


L2 <- function(lambda,x1=4,x2=4,x3=8){
  (lambda^(x1+x2+x3)/(factorial(x1)*factorial(x2)*factorial(x3)))*exp(-3*lambda)
}

lambda <- seq(0,20,0.1)
par(mar=c(2.5,2.5,0.25,0.25))
plot(lambda,L2(lambda),type='l')
abline(v=6,col=2)