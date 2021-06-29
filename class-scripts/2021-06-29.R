library(data4soils)
Ng <- cfbp_fpjuliet$ng
hist(Ng,probability = TRUE)

f2 <- function(L,h=0,a=1,b=5){
  # Make sure a > 0 and b > 0.
  
  out <- rep(-1,length(L))
  out[(L < h)] <- 0*L[(L < h)]
  out[(L >= h)] <- b^a/gamma(a)*(L[(L >= h)]-h)^(a-1)*exp(-b*(L[(L >= h)]-h))
  
  return(out)
}

a <- 1.6
b <- 0.75
h1 <- 0
L1 <- seq(h1,20,0.1)
y1 <- f2(L1,h1,a,b)

par(mfrow=c(1,2),mar=c(2,2,2,0.25),oma=c(0.5,0.5,1,0.25))
hist(Ng,probability = TRUE)
lines(L1,y1,type='l',main='f2')



f3 <- function(x,lambda=1){
  # Make sure lambda > 0.
  
  out <- rep(-1,length(x))
  out[(x >= 0)] <- lambda*exp(-lambda*x[(x >= 0)])
  
  return(out)
}

lambda <- 0.38
L2 <- seq(0,20,0.1)
y2 <- f3(L2,lambda)

hist(Ng,probability = TRUE)
lines(L2,y2,type='l',main='f3')


set.seed(2021)
tmp2 <- rgamma(25000, shape = a, rate = b)
length(which(tmp2 > 10))

num2 <- length(which(tmp2 > 10))
total2 <- length(tmp2)
prob2 <- num2/total2
prob2



set.seed(2021)
tmp3 <- rexp(25000, rate = lambda)
length(which(tmp3 > 10))

num3 <- length(which(tmp3 > 10))
total3 <- length(tmp3)
prob3 <- num3/total3
prob3


