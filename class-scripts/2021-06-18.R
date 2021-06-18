rm(list=ls())
data <- read.csv(url("https://byuistats.github.io/M119/data2_ls.csv"))
x <- data$x
y <- data$y

plot(x,y,pch=16)
abline(h=0,lty=3,col='gray')
abline(v=0,lty=3,col='gray')

#A function that finds the solutions for a system of equations of the form:
#b.1 - c.11x - c.12y = 0
#b.2 - c.21x - c.22y = 0
#where b.1, b.2, c.11, c.12, c.21, and c.22 are constants
#with c.12 = c.21.
cPts <- function(c.11,c.12,c.22,b.1,b.2){
  best.y <- (c.11*b.2 - c.12*b.1)/(c.11*c.22 - c.12^2)
  best.x <- (b.1 - c.12*best.y)/c.11
  
  return(c(best.x,best.y))
}


a <- sum(x) #c.12 and c.21
c <- sum(y) #b.1
c.11 <- 60 #c.11
d <- sum(x*y) #b.2
f <- sum(x^2) #c.22

parm <- cPts(c.11,a,f,c,d)
parm

120*2*sum(x^2)-(2*sum(x))^2

x.fit <- seq(-5,5,0.1)
y.fit <- parm[1]+parm[2]*x.fit

lines(x.fit,y.fit,col=5)


