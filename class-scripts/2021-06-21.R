library(knitr)

#Shades a rug diagram for a probability mass function. 
#Inputs: 
#  x - a vector of data points
#  p - a corresponding vector of probabilities or frequencies
#All widths are 1 unit wide. 
draw_pmf <- function(x,p){
  xs <- c(rbind(x-1/2,x-1/2,x+1/2,x+1/2))
  px <- c(rbind(0,p,p,0))
  par(mar=c(2.5,2.5,0.25,0.25))
  plot.new()
  plot(xs,px,type="l")
  polygon(xs,px,col="gray")
}

#Shades a rug diagram (shades area under) for a function f from a to b. 
#Inputs: 
#  f - a function f(x)
#  a - left end of the rug
#  b - right end of the rug
#  num_points - how many point are sent into f for plotting. 
draw_rug <- function(f,a,b,num_points=100){
  x <- c(a,seq(a,b,(b-a)/num_points),b,a)
  y <- c(0,f(seq(a,b,(b-a)/num_points)),0,0)
  par(mar=c(2.5,2.5,0.25,0.25))
  plot(x,y,type = "l")
  polygon(x,y,col="gray")
}

#Draws rectangles over the top of a given function.
#The midpoint of top of each rectangle passes through the function. 
#  f - a function f(x)
#  a - left end of graph
#  b - right end of graph
#  num_rectangles - how many rectangles to plot.
#  method - One of "left", "right", or "mid".  Defaults to mid.
draw_rect_approx <- function(f,a,b,num_rectangles, method = "mid"){
  n <- num_rectangles
  dx <- (b-a)/n
  x <- c(a,seq(a,b,dx/100),b,a)
  y <- c(0,f(seq(a,b,dx/100)),0,0)
  par(mar=c(2.5,2.5,0.25,0.25))
  plot(x,y,type = "l")
  
  if(method == "left"){
    xi <- seq(a+0*dx/2,b-dx/2,dx)
    lines(xi,f(xi),type = "h")
    lines(xi,f(xi),type = "s")
    lines(c(xi[n],xi[n]+dx),f(c(xi[n],xi[n])),type = "l")
    lines(c(xi[n],xi[n]+dx),f(c(xi[n],xi[n])),type = "h")
  }
  else if(method == "right"){
    xi <- seq(a+dx,b+dx/2,dx)
    lines(xi-dx,f(xi),type = "h")
    lines(xi-dx,f(xi),type = "s")
    lines(c(xi[n]-dx,xi[n]),f(c(xi[n],xi[n])),type = "l")
    lines(c(xi[n]-dx,xi[n]),f(c(xi[n],xi[n])),type = "h")
  } 
  else{#Use midpoint
    xi <- seq(a+dx/2,b,dx)
    lines(xi-dx/2,f(xi),type = "h")
    lines(xi-dx/2,f(xi),type = "s")
    lines(c(xi[n]-dx/2,xi[n]+dx/2),f(c(xi[n],xi[n])),type = "l")
    lines(c(xi[n]-dx/2,xi[n]+dx/2),f(c(xi[n],xi[n])),type = "h")
  }
}


#Exercise 1.5 (What is the expected value of X?)
x <- seq(2,12)
p <- c(1/36,2/36,3/36,4/36,5/36,6/36,5/36,4/36,3/36,2/36,1/36)
plot(x,p,pch=16)
sum(p)
draw_pmf(x,p)
sum(x*p)


#Hurricane Example
lambda <- 5.857143
x <- seq(0,20)
p <- lambda^x *exp(-lambda)/factorial(x)
plot(x,p,pch=16)
draw_pmf(x,p)
E.20 <- sum(x*p) #expected value when consider only x=0, 1, ..., 20.
ck.20 <- sum(p) #notice ck.20 is not 1. What happened? We did not use all the outcomes...

n <- 30
x <- seq(0,n)
p <- lambda^x *exp(-lambda)/factorial(x)
E.30 <- sum(x*p) #expected value when consider only x=0, 1, ..., 20.
ck.30 <- sum(p) 
#notice ck.30 is 1. 
#But we still did not use all the values... 
#What happened? 
#This is an artifact of machine precision... 
#(maybe using n=30 captures enough of the behavior of this distribution for many calculations)


lambda <- 5.857143
n <- 50
x <- seq(0,n)
p <- lambda^x *exp(-lambda)/factorial(x)
E.50 <- sum(x*p)
ck.50 <- sum(p)


lambda <- 5.857143
n <- 10
x <- seq(0,n)
p <- lambda^x *exp(-lambda)/factorial(x)
p(7) #This didn't work because we have not defined a function p.

prob <- p
p <- function(x,lambda){lambda^x *exp(-lambda)/factorial(x)}
p(7,lambda)

sum(prob[1:8])
cumsum(p(c(0,7)))[8]

data.frame(outcome = x, prob = prob, cdf = cumsum(prob))



