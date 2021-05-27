g <- function(x){sqrt((x + 7)/(2*x^5 - 3*x + 4))}

dg <- function(x){(1/2)*((x + 7)/(2*x^5 - 3*x + 4))^(-1/2)*((2*x^5 - 3*x + 4 - (x + 7)*(10*x^4 - 3))/(2*x^5 - 3*x + 4)^2)}

l <- function(x){sqrt(7)/2 + (25/(16*sqrt(7)))*x}


x <- seq(-1.5,5,by=0.01)
par(mar=c(2.5,2.5,0.25,0.25))
plot(x,g(x),type='l')
abline(h=0,col='gray',lty=3)
abline(v=0,col='gray',lty=3)
lines(x,l(x),col=3)