#A single fair die
x <- seq(1,6)
p <- rep(1/6,6)
draw_pmf(x,p) #run the code at the top of the Rectangles, Rugs, and Riemann Sums reading to define the draw_pmf() function


#A single unfair die
x <- seq(1,6)
p <- c(1/21,2/21,3/21,4/21,5/21,6/21)
draw_pmf(x,p)

#calculate E[X]
sum(x*p)

#calculate F(5)
sum(p[1:5])
sum(1:5)/21
1-p[6]
