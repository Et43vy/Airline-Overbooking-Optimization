ntickets <- function(N,gamma,p)

?list
1-gamma-pbinom()
1-gamma-pnorm()
which.min() <- use this in combination with indexing

?which.min

youll need to calculate the standard deviation and mean using pnorm

sd = npq
mean = np

?curve()
plot()

for curve, copy the part you use 1-gamma-pnom() but change small n to x to create an expression

for the list, i used list() you could probably create a table too idk

which.min(qbinom(1-0.02,200:210,0.95))

?pbinom
list(nd=nd,nc=nc,N=N,p=p,gamma=gamma)
1-0.02-pbinom
qbinom(1-0.02,200:210,0.95)
ml="CDF of BINOM(60, 1/6)"
curve(pbinom(x, 60, 1/6), -.5, 60.5, lwd=2, ylab="CDF", xlab="k", main=ml, n=10001)

abline(h=0, col="green3") #here

k = 0:60; cdf=pbinom(k, 60, 1/6); points(k, cdf, pch=20, col="blue")
gamma = 0.02
n = 200
p = 0.95

fx<-function(x) (abs(1-gamma-pnorm(x,x*0.95,0.95*0.05*x)))
pnorm(200,200*.95,0.95*0.05*200)
?pnorm()
which.min(qbinom(1-gamma, 200:250,p))
curve(pbinom(x,250,.95))
curve(1-0.02-pbinom(x,200,0.95))
curve(fx, from = 200, to = 250, n=501)
  warnings()
?curve
?optimize
#1-gamma-
(pbinom(200,250,0.95))

fb <- function(x) (1-gamma-pnorm(200,x,0.95))

curve(fb,from=190,to=220)
warnings()
optimise(fb,lower= 200, upper = 220)
warnings()
?plot

ntickets <- function(N = 200, gamma = .02, p = .95) {
  
  nxd <- seq(from=N, to=N*1.1, by=1)
  y <- 1-gamma-pbinom(N,nxd,p)
  plot(nxd, y, type = "b", main = "Objective vs n to find optimal tickets sold discrete", xlab = "n", ylab = "Objective",col="blue")
  nd <- nxd[which.min(abs(y))]
  abline(h=0, col="red")
  abline(v=nd,col="red")
  text(x=N*1.1-((N*1.1-N)/2),y=0.95,paste0("(",nd,")"," gamma=",gamma," N=",N," discrete"))
  
  nxc <- seq(from=N, to=N*1.1,by=0.0001)
  yc <- 1-gamma-pnorm(nxc,nxc*p,sqrt(nxc*p*(1-p)))
  curve(1-gamma-pnorm(N,x*p,sqrt(x*p*(1-p))),from=N, to=N*1.1, xlab="n",ylab="Objective",main="Objective vc n to find optimal tickets sold continuous")
  f <- function(x) (abs(1-gamma-pnorm(N,x*p,sqrt(x*p*(1-p)))))
  optum<-optimise(f,nxc)
  nc <- optum$minimum
  abline(h=0, col="blue")
  abline(v=nc,col="blue")
  
  text(x=N*1.1-((N*1.1-N)/2),y=0.95,paste0("(",nc,")"," gamma=",gamma," N=",N," continous"))
  
  returnList <- list(nd=nd, nc=nc, N=N, p=p, gamma=gamma)
  returnList
}

ntickets()
ntickets(N=400,gamma=0.02,p=0.95)  

