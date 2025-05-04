N<-400
p=0.95
nxc <- seq(from=N, to=N*1.1,by=0.001)
yc <- 1-gamma-pnorm(nxc,nxc*p,sqrt(nxc*p*(1-p)))
curve(1-gamma-pnorm(N,x*p,sqrt(x*p*(1-p))),from=N, to=N*1.1, xlab="n",ylab="Objective",main="Objective vc n to find optimal tickets sold continuous")
nc <- nxc[which.min(abs(yc))]
which.min(abs(yc))
abline(h=0, col="blue")
abline(v=nc,col="blue")

abs(yc)
nc
nxc
yc
?plot
titled<- paste0("Objective vs n to find optimal tickets sold discrete\n", "(",nc,")"," gamma=",gamma," N=",N," continous")
titled <- writeLines(c("Objective vs n to find optimal tickets sold discrete",paste0("(",nc,")"," gamma=",gamma," N=",N," continous")))

nxd <- seq(from=N, to=N*1.1, by=1)
y <- 1-gamma-pbinom(N,nxd,p)
plot(nxd, y, type = "b", main = "Objective vs n to find optimal tickets sold discrete",sub = paste0("(",nc,")"," gamma=",gamma," N=",N," continous"), xlab = "n", ylab = "Objective",col="blue")
nd <- nxd[which.min(abs(y))]
abline(h=0, col="red")
abline(v=nd,col="red")

points(x, cex = .5, col = "dark red")

?optimise
f <- function(x) (abs(1-gamma-pnorm(N,x*p,sqrt(x*p*(1-p)))))
?plot

which.min(optimise(f,nxc)
)
which()
index<-which.min(abs(yc))
index
yc
nxc

?qnorm

adjust 
qbinom(1-gamma,x,p)
uniroot
qnorm(1-gamma, np, sqrt(np(1-p)))

```{r}
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
  curve(1-gamma-pnorm(N+0.5,x*p,sqrt(x*p*(1-p))),from=N, to=N*1.1, xlab="n",ylab="Objective",main="Objective vc n to find optimal tickets sold continuous")
  f <- function(x) abs((1-gamma-pnorm(N+0.5,x*p,sqrt(x*p*(1-p)))))
  optum<-optimise(f,interval=nxc)
  nc <- optum$minimum
  abline(h=0, col="blue")
  abline(v=nc,col="blue")
  
  text(x=N*1.1-((N*1.1-N)/2),y=0.95,paste0("(",nc,")"," gamma=",gamma," N=",N," continous"))
  
  returnList <- list(nd=nd, nc=nc, N=N, p=p, gamma=gamma)
  returnList
}
