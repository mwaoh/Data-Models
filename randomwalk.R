##RANDOM WALK PROCESS
#set random number seed
set.seed(300)

#length of time series
TT <- 1000
 #initialize {x_t} and {w_t}
xx <- ww <- rnorm(n = TT, mean = 0, sd = 1)
#xx compute values 2 thru TT
for (t in 2:TT) {
  xx[t] <- xx[t - 1] + ww[t]
}

#Now let's plot the simulated time series and its ACF.
plot(xx)
#setup plot area
par(mfrow = c(2, 2),oma=c(6,6,6,6))
 #plot line
plot.ts(xx, ylab = expression(italic(x[t])))

#plot line
plot.ts(xx, ylab = expression(italic(x[t])))
#plot ACF
plot(acf(xx,plot = FALSE))
plot(pacf(xx,plot = FALSE))
mtext("Random walk process with acf and pacf",outer=T,cex=1.1)

#AR(1) PROCESS
#Simulation of an AR(1) process with coefficient ?? = 0.9 and 
 #?? = ???0.5. 1000 observations in

#each case. In the second case ??2 = 9.
alpha=.9
xt=arima.sim(1000,model=list(ar=alpha))
par(mfrow=c(3,1),oma=c(6,6,6,6))
ts.plot(ts(xt))
acf(xt,lag=20)
acf(xt,type="partial",lag=20)
mtext("AR(1) process with alpha =.9,
sigma^2=1",outer=T,cex=1.1)


#AR(2) PROCESS

#Now with a variance different to one 
epsilon=rnorm(500,mean=0,sd=sqrt(3))
alpha=-.5
xt=arima.sim(500,model=list(ar=alpha),innov=epsilon)
par(mfrow=c(3,1),oma=c(6,6,6,6))
ts.plot(ts(xt))
acf(xt,lag=50)
acf(xt,type="partial",lag=50)
mtext("AR(1) process with alpha=-.5,
sigma^2=3",outer=T,cex=1.1)


#Lets look at 500 simulated observations of an AR(2)
#process with 
#r = 0.95 (r = 0.75) and ?? = 2??/15.
#Here is
#the R code to obtain this.
r=0.95
w=2*pi/15
phi1=2*r*cos(w)
phi2=-r^2
xt=arima.sim(500,model=list(ar=c(phi1,phi2)))
par(mfrow=c(3,1),oma=c(2,2,2,2))
ts.plot(as.ts(xt))
acf(xt,lag=50)
acf(xt,lag=50,type="partial")

function (N,omega=0.002,alpha=0.36,beta=0.6) 
{
  err<-rnorm(N,0,1)
  Xt<-numeric(N)
  Sigt<-numeric(N)
  Sigt[1]<-sqrt(omega)*err[1]
  Sigt[1]<-sqrt(omega)
  for(i in 1:(N-1)){
    Sigt[i+1]<-sqrt(omega+beta*Xt[i]^{2}+alpha*Sigt[i]^{2})
    Xt[i+1]<-Sigt[i+1]*err[i+1]
  }
  time=c(1:length(Xt))
  return(list(Xt[200:N],Sigt[200:N]) )
}
par(mfrow=c(2,1))
X=GARCH11(N=1500)
plot(t,X[[1]],xlab="Time", ylab="Value",type="l")
plot(t,X[[2]],xlab="Time", ylab="Volatility",type="l")






