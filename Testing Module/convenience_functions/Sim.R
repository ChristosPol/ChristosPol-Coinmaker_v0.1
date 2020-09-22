##inputs:
alpha=-2; sigma=1; T=1; n=2^(12); X0=0.1;
#############Generate 1 trajectory
dt=T/n
t=seq(0,T,by=dt)
x=c(X0,alpha*dt+sigma*sqrt(dt)*rnorm(n,mean=0,sd=1))
Xt=cumsum(x)
plot(t,Xt,type='l',xlab="time")
