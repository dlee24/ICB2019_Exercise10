#Exercise 10
#setting parameters and creating vector for values for drug absent model
K<-1000000
N0<-99
M0<-1
timesteps <- 1500

Nt<-numeric(length=timesteps)
Nt[1]<-N0
Mt<-numeric(length=timesteps)
Mt[1]<-M0

#simulation
for (t in 1:(timesteps-1)){
  if(t < 350){
    rn=0.1
    rm=0.1
    Mt[t+1] <- Mt[t]+rm*Mt[t]*(1-((Mt[t]+Nt[t])/K))
    Nt[t+1] <- Nt[t]+rn*Nt[t]*(1-((Mt[t]+Nt[t])/K))
  }else if(t >= 350){
    rn2=-0.1
    rm2=0.05
    Mt[t+1] <- Mt[t]+rm2*Mt[t]*(1-((Mt[t]+Nt[t])/K))
    Nt[t+1] <- Nt[t]+rn2*Nt[t]*(1-((Mt[t]+Nt[t])/K))
  }
}


#plot simulation
sim<-data.frame(time=1:length(Mt),N=Nt,N2=Mt)


options(scipen=5)
plot(x=sim$time, y=sim$N, type = "l",
     col = "dark green", 
     xlab="Time(days)",
     ylab="N (number of cells)",
     main="Cell Growth Simulation",
     ylim = c(0, 1.1*K))
lines(x=sim$time, y=sim$N2, type = "l", col = "red")
abline(v=350, lty=4, lwd=3)
legend(750, 850000, legend=c("Non-mutant", "Mutant","Day Treatment Begins"),
       col=c("dark green", "red", "black"),
       lty=c(1,1,4),
       lwd=c(1,1,3),
       cex=0.6)
