#Exercise 10
#setting parameters and creating vector for values for drug absent model
K<-1000000
N0<-99
M0<-1
timesteps <- 1500

totalCells[t]<-Nt[t]+Mt[t]
Nt<-numeric(length=timesteps)
Nt[1]<-N0
Mt<-numeric(length=timesteps)
Mt[1]<-M0
totalCells<-numeric(length=timesteps)
totalCells[1]<-totalCells0

#simulation
for (t in 1:(timesteps-2)){
  Mt[t+1] <- Mt[t]+rm*Mt[t]*(1-((Mt[t]+Nt[t])/K))
  Nt[t+1] <- Nt[t]+rm*Nt[t]*(1-((Mt[t]+Nt[t])/K))
  totalCells[t]<-Mt[t]+Nt[t]
  if(totalCells[t]<K){
    rn=0.1
    rm=0.1
    totalCells[t]<-Mt[t]+Nt[t]
  }
  else if(totalCells[t]>=K){
    rn=-0.1
    rm=0.05
    totalCells[t]<-Mt[t]+Nt[t]
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
legend(170, 850000, legend=c("Non-mutant", "Mutant"),col=c("dark green", "red"), lty=1:1, cex=0.65)
