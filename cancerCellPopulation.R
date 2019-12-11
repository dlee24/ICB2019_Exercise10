#Exercise 10

#setting parameters for modeling equation
K<-1000000
N0<-99
M0<-1
timesteps <- 1500

#creating empty vectors to store model data in based on the timestep days of simulation
Nt<-numeric(length=timesteps)
Nt[1]<-N0
Mt<-numeric(length=timesteps)
Mt[1]<-M0

#simulation, using for loop through timestep, to fill the empty vectors
#uses conditional t > 350 days, a point after equilibrium was reached (Colin approved)
#thus drug treatment begins at 350 days
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


#store simulation data in data frame
sim<-data.frame(time=1:length(Mt),N=Nt,N2=Mt)


#setting axis values of graph to be integers, not scientific notation
options(scipen=5)

#plot the non-mutant data in line graph
plot(x=sim$time, y=sim$N, type = "l",
     col = "dark green", 
     xlab="Time(days)",
     ylab="N (number of cells)",
     main="Cell Growth Simulation",
     ylim = c(0, 1.1*K))

#add the mutant data with a line graph
lines(x=sim$time, y=sim$N2, type = "l", col = "red")

#add line where drug treatment began
abline(v=350, lty=4, lwd=3)

#add legend for plot
legend(780, 850000, legend=c("Non-mutant", "Mutant","Day Treatment Begins"),
       col=c("dark green", "red", "black"),
       lty=c(1,1,4),
       lwd=c(1,1,3),
       cex=0.6)
