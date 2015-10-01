#Example empirical power test for two-way ANOVA

#Two genotypes, M82 and Penn
#Two treatments, P and Control

#create table of mean and SD
mean.sd <- expand.grid(gt=c("M82","Penn"),trt=c("Cont","P"))
mean.sd

mean.sd$mean <- c(10,10,12,14)
mean.sd$sd <- c(1,2,1,2)

mean.sd

#do simulations

n.sim <- 100
n <- c(3,5,10,15,20,25,30,50) #number of plants per gt/treat combo
coefficient <- "gt:trt"
alpha <- 0.05 

results.final <- vector("numeric")

#OKAY this should not be written as a nested for loop but I am in hurry...

for(size in n) {
  
  results.tmp <- vector("numeric")
  
  for (sim in 1:n.sim){
    simulated.observations <- data.frame(
      y=rnorm(size*nrow(mean.sd),mean=mean.sd$mean,sd=mean.sd$sd),
      #note rnorm cycles through the values of mean.sd$mean and mean.sd$sd
      gt=mean.sd$gt, #this gets expanded to match the number of generated y
      trt=mean.sd$trt)  #this gets expanded to match the number of generated y
    
    results.tmp[sim] <- 
      summary(aov(y~gt*trt,data=simulated.observations))[[1]][coefficient,"Pr(>F)"]
  }
  results.final[as.character(size)] <- sum(results.tmp < alpha)/n.sim
}

results.final #proportion of simulations where the p-value was less than alpha
