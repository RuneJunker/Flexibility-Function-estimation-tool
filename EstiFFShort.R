library(glmnet)

Dat <- data.frame(Demand = read.csv("q_vs_price.csv")[,2])
Dat$Prices <- read.csv("q_vs_price.csv")[,3]



n <- 18 # Amount of time steps
Input <- matrix(0,nrow=length(Dat$Demand),ncol=n)
for(k in 1:n){
  Input[,k] <- c(numeric(k-1),Dat$Prices[1:(length(Dat$Prices)-k+1)]-mean(Dat$Prices))
}

fit3 <- glmnet(Input,Dat$Demand-mean(Dat$Demand),alpha=0) ## Alpha can be adjusted between 0 and 1, to alternate between ridge and lasso
Use <- dim(fit3$beta)[2]

## Plot step response
x <- (1:dim(fit3$beta)[1])/1
plot(cumsum(fit3$beta[,Use]) ~ x,type='s',ylab='W/$',xlab="Time (Hour)")
lines(c(0,0) ~ c(-1,2000),lty=2)

## Plot original data with model predictions on top
plot(Dat$Demand,type='l')
lines(predict(fit3,newx=Input)[,Use]+mean(Dat$Demand),col='red')

## R2
1-mean((Dat$Demand-(predict(fit3,newx=Input)[,Use]+mean(Dat$Demand)))^2)/mean(Dat$Demand^2)
