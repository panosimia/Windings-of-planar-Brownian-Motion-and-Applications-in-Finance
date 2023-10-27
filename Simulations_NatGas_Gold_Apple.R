###############################################################
### Function to simulate Asian option using Brownian motion ###
###############################################################
simulateAsianOption <- function(S, K, t, N, numSimulations) {
  dt <- t/N  # Time step
  
  optionPayoffs <- numeric(numSimulations)  # Array to store option payoffs
  
  for (sim in 1:numSimulations) {
    pricePath <- numeric(N+1)  # Array to store price path
    
    pricePath[1] <- S  # Initial price
    
    for (i in 2:N) {
      dW <- rnorm(1,0,dt)  # Increment of Brownian motion
      pricePath[i] <- pricePath[i-1]+ dW
    }
    
    averagePrice <- mean(pricePath)  # Average price over the specified period
    
    optionPayoffs[sim] <- ifelse(averagePrice > K, averagePrice - K, 0)  # Option payoff
  }
  
  optionValue <- mean(optionPayoffs)  # Option value
  return(optionValue)
}



#import data for Nat. Gas
library(readr)
NatGas<- read_csv("Natural Gas Futures Historical Data.csv")
View(NatGas)
head(NatGas)
#prepare date in appropriate format
md<-c()
for(i in 1:length(NatGas$Date)){
  md<-c(md,substr(NatGas$Date[i],1,6))
}
y<-c()
for(i in 1:length(NatGas$Date)){
  y<-c(y,substr(NatGas$Date[i],9,10))
}
days<-paste(md,y)
days<-gsub(" ", "", paste(md,y))
day<-as.Date(days,format="%m/%d/%y")

#new data frame with the days and the prices
day<-day[1:66]
price<-NatGas$Price[1:66]
NatGas1<-data.frame(day,price)

#plot prices
library(ggplot2)
ggplot(NatGas1,aes(x=day,y=price))+
  geom_line()+
  labs(x="Day",y="Price in USD per 1 MMBtu",title="Natural Gas (2019)")


##############################
#### For Natural Gas ########
####    3 months    ########
############################
set.seed(123)
S <-NatGas1$price[1]   # Underlying asset price
K <- c(2.6,2.7,2.8,2.9,3,3.1,3.2,3.3)  # Strike price
t <- 66  # Time to maturity
N <- 100000  # Number of time steps
numSimulations <- 1000  # Number of simulations
#Options for each K
optionValue <-c() 
for(i in 1:length(K)){
  optionValue<-c(optionValue,simulateAsianOption(S, K[i], t, N, numSimulations))
}
print(paste("For K=",K,"Asian option value:", round(optionValue,6)))


#Actual options for each K
averageActPrice<-mean(NatGas1$price)
optionActPayoffs<-c()
for(i in 1:length(K)){
optionActPayoffs<-c(optionActPayoffs,ifelse(averageActPrice > K[i], averageActPrice- K[i], 0))
}
print(paste("For K=",K,"actual Asian option:",optionActPayoffs))


#import data for Gold
Gold<- read_csv("Gold Futures Historical Data.csv")
View(Gold)
head(Gold)
#prepare the date in appropriate format
md<-c()
for(i in 1:length(Gold$Date)){
  md<-c(md,substr(Gold$Date[i],1,6))
}
y<-c()
for(i in 1:length(Gold$Date)){
  y<-c(y,substr(Gold$Date[i],9,10))
}
days<-paste(md,y)
days<-gsub(" ", "", paste(md,y))
day<-as.Date(days,format="%m/%d/%y")
day<-day[1:132]
#new data frame with the days and the prices
price<-Gold$Price[1:132]
Gold1<-data.frame(day,price)
#plot prices
library(ggplot2)
ggplot(Gold1,aes(x=day,y=price))+
  geom_line()+
  labs(x="Day",y="Price in USD per 1 tr.ounce",title="Gold (2022)")


#####################
####   For Gold  ####
####  6 months   ####
####################
set.seed(123)
S <-Gold$Price[1]   # Underlying asset price
K <- c(1650,1675,1700,1725,1750,1775,1800,1825)  # Strike price
t <- 132  # Time to maturity
N <- 100000  # Number of time steps
numSimulations <- 1000  # Number of simulations
#Options for each K
optionValue <-c() 
for(i in 1:length(K)){
  optionValue<-c(optionValue,simulateAsianOption(S, K[i], t, N, numSimulations))
}
print(paste("For K=",K,"Asian option value:", round(optionValue,4)))


#Actual options for each K
averageActPrice<-mean(Gold1$price)
optionActPayoffs<-c()
for(i in 1:length(K)){
  optionActPayoffs<-c(optionActPayoffs,ifelse(averageActPrice > K[i], averageActPrice- K[i], 0))
}
print(paste("For K=",K,"actual Asian option:",optionActPayoffs))


###############################
# import data for APPLE's stocks

Appl<- read_csv("AAPL Historical Data.csv")
View(Appl)
head(Appl)
#prepare date in appropriate format
md<-c()
for(i in 1:length(Appl$Date)){
  md<-c(md,substr(Appl$Date[i],1,6))
}
y<-c()
for(i in 1:length(Appl$Date)){
  y<-c(y,substr(Appl$Date[i],9,10))
}
days<-paste(md,y)
days<-gsub(" ", "", paste(md,y))
day<-as.Date(days,format="%m/%d/%y")

#new data frame with the days and the prices
day<-day[64:106]
price<-Appl$Price[64:106]
Appl1<-data.frame(day,price)

#plot prices
ggplot(Appl1,aes(x=day,y=price))+
  geom_line()+
  labs(x="Day",y="Price in USD per stock",title="Apple's stocks (2021)")


##############################
#### For Apple's stocks ########
####    1 month    ########
############################
set.seed(123)
S <-Appl1$price[1]   # Underlying asset price
K <- c(140,142,144,146,148,150,152)  # Strike price
t <- 43  # Time to maturity
N <- 100000 # Number of time steps
numSimulations <- 1000  # Number of simulations
#Options for each K
optionValue <-c() 
for(i in 1:length(K)){
  optionValue<-c(optionValue,simulateAsianOption(S, K[i], t, N, numSimulations))
}
print(paste("For K=",K,"Asian option value:", round(optionValue,6)))


#Actual options for each K
averageActPrice<-mean(Appl1$price)
optionActPayoffs<-c()
for(i in 1:length(K)){
  optionActPayoffs<-c(optionActPayoffs,ifelse(averageActPrice > K[i], averageActPrice- K[i], 0))
}
print(paste("For K=",K,"actual Asian option:",optionActPayoffs))



n = 1000
t = 100
No.Ex = 5
steps = seq(0,t,length=n+1)
BM = replicate(No.Ex, {
  bm <- c(0, cumsum(rnorm(n,0,sqrt(t/n))))
}) 


matplot(BM, type = "l", lty = 1, ylab="Bt",xlab="time")



ggplot(NatGas1, aes(x=day)) + 
  geom_line(aes(y = price), color = "red") + 
  geom_line(aes(y = At), color="blue", linetype="twodash") 


ggplot(NatGas1,aes(x=day))+
  geom_line(aes(y=price),color="red")+
  geom_hline(yintercept=At,color="blue")+
  labs(x="Day",y="Price in USD")
