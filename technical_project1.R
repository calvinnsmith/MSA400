library("tidyverse")
library("xts")
library("PerformanceAnalytics")
library("fitdistrplus")
library("ismev")

### Full dataset of daily stock prices from 2001-07-10 - 2021-04-22
data <- read.csv("SAS_data.csv",header = TRUE, sep = ",")

#### INITIAL DATA HANDLING #####

# removing missing values
new_data <- data[-which(data$Close == "null"),]


# creating new dataset with Date and Close

SAS_closing <- new_data %>% select(Date,Close)
SAS_closing$Close <- as.numeric(SAS_closing$Close)
dates <- as.Date(SAS_closing$Date)
SAS_time_series <- xts(SAS_closing[,-1],order.by = dates)
names(SAS_time_series) <- "Close"

## including 2009 financial crisis
sas2006 <- SAS_time_series["2006/2012"]

# including covid pandemic
sas2020<- SAS_time_series["2016/2021"]


##### plotting historicql stock prices

plot(SAS_time_series[,"Close"], main = "SAS" )


#par(mfrow=c(2,1))

plot(sas2006[,"Close"], main = "SAS Closing Prices")

plot(sas2020[,"Close"], main = "SAS Closing Prices")



###### returns ####################
###################################
sas2006 <- data.frame(date = index(sas2006), coredata(sas2006))
sas2006$Close <- as.numeric(sas2006$Close)

write.csv(sas2006,"sas2006.csv")

sas2020 <- data.frame(date = index(sas2020), coredata(sas2020))
sas2020$Close <- as.numeric(sas2020$Close)

write.csv(sas2020,"sas2020.csv")

### function to calculate returns ###
Returns <- function(prices){
  r <- numeric(length(prices)-1)
  for (i in 1:(length(r)-1)){
    r[i] <- -100*((prices[i+1]-prices[i])/prices[i])
  }
  return(r)
}



ret2006 <- Returns(sas2006$Close)
write.csv(ret2006,"returns2006.csv")

ret2020 <- Returns(sas2020$Close)
write.csv(ret2020,"returns2020.csv")

# plotting returns

plot(ret2020[-c(1037,1192,1193)],type = "h",main = "SAS daily returns (%) for 2016-2021 ",xlab = "Days",ylab = "Returns (%)")
abline(h = 2,col = "red", lty = 1,lwd = 2)
legend(1,25,c("returns","threshold = 2"), col = c("black","red"),lty = 1:1)



#### PEAKS OVER THRESHOLD ####


### Choosing appropriate mu ####
## 2006 , mu = 4 seem appropriate
u <- seq(0,10,length.out = 100)
mean_excess <- numeric(length(u))
for (i in 1:length(u)){
  exceedances <- ret2006[which(ret2006 > u[i])]
  mean_excess[i] <- mean(exceedances) - u[i]
}
plot(u,mean_excess,"l",ylab = "Mean Excess")
abline(v = 4, col = "red")

# 2020, mu = 2 seems appropriate
u <- seq(0,10,length.out = 100)
mean_excess <- numeric(length(u))
for (i in 1:length(u)){
  exceedances <- ret2020[which(ret2020 > u[i])]
  mean_excess[i] <- mean(exceedances) - u[i]
}
plot(u,mean_excess,"l", ylab = "Mean Excess")
abline(v = 2,col = "red")

mu06 <- 4
mu20 <- 2

excess2006 <- ret2006[which(ret2006 > mu06)]
excess2020 <- ret2020[which(ret2020 > mu20)]

#### GP #####

gpfit06 <- gpd.fit(ret2006,mu06)
gdp.diag(gpfit06)

gpfit20 <- gpd.fit(ret2020,mu20)
gpd.diag(gpfit20)

# Value At Risk
gp_quantile <- function(fit,quantiles){
  scale <- fit$mle[1]
  shape <- fit$mle[2]
  rate <- fit$rate
  mu <- fit$threshold
  
  q <- mu + (scale/shape)*((((rate)*(1-quantiles))^(-shape)) -1)
  
  return(q)
}

# Expected Shortfall
exp_short <- function(fit,quantiles,Var){
  scale <- fit$mle[1]
  shape <- fit$mle[2]
  mu <- fit$threshold
  
  expected <- Var  + (scale + shape*(Var - mu))/(1-shape)
  return(expected)
}


### Gaussian 


gaussian06 <- fitdist(ret2006,"norm")

gaussian20 <- fitdist(ret2020,"norm")  


# Value at Risk
function_quantile <- function(quantiles,Data){
  mean <- Data$estimate[1]
  sd <- Data$estimate[2]
  q <- qnorm(quantiles,mean,sd)
  return(q)
}
 


### Monthly BM VaR from PoT ####
library("fExtremes")

maximas06 <- blockMaxima(ret2006,20)

maximas20 <- blockMaxima(ret2020,20)

