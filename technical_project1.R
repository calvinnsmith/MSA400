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


lines(R, type="h", on=NA)


plot(sas2006[,"Close"])

plot(sas2020[,"Close"])



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

plot(ret2006,type = "h",main = "SAS daily returns (%) for 2006-2012 ",xlab = "Days",ylab = "return")
abline(h = 0,col = "red", lty = 1,lwd = 2)
abline(h = 2,col = "green",lwd =3)


#### PEAKS OVER THRESHOLD ####


### Choosing appropriate mu ####
## 2006 , mu = 8 seem appropriate
u <- seq(0,10,length.out = 100)
mean_excess <- numeric(length(u))
for (i in 1:length(u)){
  exceedances <- ret2006[which(ret2006 > u[i])]
  mean_excess[i] <- mean(exceedances) - u[i]
}
plot(u,mean_excess,"l")
abline()

# 2020, mu = 3 seems appropriate
u <- seq(0,10,length.out = 100)
mean_excess <- numeric(length(u))
for (i in 1:length(u)){
  exceedances <- ret2020[which(ret2020 > u[i])]
  mean_excess[i] <- mean(exceedances) - u[i]
}
plot(u,mean_excess,"l")


#####

mu06 <- 4
mu20 <- 2

excess2006 <- ret2006[which(ret2006 > mu06)]
excess2020 <- ret2020[which(ret2020 > mu20)]

N06 <- length(excess2006)
N20 <- length(excess2020)

n06 <- length(ret2006)
n20 <- length(ret2020)

#### GP #####

gpfit06 <- gpd.fit(ret2006,mu06)
gdp.diag(gpfit06)

scale06 <- gpfit06$mle[1]
shape06 <- gpfit06$mle[2]


gpfit20 <- gpd.fit(ret2020,mu20)
gpd.diag(gpfit20)

x95 <- mu06 + (scale06/shape06)*((((N06/n06)*0.05)^(-shape06)) -1)

x99 <-  mu06 + (scale06/shape06)*((((N06/n06)*0.01)^(-shape06)) -1)
