library("tidyverse")
library("xts")
library("PerformanceAnalytics")
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

sas2020 <- data.frame(date = index(sas2020), coredata(sas2020))


### function to calculate returns ###
Returns <- function(prices){
  r <- numeric(length(prices)-1)
  for (i in 1:(length(r)-1)){
    r[i] <- -100*((prices[i+1]-prices[i])/prices[i])
  }
  return(r)
}



ret2006 <- Returns(sas2006$Close)

ret2020 <- Returns(sas2020$Close)


# plotting returns

plot(ret2006,type = "p",main = "SAS daily returns (%) for 2006-2012 ",xlab = "Days",ylab = "return",pch = 20)
abline(h = 0,col = "red", lty = 1,lwd = 2)
