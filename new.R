
return06 <- read.csv("returns2006.csv",header = TRUE, sep =",")

return20 <- read.csv("returns2020.csv",header = TRUE, sep =",")

library("fExtremes")

quantiles <- c(0.95,0.96,0.97,0.98,0.99,0.999,0.9999)

# Empirical quantiles

emp06_vector <- sort(return06$x)
emp20_vector <- sort(return20$x)

emp06_quantiles <- quantile(emp06_vector,quantiles)
emp20_quantiles <- quantile(emp20_vector,quantiles)

# fitting gp distribution to data
gpd06 <- gpdFit(return06$x, u = 4)

gpd20 <- gpdFit(return20$x, u = 2)

# calculating VaR and ES

gpd_quantiles06 <- tailRisk(gpd06,prob = quantiles) 

gpd_quantiles20 <- tailRisk(gpd20,prob = quantiles)

# Fitting gaussian distribution to data

library("fitdistrplus")

gaussian06 <- fitdist(return06$x,"norm")

gaussian20 <- fitdist(return20$x,"norm")  

norm_quant06 <- qnorm(quantiles,mean = gaussian06$estimate[1], sd = gaussian06$estimate[2])

norm_quant20 <-  qnorm(quantiles,mean = gaussian20$estimate[1], sd = gaussian20$estimate[2])



#### barplots

## 2006
color.names <- c("blue","green","red")
barplot(rbind(emp06_quantiles,norm_quant06,gpd_quantiles06$VaR),beside = T,ylim = c(0,35),col = color.names)
legend("top",c("Empirical","Gaussian","GPD"), fill = color.names)

## 2020 
color.names <- c("blue","green","red")
barplot(rbind(emp20_quantiles,norm_quant20,gpd_quantiles20$VaR),beside = T,ylim = c(0,100),col = color.names)
legend("top",c("Empirical","Gaussian","GPD"), fill = color.names)



### PoT to block maxima ###
#2006
T = 20
lambda06 <- 158/1755
scale06 <- as.numeric(gpd06@fit$par.ests[1])
shape06 <- as.numeric(gpd06@fit$par.ests[2])

block06 <- blockMaxima(return06$x,T,doplot = TRUE)
block06 <- as.vector(block06)

mu06 <- as.numeric(((lambda06*T)^(scale06) - 1)*(shape06/scale06))
shape_06new <- as.numeric(shape06*(lambda06*T)^(scale06))

monthlyVaR06 <- qgev(quantiles, xi = scale06, mu = mu06, beta = shape_06new, lower.tail = TRUE)

#2020
lambda20 <- 245/1330
scale20 <- as.numeric(gpd20@fit$par.ests[1])
shape20 <- as.numeric(gpd20@fit$par.ests[2])

block20 <- blockMaxima(return20$x,T,doplot = TRUE)
block20 <- as.vector(block20)

mu20 <- as.numeric(((lambda20*T)^(scale20) - 1)*(shape20/scale20))
shape_20new <- as.numeric(shape20*(lambda20*T)^(scale20))

monthlyVaR20 <- qgev(quantiles, xi = scale20, mu = mu20, beta = shape_20new, lower.tail = TRUE)



#### Backtesting #####

data <- read.csv("SAS_data.csv",header = TRUE, sep = ",")

data <- data[-which(data$Close == "null"),]
data$Date <- as.Date(data$Date)

training_06 <- with(data, data[(Date >= "2001-07-10" & Date <= "2005-12-31"),])

training_20 <- with(data, data[(Date >= "2006-01-01" & Date <= "2015-12-31"),])


training_returns_06 <- Returns(as.numeric(training_06$Close))

training_returns_20 <- Returns(as.numeric(training_20$Close))

## Expected violations 95% VaR
expected95_06 <- length(return06$x)*0.05
expected95_20 <- length(return20$x)*0.05

var95_06 <- gpd_quantiles06$VaR[1]

var95_20 <- gpd_quantiles20$Var[1]


backtesting_gpd <- function(training,testing,p){
  
  violations <- numeric(2)
  violations[2] <- length(testing)*(1-p)
  
  total <- c(training,testing)
  
  GP <- gpdFit(total[1:length(training)])
  
  GP_quantile <- tailRisk(GP,p)$VaR
  
  if (total[length(training)+1] > GP_quantile){
    violations[1] <- violations[1] + 1
  }
  
  for (i in 2:length(testing)){
    GP <- gpdFit(total[i:(length(training)+(i-1))])
    GP_quantile <- tailRisk(GP,p)$VaR
    
    value <- total[length(training)+i]
    
    if(value > GP_quantile){
      violations[1] <- violations[1] + 1
    }
    
  }
 
  
  return(violations)  
}
