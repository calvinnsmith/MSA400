
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


### fitting gev distribution to data 
T = 20 
C <- 1/T
#2006
gev06 <- gevFit(return06$x,block = T, type = "mle")

xi_06 <-as.numeric(gev06@fit$par.ests[1])
mu_06 <- as.numeric(gev06@fit$par.ests[2])
beta_06 <-as.numeric(gev06@fit$par.ests[3])

mu_new_06 <- mu_06 - ((1-C^(xi_06))*beta_06)/xi_06
beta_new_06 <- C^(xi_06)*beta_06

dailyVaR06 <- qgev(quantiles, xi = xi_06, mu = mu_new_06, beta = beta_new_06, lower.tail = TRUE)

#2020
gev20 <- gevFit(return20$x,block = T, type = "mle")

xi_20 <-as.numeric(gev20@fit$par.ests[1])
mu_20 <- as.numeric(gev20@fit$par.ests[2])
beta_20 <-as.numeric(gev20@fit$par.ests[3])

mu_new_20 <- mu_20 - ((1-C^(xi_20))*beta_20)/xi_20
beta_new_20 <- C^(xi_20)*beta_20

dailyVaR20 <- qgev(quantiles, xi = xi_20, mu = mu_new_20, beta = beta_new_20, lower.tail = TRUE)




#### barplots

## 2006
color.names <- c("blue","green","red","black")
barplot(rbind(emp06_quantiles,norm_quant06,gpd_quantiles06$VaR,dailyVaR06),beside = T,ylim = c(0,45),col = color.names)
legend("top",c("Empirical","Gaussian","GPD","GEV"), fill = color.names)

## 2020 
color.names <- c("blue","green","red","black")
barplot(rbind(emp20_quantiles,norm_quant20,gpd_quantiles20$VaR,dailyVaR20),beside = T,ylim = c(0,100),col = color.names)
legend("top",c("Empirical","Gaussian","GPD","GPD"), fill = color.names)



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



backtesting_gev <- function(training,testing,p){
  
  T <- 20
  C <- 1/T
  
  violations <- numeric(2)
  violations[2] <- length(testing)*(1-p)
  
  total <- c(training,testing)
  
  GEV <- gevFit(total[1:length(training)], block = T)
  
  xi <-as.numeric(GEV@fit$par.ests[1])
  mu <- as.numeric(GEV@fit$par.ests[2])
  beta <-as.numeric(GEV@fit$par.ests[3])
  
  mu_new <- mu - ((1-C^(xi))*beta)/xi
  beta_new <- C^(xi)*beta
  
  VAR <- qgev(p, xi = xi, mu = mu_new, beta = beta_new, lower.tail = TRUE)

  if (total[length(training)+1] > VAR){
    violations[1] <- violations[1] + 1
  }
  
  for (i in 2:length(testing)){
    GEV <- gevFit(total[i:(length(training)+(i-1))],block = T)
    
    xi <-as.numeric(GEV@fit$par.ests[1])
    mu <- as.numeric(GEV@fit$par.ests[2])
    beta <-as.numeric(GEV@fit$par.ests[3])
    
    mu_new <- mu - ((1-C^(xi))*beta)/xi
    beta_new <- C^(xi)*beta
    
    VAR <- qgev(p, xi = xi, mu = mu_new, beta = beta_new, lower.tail = TRUE)
    
    value <- total[length(training)+i]
    
    if(value > VAR){
      violations[1] <- violations[1] + 1
    }
    
  }
  
  return(violations)  
}














