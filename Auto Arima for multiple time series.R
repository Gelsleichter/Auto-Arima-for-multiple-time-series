############################################################################################
### R script: Auto Arima for multiple time series
### Version: 0.1, November 2020
### Yuri Andrei Gelsleichter and Rafael Delgado
### License: CC-BY-NC-SA
############################################################################################
#    ___       __           ___       _           
#   / _ |__ __/ /____      / _ | ____(_)_ _  ___ _
#  / __ / // / __/ _ \    / __ |/ __/ /  ' \/ _ `/
# /_/ |_\_,_/\__/\___/   /_/ |_/_/ /_/_/_/_/\_,_/ 
#                                                 
#    __  ___     ____  _      __       _______               ____        _       
#   /  |/  /_ __/ / /_(_)__  / /__    /_  __(_)_ _  ___     / __/__ ____(_)__ ___
#  / /|_/ / // / / __/ / _ \/ / -_)    / / / /  ' \/ -_)   _\ \/ -_) __/ / -_|_-<
# /_/  /_/\_,_/_/\__/_/ .__/_/\__/    /_/ /_/_/_/_/\__/   /___/\__/_/ /_/\__/___/
#                    /_/                                                         
#
# figlet -f smslant Auto-Arima ### run on terminal ctrl+alt+enter (for linux)
# figlet -f smslant Multiple-Time-Series ### (for linux)
# showfigfonts (for linux)
############################################################################################

### Set and check the working directory
### Auto setwd
install.packages("rstudioapi")
library(rstudioapi); current_path <- getActiveDocumentContext()$path; setwd(dirname(current_path)) ### https://eranraviv.com/r-tips-and-tricks-working-directory/
getwd()
gc(); rm(list=ls())

library(MASS)
library(tseries)
library(forecast)

### load the data
rainfall <- read.csv("teste.csv")

### quick check on the data
str(rainfall)
table(is.na(rainfall))
dim(rainfall)
rainfall[1:8, 1:6]

### selecting the columns; in this case, drop the first column as well
rainfall <- (rainfall[, 2:6])

### create empty lists to store the results
output_list <- list()
output_mt <- list()
# output_lnchuva <- list()
# output_fitlnchuva <- list()


### The loop
for(j in c(1:5)){ ### the wanted columns

  ### Convert to ln format
  chuva=(rainfall[, j])
  lnchuva=log(chuva)
  # output_lnchuva[[j]] <- lnchuva

  ### tests (off)
  #ACF, PACF and Dickey-Fuller Test
  # acf(lnchuva, lag.max=20)
  # pacf(lnchuva, lag.max=20)
  # difflnchuva=diff(lnchuva, 1)
  # difflnchuva
  # adf.test(lnchuva)
  # adf.test(difflnchuva)
  
  ### Time series and auto.arima
  chuvaarima <- ts(lnchuva, start = c(2001,1), end=c(2019,12), frequency = 12)

  fitlnchuva <- auto.arima(chuvaarima)
  fitlnchuva
  # output_fitlnchuva[[j]] <- fitlnchuva
  
  ### Plot
  # plot(chuvaarima, type ='l')
  # title('JNJ Chuva')
  # exp(lnchuva)

  ### Forecasted Values From ARIMA
  forecastedvalues_ln=forecast(fitlnchuva, h=168)
  forecastedvalues_ln
  
  ### plot (off)
  # plot(forecastedvalues_ln)

  forecastedvaluesextracted=as.numeric(forecastedvalues_ln$mean)
  finalforecastvalues=exp(forecastedvaluesextracted)
  finalforecastvalues
  
  ### store the results in a "list()" object
  output_list[[j]] <- finalforecastvalues
  
  ### extract from the list to matrix
  output_mt <- (t(as.data.frame(do.call(rbind, output_list))))
}

### check the outputs
output_list
output_mt

### check the class of the outputs
class(output_list)
class(output_mt)

### save in a scv file
write.csv(output_mt, "Many_CanaARIMA.csv")




















############################################################################################
### Auto Arima Single Time Serie
### Auto Arima Single Time Serie
### Auto Arima Single Time Serie
### Auto Arima Single Time Serie
############################################################################################

############################################################################################
#    ___        ___       _                _____           __  __________
#   / _ |      / _ | ____(_)_ _  ___ _    / __(_)__  ___ _/ /_/_  __/ __/
#  / __ |_    / __ |/ __/ /  ' \/ _ `/   /\ \/ / _ \/ _ `/ / -_) / _\ \  
# /_/ |_(_)  /_/ |_/_/ /_/_/_/_/\_,_/   /___/_/_//_/\_, /_/\__/_/ /___/  
#                                                 /___/                 
#figlet -f smslant A. Arima SingleTS ### run on terminal ctrl+alt+enter (for linux)  
############################################################################################

library(MASS)
library(tseries)
library(forecast)

### check and set the working directory
getwd()
# setwd(C:\My_dir)

### load the data
rainfall <- read.csv("teste.csv")

# lnchuva=log(rainfall[1:192])
lnchuva=log(rainfall[, 2:2])
lnchuva

### testes, desligados
#ACF, PACF and Dickey-Fuller Test
# acf(lnchuva, lag.max=20)
# pacf(lnchuva, lag.max=20)
# difflnchuva=diff(lnchuva, 1)
# difflnchuva
# adf.test(lnchuva)
# adf.test(difflnchuva)

#Time series and auto.arima

chuvaarima<-ts(lnchuva, start = c(2001,1), end=c(2019,12), frequency = 12)
fitlnchuva<-auto.arima(chuvaarima)
fitlnchuva

### plot
# plot(chuvaarima, type ='l')
# title('JNJ Chuva')
# exp(lnchuva)

#Forecasted Values From ARIMA
forecastedvalues_ln=forecast(fitlnchuva, h=168)
forecastedvalues_ln
plot(forecastedvalues_ln)

forecastedvaluesextracted=as.numeric(forecastedvalues_ln$mean)
finalforecastvalues=exp(forecastedvaluesextracted)
finalforecastvalues

write.csv(finalforecastvalues, "CanaARIMA.csv")