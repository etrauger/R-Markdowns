#### FORMATTING R ENVIRONMENT ####
cat("\014")
rm(list=ls())
dev.off()


#### LIBRARY IMPORT ####

# manipulate strings
library(stringr)
# manipulate dates and times
library(lubridate)
# data visualization
library(ggplot2)
library(ggfortify)
# quality control
library(qcc)
# random forest methods for machine learning
library(randomForest)
# manipulate data sets
library(dplyr)
# basic statistics
library(stats)
# elastic net regression with CV
library(glmnet)
# regression model training tools
library(caret)
# save time series objects
library(zoo)
library(timeDate)
library(bizdays)
# manipulate time series
library(xts)
# technical financial analysis
library(quantmod)
# forecast data trends
library(forecast)
# read Excel documents
library(readxl)
# audio notifications within run
library(beepr)
# forecasting time series data
library(prophet)
# used for correlation visualization
library(GGally)
library(corrplot)
library(psych)
library(nFactors)
library(GPArotation)
library(kernlab)
library(usdm)
# read in data
library(readr)
# manipulate data tables easily
library(data.table)
# PCA Analysis
library(FactoMineR)
library(factoextra)
# allows foreach command
library(foreach)
# manipulate r lists
library(rlist)
# machine learning algorithms
#library(h2o)
# modeling financial assets
library(fAssets)
# R interaction with Bloomberg
library(Rblpapi)
# connect to Bloomberg
blpConnect()
# animate chart
library(gganimate)
library(animation)
library(magick)
library(mosaic)









#### INITIALIZATION ####
## Set dates of interest ##
date.start <- as.Date("2018-06-10")
date.end <- Sys.Date()-1


#### FUNCTIONS ####





#### SIMULATION ####
# necessary packages
library(YieldCurve)
library(XML)

## Pull in Bloomberg data for analysis (updated) ##
US1Y  <- bdh("USGG12M Index", c("PR005"), start.date=date.start, end.date=date.end)
US2Y  <- bdh("USGG2YR Index", c("PR005"), start.date=date.start, end.date=date.end)
US3Y  <- bdh("USGG3YR Index", c("PR005"), start.date=date.start, end.date=date.end)
US5Y  <- bdh("USGG5YR Index", c("PR005"), start.date=date.start, end.date=date.end)
US7Y  <- bdh("USGG7YR Index", c("PR005"), start.date=date.start, end.date=date.end)
US10Y <- bdh("USGG10YR Index", c("PR005"), start.date=date.start, end.date=date.end)
US30Y <- bdh("USGG30YR Index", c("PR005"), start.date=date.start, end.date=date.end)

## Add data t0 data table

hist.YC <- data.frame(US2Y[,1], 
                      US1Y[,2],
                      US2Y[,2], 
                      US3Y[,2], 
                      US5Y[,2], 
                      US7Y[,2],
                      US10Y[,2],
                      US30Y[,2])

yr.values <- c(1, 2, 3, 5, 7, 10, 30)
plot(yr.values, hist.YC[1,2:length(hist.YC)],
     xlim=c(0,30), 
     ylim=c(0,5), 
     type="o",
     xlab="Maturity (years)",
     ylab="Yield (%)")
nloop <- nrow(hist.YC)

colfunc <- colorRampPalette(c("black", "white"))

saveVideo({
  for(i in 1:nloop) {
    name <- hist.YC[i,1]
    plot(yr.values, hist.YC[i,2:length(hist.YC)],
         xlim=c(0,30), 
         ylim=c(0,5), 
         type="o",
         main=name,
         xlab="Maturity (years)",
         ylab="Yield (%)")
    lines(yr.values, hist.YC[1,2:length(hist.YC)],
          col="red")
  }
},
interval=.05,
movie.name="yieldOutput.mp4", 
ani.width=400,
ani.height=400)

