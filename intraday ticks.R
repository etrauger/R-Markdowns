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

## Find members of Treasury Actives Curve as of given date ##
member_names <- function(inputdate) {
  cv_date <- paste(format(inputdate,"%Y"), format(inputdate,"%m"), format(inputdate,"%d"), sep="")
  # create override
  ovrd <- c("CURVE_DATE"=cv_date)
  # accessing members
  tes <- bds(security="YCGT0025 Index", field="CURVE_MEMBERS", overrides=ovrd)
  # return all members of curve
  return(tes)
}

## Finds number of member in US Actives List
member_number <- function(inp) {
  val <- 0
  
  if (inp == "1M") {
    val <- 1
  } else if (inp == "3M") {
    val <- 2
  } else if (inp == "6M") {
    val <- 3
  } else if (inp == "1Y") {
    val <- 4
  } else if (inp == "2Y") {
    val <- 5
  } else if (inp == "3Y") {
    val <- 6
  } else if (inp == "5Y") {
    val <- 7
  } else if (inp == "7Y") {
    val <- 8
  } else if (inp == "10Y") {
    val <- 9
  } else {
    val <- 10
  }
  return(val)
}

## Get tick values for specified day for specified security on Treasury Actives Curve ##
get_ticks_date <- function(inputdate, member) {
  ## Find tick for member ##
  cv_date <- paste(format(inputdate,"%Y"), format(inputdate,"%m"), format(inputdate,"%d"), sep="")
  # start time
  st <- paste(cv_date, "00:00:01")
  st.lub <- ymd_hms(st, tz="America/New_York")
  # end time
  ct <- paste(cv_date, "23:59:59")
  ct.lub <- ymd_hms(ct, tz="America/New_York")
  
  ## Find member name from input
  mem.ind <- member_number(member)
  tes <- member_names(inputdate)
  mem.name <- tes[mem.ind,1]
  
  ## Get and return daily ticks ##
  tick.test <- getTicks(security=mem.name, eventType=c("TRADE"), startTime=st.lub, endTime=ct.lub)
  tick.test[,1] <- with_tz(tick.test[,1], tzone="America/New_York")
  tick.output <- data.frame(tick.test[,1], tick.test[,3])
  names(tick.output) <- c("Time", "Price")
  
  return(tick.output)
}


## Compile intra-day tick data for initially-speficied time period ##
tick_data_timeframe <- function(stdate, eddate, member) {
  # create list of all dates to scan over and remove weekends
  alldates <- seq(stdate, eddate, by="days")
  datekeep <- !(weekdays(alldates) %in% c('Saturday','Sunday'))
  alldates <- as.data.frame(alldates)
  alldates <- alldates[datekeep,1]
  all_tick_data <- data.frame()
  
  for (i in 1:length(alldates)) {
    tod <- as.Date(alldates[i])
    dat <- get_ticks_date(tod, member)
    all_tick_data <- rbind(all_tick_data, dat)
  }
  return(all_tick_data)
}




#### SIMULATION ####

memb.num <- member_number("10Y")

# Pull 10Y price during specified timeframe
abc <- tick_data_timeframe(date.start, date.end, "10Y")
# Plot the data pulled
plot(abc, type="l")
# Convert data to XTS object
def <- as.xts(abc[,-1], order.by=abc[,1])
# Plot XTS object with pulled data
plot(def)

## Standardize data (do not use) ##
date.def <- seq(from=start(def),
                to=end(def),
                by="sec")
reg.def <- xts( , date.def)
reg.def <- merge(def, reg.def)
reg.def <- cbind(orig=reg.def, locf=na.locf(reg.def))
head(reg.def)
reg.def <- reg.def[,2]
head(reg.def)



## Find average 1-minunte movements


