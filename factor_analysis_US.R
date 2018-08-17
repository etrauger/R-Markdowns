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
## Start and end date ##
date.end <- Sys.Date()
d <- as.POSIXlt(date.end)
d$year <- d$year-5
date.start <- as.Date(d)
rm(d)


## Input names of securities desired ##
securities <- c(# "USGG2YR Index",
                # "USGG3YR Index",
                # "USGG5YR Index",
                # "USGG7YR Index",
                # "USGG10YR Index",
                # "USGG30YR Index",
                # 
                # "USYC2Y3Y Index",
                # "USYC2Y5Y Index",
                # "USYC2Y7Y Index",
                # "USYC2Y10Y Index",
                # "USYC2Y30Y Index",
                # "USYC3Y5Y Index",
                # "USYC3Y7Y Index",
                # "USYC3Y10Y Index",
                # "USYC3Y30Y Index",
                # "USYC5Y7Y Index",
                # "USYC5Y10Y Index",
                # "USYC5Y30Y Index",
                # "USYC7Y10Y Index",
                # "USYC7Y30Y Index",
                # "USYC10Y30Y Index",
                # 
                # "USSN0A2 Curncy",
                # "USSN0A7 Curncy",
                # "USSN0A10 Curncy",
                # "USSN0A30 Curncy",
                # "USSN0C2 Curncy",
                # "USSN0C7 Curncy",
                # "USSN0C10 Curncy",
                # "USSN0C30 Curncy",
                # "USSN0F2 Curncy",
                # "USSN0F7 Curncy",
                # "USSN0F10 Curncy",
                # "USSN012 Curncy",
                # "USSN015 Curncy",
                # "USSN017 Curncy",
                # "USSN0110 Curncy",
                # 
                # 
                # "FVAISPO Comdty",
                # "TUAISPO Comdty",
                # "TYAISPO Comdty",
                # "USAISPO Comdty",
                # "WNAISPO Comdty",
                # 
                # "USSP2 Curncy",
                # "USSP3 Curncy",
                # "USSP5 Curncy",
                # "USSP7 Curncy",
                # "USSP10 Curncy",
                # "USSP30 Curncy"
  
                "USYC2Y7Y Index",
                "USSN0A30 Curncy",
                "USSP2 Curncy",
                "USAISPO Comdty",
                "USGG7YR Index",
                "USSN012 Curncy"
                
                  )




#### FUNCTIONS ####

## Get historical value of security, converted to time series ##
get_hist_conv <- function(stDate, edDate, sec) {
  d <- bdh(securities=sec, 
           fields=c("PX_LAST"), 
           start.date=stDate, 
           end.date=edDate, 
           include.non.trading.days=FALSE)
  
  test1 <- as.data.frame(d$PX_LAST)
  names(test1) <- sec
  rownames(test1) <- d$date
  test1 <- as.xts(test1)
  return(test1)
}

## Generate historical time series data for eabf
hist_timeseries_pull <- function(stDate, edDate, sec) {
  retdata <- xts()
  for (i in 1:length(sec)) {
    secobj <- sec[i]
    hold.dat <- get_hist_conv(stDate, edDate, secobj)
    retdata <- merge.xts(retdata, hold.dat)
  }
  return(retdata)
}

## Checks for the number of N/A values in each dataset ##
check_for_NA <- function(ts.data) {
  a <- c()
  for(i in 1:ncol(ts.data)) {
    a[i] <- sum(is.na(ts.data[,i]))
  }
  a <- as.data.frame(a)
  rownames(a) <- colnames(ts.data)
  colnames(a) <- c("Number of N/A")
  return(a)
}

## Generates simple time-series plot for each security ##
charts_plot <- function(sec) {
  for(i in 1:length(sec)) {
    plotdat <- get_hist_conv(date.start, date.end, sec[i])
    abc <- autoplot.zoo(plotdat) + xlab("Time") + ggtitle(sec[i]) + theme_minimal()
    print(abc)
  }
}





#### ANALYSIS ####
# plot charts of interest
# charts_plot(securities)

# generate time series dataset
testdat <- hist_timeseries_pull(date.start, date.end, securities)
nacheck <- check_for_NA(testdat)




## Find correlation between data ##
# fill in any NA values via interpolation/extrapolation
testdat <- na.locf(na.locf(na.approx(testdat)), fromLast = TRUE)
nacheck <- check_for_NA(testdat)
# Pearson correlation -- Assumes linearity, homoscedasticity,
cormat <- cor(testdat, use = "complete.obs")

# calculate vif for variables
vif(as.data.frame(testdat))
# identify collinear variables to exclude
ex1 <- vifstep(as.data.frame(testdat), th=10)
# exclude the highly collinear variables from dataset
testdat.vif <- exclude(as.data.frame(testdat), ex1)
ex1

ex2 <- vifcor(as.data.frame(testdat), th=0.7)
testdat.vif2 <- exclude(as.data.frame(testdat), ex2)
ex2





# ## Clearly, we have extremely high collinearity in our variables. VIF may not be the best approach to reduce multicollinearity
# 
# ## Attempt factors analysis to understand groupings among predictors
# # entering raw data and extracting 7 factors, with varimax rotation
# fact.analysis <- factanal(testdat,  factors=3, lower=0.1, rotation="varimax")
# # printing overview of factors analysis
# print(fact.analysis, digits=3, cutoff=.3, sort=TRUE)
# 
# 
# ## Now we have established theoretical groupings for our predictors.  Let's run another factors analysis to better understand potential factors
# # describes a theoretical number of factors to use in our analysis (using generalized weighted least squares)
# fa.parallel(testdat, fa="both", fm="ml")
# # factors analysis using a clustering approach to rotation (OLS = ordinary least squares)
# f.a <- fa(r=cormat, nfactors = 6, rotate = "cluster", fm = "ols")
# print(f.a$loadings, cutoff=0.3)
# # factors analysis using an oblique transformation
# f.a2 <- fa(r=cormat, nfactors = 3, rotate = "oblimax", fm = "ols")
# print(f.a2$loadings, cutoff=0.3)
# # factors analysis using an alternative oblique transformation
# f.a3 <- fa(r=cormat, nfactors = 3, rotate = "Promax", fm = "ols")
# print(f.a3$loadings, cutoff=0.3)



