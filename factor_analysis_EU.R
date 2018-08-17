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

## Input names of securities desired ##
securities <- c( "GDBR2 Index",
                 "GDBR3 Index",
                 "GDBR5 Index",
                 "GDBR7 Index",
                 "GDBR10 Index",
                 "GDBR15 Index",
                 "GDBR20 Index",
                 "GDBR30 Index",
                 "GFRN2 Index",
                 "GFRN3 Index",
                 "GFRN5 Index",
                 "GFRN7 Index",
                 "GFRN10 Index",
                 "GFRN15 Index",
                 "GFRN20 Index",
                 "GFRN30 Index",
                 "GNTH2YR Index",
                 "GNTH3YR Index",
                 "GNTH5YR Index",
                 "GNTH7YR Index",
                 "GNTH10YR Index",
                 "GNTH30YR Index",
                 "GAGB2YR Index",
                 "GAGB3YR Index",
                 "GAGB5YR Index",
                 "GAGB7YR Index",
                 "GAGB10YR Index",
                 "GAGB15YR Index",
                 "GAGB20YR Index",
                 "GAGB30YR Index",
                 "GBTPGR2 Index",
                 "GBTPGR3 Index",
                 "GBTPGR5 Index",
                 "GBTPGR7 Index",
                 "GBTPGR10 Index",
                 "GBTPGR15 Index",
                 "GBTPGR20 Index",
                 "GBTPGR30 Index",
                 "GBGB2YR Index",
                 "GBGB3YR Index",
                 "GBGB5YR Index",
                 "GBGB7YR Index",
                 "GBGB10YR Index",
                 "GBGB15YR Index",
                 "GBGB20YR Index",
                 "GSPG2YR Index",
                 "GSPG3YR Index",
                 "GSPG5YR Index",
                 "GSPG7YR Index",
                 "GSPG10YR Index",
                 "GSPG15YR Index",
                 "GSPG30YR Index",
                 "GJGB2 Index",
                 "GJGB3 Index",
                 "GJGB5 Index",
                 "GJGB7 Index",
                 "GJGB10 Index",
                 "GJGB20 Index",
                 "GJGB30 Index",
                 "GJGB40 Index",
                 # "GUKG2 Index",
                 # "GUKG3 Index",
                 # "GUKG5 Index",
                 # "GUKG7 Index",
                 # "GUKG10 Index",
                 # "GUKG20 Index",
                 # "GUKG30 Index",

                 
                 "ASWASHTT Curncy",
                 "BTAAISP Curncy",
                 "DUAISP Curncy",
                 "GAISP Curncy",
                 "IKAISP Curncy",
                 "JBAISP Curncy",
                 "OATAISP Curncy",
                 "RXAISP Curncy",
                 "OEAISP Curncy",
                 "UBAISP Curncy",
                 
                 "EUNEA2 Curncy",
                 "EUNEA7 Curncy",
                 "EUNEA10 Curncy",
                 "EUNEA30 Curncy",
                 "EUNE12 Curncy",
                 "EUNE15 Curncy",
                 "EUNE110 Curncy"
                 
                 # "BPNEA2 Curncy",
                 # "BPNEA7 Curncy",
                 # "BPNEA10 Curncy",
                 # "BPNEA30 Curncy",
                 # "BPNEC2 Curncy",
                 # "BPNEC7 Curncy",
                 # "BPNEC10 Curncy",
                 # "BPNEF2 Curncy",
                 # "BPNEF7 Curncy",
                 # "BPNEF10 Curncy",
                 # "BPNE12 Curncy",
                 # "BPNE110 Curncy"
                 
                 
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
# Pearson correlation -- Assumes linearity, homoscedasticity,
cormat <- cor(testdat, use = "complete.obs")

# calculate vif for variables
vif(as.data.frame(testdat))
# identify collinear variables to exclude
ex1 <- vifstep(as.data.frame(testdat), th=20)
# exclude the highly collinear variables from dataset
testdat.vif <- exclude(as.data.frame(testdat), ex1)
ex1

ex2 <- vifcor(as.data.frame(testdat), th=0.9)
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



