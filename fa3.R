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
securities <- c("USGG2YR Index",
                "USGG3YR Index",
                "USGG5YR Index",
                "USGG7YR Index",
                "USGG10YR Index",
                "USGG30YR Index",
                "GDBR2 Index",
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
                
                "BTAAISP Comdty",
                "BTAAISPO Comdty",
                "BTAAFWD Comdty",
                "BTA1 Comdty",
                "BTSAISP Comdty",
                "BTSAISPO Comdty",
                "BTSAFWD Comdty",
                "BTS1 Comdty",
                "DUAISP Comdty",
                "DUAISPO Comdty",
                "DUAFWD Comdty",
                "DU1 Comdty",
                "FVAISP Comdty",
                "FVAISPO Comdty",
                "FVAFWD Comdty",
                "FV1 Comdty",
                "GAISP Comdty",
                "GAISPO Comdty",
                "GAFWD Comdty",
                "G 1 Comdty",
                "IKAISP Comdty",
                "IKAISPO Comdty",
                "IKAFWD Comdty",
                "IK1 Comdty",
                "JBAISP Comdty",
                "JBAISPO Comdty",
                "JBAFWD Comdty",
                "JB1 Comdty",
                "OATAISP Comdty",
                "OATAISPO Comdty",
                "OATAFWD Comdty",
                "OAT1 Comdty",
                "OEAISP Comdty",
                "OEAISPO Comdty",
                "OEAFWD Comdty",
                "OE1 Comdty",
                "RXAISP Comdty",
                "RXAISPO Comdty",
                "RXAFWD Comdty",
                "RX1 Comdty",
                "TUAISP Comdty",
                "TUAISPO Comdty",
                "TUAFWD Comdty",
                "TU1 Comdty",
                "TYAISP Comdty",
                "TYAISPO Comdty",
                "TYAFWD Comdty",
                "TY1 Comdty",
                "UBAISP Comdty",
                "UBAISPO Comdty",
                "UBAFWD Comdty",
                "UB1 Comdty",
                "USAISP Comdty",
                "USAISPO Comdty",
                "USAFWD Comdty",
                "US1 Comdty",
                "WNAISP Comdty",
                "WNAISPO Comdty",
                "WNAFWD Comdty",
                "WN1 Comdty",
                
                "USFOSC2 Curncy",
                "USFOSC3 Curncy",
                "USFOSC4 Curncy",
                "USFOSC5 Curncy",
                "USFOSC7 Curncy",
                "EUFOSC2 Curncy",
                "EUFOSC3 Curncy",
                "EUFOSC4 Curncy",
                "EUFOSC5 Curncy",
                "EUFOSC7 Curncy",
                
                "EUSS0203 Curncy",
                "EUSS0205 Curncy",
                "EUSS0210 Curncy",
                "EUSS0230 Curncy",
                "EUSS0510 Curncy",
                "EUSS0530 Curncy",
                "EUSS0710 Curncy",
                "EUSS1015 Curncy",
                "EUSS1030 Curncy",
                "EUSS1050 Curncy",
                
                "EUBS2 Curncy",
                "EUBS3 Curncy",
                "EUBS5 Curncy",
                "EUBS7 Curncy",
                "EUBS10 Curncy",
                "EUBS15 Curncy",
                "EUBS20 Curncy",
                "EUBS25 Curncy",
                "EUBS30 Curncy",
                "EUBS40 Curncy",
                "EUBS50 Curncy",
                
                "EFSF GB 1Y Corp",
                "EFSF GB 2Y Corp",
                "EFSF GB 20Y Corp"
                
                
)
security_names <- list()
security_names <- list.append(security_names, "USGG2YR Index" = "US Generic 2Y Yield")
security_names <- list.append(security_names, "USGG3YR Index" = "US Generic 3Y Yield")
security_names <- list.append(security_names, "USGG5YR Index" = "US Generic 5Y Yield")
security_names <- list.append(security_names, "USGG7YR Index" = "US Generic 7Y Yield")
security_names <- list.append(security_names, "USGG10YR Index" = "US Generic 10Y Yield")
security_names <- list.append(security_names, "USGG30YR Index" = "US Generic 30Y Yield")

security_names <- list.append(security_names, "GDBR2 Index" = "Germany 2Y Yield")
security_names <- list.append(security_names, "GDBR3 Index" = "Germany 3Y Yield")
security_names <- list.append(security_names, "GDBR5 Index" = "Germany 5Y Yield")
security_names <- list.append(security_names, "GDBR7 Index" = "Germany 7Y Yield")
security_names <- list.append(security_names, "GDBR10 Index" = "Germany 10Y Yield")
security_names <- list.append(security_names, "GDBR15 Index" = "Germany 15Y Yield")
security_names <- list.append(security_names, "GDBR20 Index" = "Germany 20Y Yield")
security_names <- list.append(security_names, "GDBR30 Index" = "Germany 30Y Yield")

security_names <- list.append(security_names, "GFRN2 Index" = "France 2Y OAT BTAN Yield")
security_names <- list.append(security_names, "GFRN3 Index" = "France 3Y OAT BTAN Yield")
security_names <- list.append(security_names, "GFRN5 Index" = "France 5Y OAT BTAN Yield")
security_names <- list.append(security_names, "GFRN7 Index" = "France 7Y OAT BTAN Yield")
security_names <- list.append(security_names, "GFRN10 Index" = "France 10Y OAT BTAN Yield")
security_names <- list.append(security_names, "GFRN15 Index" = "France 15Y OAT BTAN Yield")
security_names <- list.append(security_names, "GFRN20 Index" = "France 20Y OAT BTAN Yield")
security_names <- list.append(security_names, "GFRN30 Index" = "France 30Y OAT BTAN Yield")

security_names <- list.append(security_names, "GNTH2YR Index" = "Netherlands 2Y Yield")
security_names <- list.append(security_names, "GNTH3YR Index" = "Netherlands 3Y Yield")
security_names <- list.append(security_names, "GNTH5YR Index" = "Netherlands 5Y Yield")
security_names <- list.append(security_names, "GNTH7YR Index" = "Netherlands 7Y Yield")
security_names <- list.append(security_names, "GNTH10YR Index" = "Netherlands 10Y Yield")
security_names <- list.append(security_names, "GNTH30YR Index" = "Netherlands 30Y Yield")

security_names <- list.append(security_names, "GAGB2YR Index" = "Austria 2Y Yield")
security_names <- list.append(security_names, "GAGB3YR Index" = "Austria 3Y Yield")
security_names <- list.append(security_names, "GAGB5YR Index" = "Austria 5Y Yield")
security_names <- list.append(security_names, "GAGB7YR Index" = "Austria 7Y Yield")
security_names <- list.append(security_names, "GAGB10YR Index" = "Austria 10Y Yield")
security_names <- list.append(security_names, "GAGB15YR Index" = "Austria 15Y Yield")
security_names <- list.append(security_names, "GAGB20YR Index" = "Austria 20Y Yield")
security_names <- list.append(security_names, "GAGB30YR Index" = "Austria 30Y Yield")

security_names <- list.append(security_names, "GBTPGR2 Index" = "Italy 2Y Gross Yield")
security_names <- list.append(security_names, "GBTPGR3 Index" = "Italy 3Y Gross Yield")
security_names <- list.append(security_names, "GBTPGR5 Index" = "Italy 5Y Gross Yield")
security_names <- list.append(security_names, "GBTPGR7 Index" = "Italy 7Y Gross Yield")
security_names <- list.append(security_names, "GBTPGR10 Index" = "Italy 10Y Gross Yield")
security_names <- list.append(security_names, "GBTPGR15 Index" = "Italy 15Y Gross Yield")
security_names <- list.append(security_names, "GBTPGR20 Index" = "Italy 20Y Gross Yield")
security_names <- list.append(security_names, "GBTPGR30 Index" = "Italy 30Y Gross Yield")

security_names <- list.append(security_names, "GBGB2YR Index" = "Belgium 2Y Yield")
security_names <- list.append(security_names, "GBGB3YR Index" = "Belgium 3Y Yield")
security_names <- list.append(security_names, "GBGB5YR Index" = "Belgium 5Y Yield")
security_names <- list.append(security_names, "GBGB7YR Index" = "Belgium 7Y Yield")
security_names <- list.append(security_names, "GBGB10YR Index" = "Belgium 10Y Yield")
security_names <- list.append(security_names, "GBGB15YR Index" = "Belgium 15Y Yield")
security_names <- list.append(security_names, "GBGB20YR Index" = "Belgium 20Y Yield")

security_names <- list.append(security_names, "GSPG2YR Index" = "Spanish Generic 2Y Yield")
security_names <- list.append(security_names, "GSPG3YR Index" = "Spanish Generic 3Y Yield")
security_names <- list.append(security_names, "GSPG5YR Index" = "Spanish Generic 5Y Yield")
security_names <- list.append(security_names, "GSPG7YR Index" = "Spanish Generic 7Y Yield")
security_names <- list.append(security_names, "GSPG10YR Index" = "Spanish Generic 10Y Yield")
security_names <- list.append(security_names, "GSPG15YR Index" = "Spanish Generic 15Y Yield")
security_names <- list.append(security_names, "GSPG30YR Index" = "Spanish Generic 30Y Yield")




security_names <- list.append(security_names, "BTAAISP Comdty" = "BTAA Invoice Spread")
security_names <- list.append(security_names, "BTAAISPO Comdty" = "BTAA Invoice Spread OIS")
security_names <- list.append(security_names, "BTAAFWD Comdty" = "BTAA Forward Yield")
security_names <- list.append(security_names, "BTA1 Comdty" = "Generic 1st Mid-Term Euro-OAT Futures")

security_names <- list.append(security_names, "BTSAISP Comdty" = "BTSA Invoice Spread")
security_names <- list.append(security_names, "BTSAISPO Comdty" = "BTSA Invoice Spread OIS")
security_names <- list.append(security_names, "BTSAFWD Comdty" = "BTSA Forward Yield")
security_names <- list.append(security_names, "BTS1 Comdty" = "Generic 1st Mid-Term Euro-BTP Futures")

security_names <- list.append(security_names, "DUAISP Comdty" = "DUA Invoice Spread")
security_names <- list.append(security_names, "DUAISPO Comdty" = "DUA Invoice Spread OIS")
security_names <- list.append(security_names, "DUAFWD Comdty" = "DUA Forward Yield")
security_names <- list.append(security_names, "DU1 Comdty" = "Generic 1st Euro Schatz")

security_names <- list.append(security_names, "FVAISP Comdty" = "FVA Invoice Spread")
security_names <- list.append(security_names, "FVAISPO Comdty" = "FVA Invoice Spread OIS")
security_names <- list.append(security_names, "FVAFWD Comdty" = "FVA Forward Yield")
security_names <- list.append(security_names, "FV1 Comdty" = "Generic 1st US Treasury 5Y")

security_names <- list.append(security_names, "GAISP Comdty" = "GA Invoice Spread")
security_names <- list.append(security_names, "GAISPO Comdty" = "GA Invoice Spread OIS")
security_names <- list.append(security_names, "GAFWD Comdty" = "GA Forward Yield")
security_names <- list.append(security_names, "G 1 Comdty" = "Generic 1st Long Gilt")

security_names <- list.append(security_names, "IKAISP Comdty" = "IKA Invoice Spread")
security_names <- list.append(security_names, "IKAISPO Comdty" = "IKA Invoice Spread OIS")
security_names <- list.append(security_names, "IKAFWD Comdty" = "IKA Forward Yield")
security_names <- list.append(security_names, "IK1 Comdty" = "Generic 1st Euro BTP Future")

security_names <- list.append(security_names, "JBAISP Comdty" = "JBA Invoice Spread")
security_names <- list.append(security_names, "JBAISPO Comdty" = "JBA Invoice Spread OIS")
security_names <- list.append(security_names, "JBAFWD Comdty" = "JBA Forward Yield")
security_names <- list.append(security_names, "JB1 Comdty" = "Generic 1st Japan 10Y")

security_names <- list.append(security_names, "OATAISP Comdty" = "OATA Invoice Spread")
security_names <- list.append(security_names, "OATAISPO Comdty" = "OATA Invoice Spread OIS")
security_names <- list.append(security_names, "OATAFWD Comdty" = "OATA Forward Yield")
security_names <- list.append(security_names, "OAT1 Comdty" = "Generic 1st French Bond")

security_names <- list.append(security_names, "OEAISP Comdty" = "OEA Invoice Spread")
security_names <- list.append(security_names, "OEAISPO Comdty" = "OEA Invoice Spread OIS")
security_names <- list.append(security_names, "OEAFWD Comdty" = "OEA Forward Yield")
security_names <- list.append(security_names, "OE1 Comdty" = "Generic 1st Euro Bobl")

security_names <- list.append(security_names, "RXAISP Comdty" = "RXA Invoice Spread")
security_names <- list.append(security_names, "RXAISPO Comdty" = "RXA Invoice Spread OIS")
security_names <- list.append(security_names, "RXAFWD Comdty" = "RXA Forward Yield")
security_names <- list.append(security_names, "RX1 Comdty" = "Generic 1st Euro Bund")

security_names <- list.append(security_names, "TUAISP Comdty" = "TUA Invoice Spread")
security_names <- list.append(security_names, "TUAISPO Comdty" = "TUA Invoice Spread OIS")
security_names <- list.append(security_names, "TUAFWD Comdty" = "TUA Forward Yield")
security_names <- list.append(security_names, "TU1 Comdty" = "Generic 1st US 2Y Treasury Note")

security_names <- list.append(security_names, "TYAISP Comdty" = "TYA Invoice Spread")
security_names <- list.append(security_names, "TYAISPO Comdty" = "TYA Invoice Spread OIS")
security_names <- list.append(security_names, "TYAFWD Comdty" = "TYA Forward Yield")
security_names <- list.append(security_names, "TY1 Comdty" = "Generic 1st US 10Y Treasury Note")

security_names <- list.append(security_names, "UBAISP Comdty" = "UBA Invoice Spread")
security_names <- list.append(security_names, "UBAISPO Comdty" = "UBA Invoice Spread OIS")
security_names <- list.append(security_names, "UBAFWD Comdty" = "UBA Forward Yield")
security_names <- list.append(security_names, "UB1 Comdty" = "Generic 1st US 10Y Treasury Note")

security_names <- list.append(security_names, "USAISP Comdty" = "USA Invoice Spread")
security_names <- list.append(security_names, "USAISPO Comdty" = "USA Invoice Spread OIS")
security_names <- list.append(security_names, "USAFWD Comdty" = "USA Forward Yield")
security_names <- list.append(security_names, "US1 Comdty" = "Generic 1st US Long Treasury Bond")

security_names <- list.append(security_names, "WNAISP Comdty" = "WNA Invoice Spread")
security_names <- list.append(security_names, "WNAISPO Comdty" = "WNA Invoice Spread OIS")
security_names <- list.append(security_names, "WNAFWD Comdty" = "WNA Forward Yield")
security_names <- list.append(security_names, "WN1 Comdty" = "Generic 1st US Treasury Ultra Bond")



security_names <- list.append(security_names, "USFOSC2 Curncy" = "USD FRA/OIS Spread 3M IMM2")
security_names <- list.append(security_names, "USFOSC3 Curncy" = "USD FRA/OIS Spread 3M IMM3")
security_names <- list.append(security_names, "USFOSC4 Curncy" = "USD FRA/OIS Spread 3M IMM4")
security_names <- list.append(security_names, "USFOSC5 Curncy" = "USD FRA/OIS Spread 3M IMM5")
security_names <- list.append(security_names, "USFOSC7 Curncy" = "USD FRA/OIS Spread 3M IMM7")
security_names <- list.append(security_names, "EUFOSC2 Curncy" = "EUR FRA/OIS Spread 3M IMM2")
security_names <- list.append(security_names, "EUFOSC3 Curncy" = "EUR FRA/OIS Spread 3M IMM3")
security_names <- list.append(security_names, "EUFOSC4 Curncy" = "EUR FRA/OIS Spread 3M IMM4")
security_names <- list.append(security_names, "EUFOSC5 Curncy" = "EUR FRA/OIS Spread 3M IMM5")
security_names <- list.append(security_names, "EUFOSC7 Curncy" = "EUR FRA/OIS Spread 3M IMM7")

security_names <- list.append(security_names, "EUSS0203 Curncy" = "EUR Swap Curve 2x3")
security_names <- list.append(security_names, "EUSS0205 Curncy" = "EUR Swap Curve 2x5")
security_names <- list.append(security_names, "EUSS0210 Curncy" = "EUR Swap Curve 2x10")
security_names <- list.append(security_names, "EUSS0230 Curncy" = "EUR Swap Curve 2x30")
security_names <- list.append(security_names, "EUSS0510 Curncy" = "EUR Swap Curve 5x10")
security_names <- list.append(security_names, "EUSS0530 Curncy" = "EUR Swap Curve 5x30")
security_names <- list.append(security_names, "EUSS0710 Curncy" = "EUR Swap Curve 7x10")
security_names <- list.append(security_names, "EUSS1015 Curncy" = "EUR Swap Curve 10x15")
security_names <- list.append(security_names, "EUSS1030 Curncy" = "EUR Swap Curve 10x30")
security_names <- list.append(security_names, "EUSS1050 Curncy" = "EUR Swap Curve 10x50")

security_names <- list.append(security_names, "EUBS2 Curncy" = "EUR_USD XCCY Basis 2Y")
security_names <- list.append(security_names, "EUBS3 Curncy" = "EUR_USD XCCY Basis 3Y")
security_names <- list.append(security_names, "EUBS5 Curncy" = "EUR_USD XCCY Basis 5Y")
security_names <- list.append(security_names, "EUBS7 Curncy" = "EUR_USD XCCY Basis 7Y")
security_names <- list.append(security_names, "EUBS10 Curncy" = "EUR_USD XCCY Basis 10Y")
security_names <- list.append(security_names, "EUBS15 Curncy" = "EUR_USD XCCY Basis 15Y")
security_names <- list.append(security_names, "EUBS20 Curncy" = "EUR_USD XCCY Basis 20Y")
security_names <- list.append(security_names, "EUBS25 Curncy" = "EUR_USD XCCY Basis 25Y")
security_names <- list.append(security_names, "EUBS30 Curncy" = "EUR_USD XCCY Basis 30Y")
security_names <- list.append(security_names, "EUBS40 Curncy" = "EUR_USD XCCY Basis 40Y")
security_names <- list.append(security_names, "EUBS50 Curncy" = "EUR_USD XCCY Basis 50Y")



security_names <- list.append(security_names, "EFSF GB 1Y Corp" = "European Financial Stability Facility Generic 1Y Corporate")
security_names <- list.append(security_names, "EFSF GB 2Y Corp" = "European Financial Stability Facility Generic 2Y Corporate")
security_names <- list.append(security_names, "EFSF GB 20Y Corp" = "European Financial Stability Facility Generic 20Y Corporate")




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

## Generate historical time series data for each security ##
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
testdat2 <- hist_timeseries_pull(date.start, date.end, securities)
nacheck <- check_for_NA(testdat2)




## Find correlation between data ##
# fill in any NA values via interpolation/extrapolation
testdat2 <- na.locf(na.locf(na.approx(testdat2)), fromLast = TRUE)
nacheck <- check_for_NA(testdat2)
# Pearson correlation -- Assumes linearity, homoscedasticity,
cormat2 <- cor(testdat2, use = "complete.obs")
cormat2[upper.tri(cormat2)] <- ""
cormat2 <- cor(testdat2, use = "complete.obs")
hc = findCorrelation(cormat2, cutoff=0.90)
hc = sort(hc)
red.testdat <- testdat2[, -c(hc)]
ncol(testdat2)
ncol(red.testdat)
lowerCor(red.testdat)
# sum column values of correlation
corsums <- colSums(abs(cormat2))
corsums <- as.data.frame(corsums)
names(corsums) <- c("Sum of Correlation (magnitude)")
# visualize this correlation

# calculate vif for variables
vif(as.data.frame(testdat2))
# identify collinear variables to exclude
ex1 <- vifcor(as.data.frame(testdat2), th=0.8)
ex2 <- vifstep(as.data.frame(testdat2), th=20)
# exclude the highly collinear variables from dataset
testdat3 <- exclude(as.data.frame(testdat2), ex1)
testdat4 <- exclude(as.data.frame(testdat2), ex2)
ex1
ex2



#---- ggcorr(testdat2, label=TRUE, label_alpha=TRUE)

