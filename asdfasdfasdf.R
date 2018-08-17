#### FORMATTING R ENVIRONMENT ####
cat("\014")
rm(list=ls())
graphics.off()


#### LIBRARY IMPORT ####

## Data manipulation ##
if (!require(stringr)) install.packages('stringr'); library(stringr)
if (!require(lubridate)) install.packages('lubridate'); library(lubridate)
if (!require(qcc)) install.packages('qcc'); library(qcc)
if (!require(dplyr)) install.packages('dplyr'); library(dplyr)
if (!require(xts)) install.packages('xts'); library(xts)
if (!require(zoo)) install.packages('zoo'); library(zoo)
if (!require(timeDate)) install.packages('timeDate'); library(timeDate)
if (!require(bizdays)) install.packages('bizdays'); library(bizdays)
if (!require(data.table)) install.packages('data.table'); library(data.table)
if (!require(quantmod)) install.packages('quantmod'); library(quantmod)
if (!require(readxl)) install.packages('readxl'); library(readxl)
if (!require(readr)) install.packages('readr'); library(readr)
if (!require(rlist)) install.packages('rlist'); library(rlist)
if (!require(stats)) install.packages('stats'); library(stats)
if (!require(sqldf)) install.packages('sqldf'); library(sqldf)
if (!require(openxlsx)) install.packages('openxlsx'); library(openxlsx)
if (!require(tidyquant)) install.packages('tidyquant'); library(tidyquant)

## Data Visualization ##
if (!require(ggplot2)) install.packages('ggplot2'); library(ggplot2)
if (!require(ggfortify)) install.packages('ggfortify'); library(ggfortify)
if (!require(GGally)) install.packages('GGally'); library(GGally)
if (!require(corrplot)) install.packages('corrplot'); library(corrplot)
if (!require(gganimate)) install.packages('gganimate'); library(gganimate)
if (!require(animation)) install.packages('animation'); library(animation)
if (!require(magick)) install.packages('magick'); library(magick)
if (!require(mosaic)) install.packages('mosaic'); library(mosaic)
if (!require(plotrix)) install.packages('plotrix'); library(plotrix)
if (!require(shape)) install.packages('shape'); library(shape)
if (!require(Quandl)) install.packages('Quandl'); library(Quandl)


## Modeling / Forecasting ##
if (!require(randomForest)) install.packages('randomForest'); library(randomForest)
if (!require(glmnet)) install.packages('glmnet'); library(glmnet)
if (!require(forecast)) install.packages('forecast'); library(forecast)
if (!require(prophet)) install.packages('prophet'); library(prophet)
if (!require(psych)) install.packages('psych'); library(psych)
if (!require(nFactors)) install.packages('nFactors'); library(nFactors)
if (!require(GPArotation)) install.packages('GPArotation'); library(GPArotation)
if (!require(kernlab)) install.packages('kernlab'); library(kernlab)
if (!require(usdm)) install.packages('usdm'); library(usdm)
if (!require(FactoMineR)) install.packages('FactoMineR'); library(FactoMineR)
if (!require(factoextra)) install.packages('factoextra'); library(factoextra)
if (!require(fAssets)) install.packages('fAssets'); library(fAssets)
if (!require(cluster)) install.packages('cluster'); library(cluster)
if (!require(fpc)) install.packages('fpc'); library(fpc)
if (!require(ClustOfVar)) install.packages('ClustOfVar'); library(ClustOfVar)
if (!require(Hmisc)) install.packages('Hmisc'); library(Hmisc)
if (!require(mclust)) install.packages('mclust'); library(mclust)
if (!require(pls)) install.packages('pls'); library(pls)
if (!require(ridge)) install.packages('ridge'); library(ridge)
if (!require(quantmod)) install.packages('quantmod'); library(quantmod)
if (!require(pracma)) install.packages('pracma'); library(pracma)


## Interaction with Bloomberg ##
if (!require(Rblpapi)) install.packages('Rblpapi'); library(Rblpapi)
blpConnect()



#### FUNCTIONS ####

## Generate historical time series data ##
hist_timeseries_pull <- function(stDate, edDate, sec) {
  retdata <- xts()
  for (i in 1:length(sec)) {
    secobj <- sec[i]
    hold.dat <- get_hist_yield(stDate, edDate, secobj)
    retdata <- merge.xts(retdata, hold.dat)
  }
  retdata <- diff(retdata)
  return(retdata)
}

## Get historical yield of security, converted to time series ##
get_hist_yield <- function(stDate, edDate, sec) {
  d <- bdh(securities=sec,
           fields = c("PX_LAST"),
           start.date=stDate,
           end.date=edDate,
           include.non.trading.days=FALSE)
  
  test1 <- as.data.frame(d$PX_LAST)
  names(test1) <- sec
  rownames(test1) <- d$date
  test1 <- as.xts(test1)
  return(test1)
}

## Generate historical time series data (yield) ##
hist_yield_pull <- function(stDate, edDate, sec) {
  retdata <- xts()
  for (i in 1:length(sec)) {
    secobj <- sec[i]
    hold.dat <- get_hist_yield(stDate, edDate, secobj)
    retdata <- merge.xts(retdata, hold.dat)
  }
  
  # create appropriate starting point
  NonNAport <- retdata[complete.cases(retdata)]
  firstindex <- index(NonNAport)[1]
  retdata <- retdata[paste(firstindex, "/", sep="")]
  retdata <- na.locf(na.locf(na.approx(retdata)), fromLast = TRUE)
  # retdata <- diff(retdata)
  # retdata <- retdata[-1,]
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

## Formats portfolio for analysis ##
format_port <- function(tstdt, tstpt, wgts, secwgts) {
  # do weighting and rowsums for portfolio
  portf <- tstpt
  for (i in 1:length(wgts)) {
    portf[,i] <- tstpt[,i] * wgts[i]
  }
  portfolio <- .xts(rowSums(portf), .index(portf))
  # do weighting for factors
  factf <- tstdt
  for (j in 1:length(secwgts)) {
    factf[,j] <- tstdt[,j] * secwgts[j]
  }
  # merge weighted timeseries objects
  fullport <- merge.xts(portfolio, factf)
  # create appropriate starting point
  NonNAport <- fullport[complete.cases(fullport)]
  firstindex <- index(NonNAport)[1]
  fullport <- fullport[paste(firstindex, "/", sep="")]
  # roll over N/A values
  fullport <- na.locf(na.locf(na.approx(fullport)), fromLast = TRUE)
  return(fullport)
}


format_trade <- function(td, wd, tp, wp) {
  # weight trade
  trade.single <- tp*wp
  colnames(trade.single) <- "trade.name"
  
  # weight portfolio
  factf <- td
  for (j in 1:length(wd)) {
    factf[,j] <- td[,j] * wd[j]
  }
  
  # merge trade with factors
  newport <- merge.xts(trade.single, factf)
  # create appropriate starting point
  NonNAport <- newport[complete.cases(newport)]
  firstindex <- index(NonNAport)[1]
  newport <- newport[paste(firstindex, "/", sep="")]
  
  # fill in #N/A with previous day data
  newport <- na.locf(na.locf(newport), fromLast = TRUE)
  return(newport)
}



date.end <- Sys.Date() 
date.start <- date.end  - 360*30






#### Analysis ####

us.1m <- hist_yield_pull(date.start, date.end, sec="GBM Govt")
us.3m <- hist_yield_pull(date.start, date.end, sec="GB3 Govt")
us.6m <- hist_yield_pull(date.start, date.end, sec="GB6 Govt")
us.12m <- hist_yield_pull(date.start, date.end, sec="GB12 Govt")
us.2y <- hist_yield_pull(date.start, date.end, sec="USGG2YR Index")
us.3y <- hist_yield_pull(date.start, date.end, sec="USGG3YR Index")
us.5y <- hist_yield_pull(date.start, date.end, sec="USGG5YR Index")
us.7y <- hist_yield_pull(date.start, date.end, sec="USGG7YR Index")
us.10y <- hist_yield_pull(date.start, date.end, sec="USGG10YR Index")
us.30y <- hist_yield_pull(date.start, date.end, sec="USGG30YR Index")


de.1m <- hist_yield_pull(date.start, date.end, sec="GTDEM1M Govt")
de.3m <- hist_yield_pull(date.start, date.end, sec="GTDEM3M Govt")
de.6m <- hist_yield_pull(date.start, date.end, sec="GTDEM6M Govt")
de.1y <- hist_yield_pull(date.start, date.end, sec="GTDEM1Y Govt")
de.2y <- hist_yield_pull(date.start, date.end, sec="GTDEM2Y Govt")
de.3y <- hist_yield_pull(date.start, date.end, sec="GTDEM3Y Govt")
de.4y <- hist_yield_pull(date.start, date.end, sec="GTDEM4Y Govt")
de.5y <- hist_yield_pull(date.start, date.end, sec="GTDEM5Y Govt")
de.6y <- hist_yield_pull(date.start, date.end, sec="GTDEM6Y Govt")
de.7y <- hist_yield_pull(date.start, date.end, sec="GTDEM7Y Govt")
de.8y <- hist_yield_pull(date.start, date.end, sec="GTDEM8Y Govt")
de.9y <- hist_yield_pull(date.start, date.end, sec="GTDEM9Y Govt")
de.10y <- hist_yield_pull(date.start, date.end, sec="GTDEM10Y Govt")
de.15y <- hist_yield_pull(date.start, date.end, sec="GTDEM15Y Govt")
de.20y <- hist_yield_pull(date.start, date.end, sec="GTDEM20Y Govt")
de.25y <- hist_yield_pull(date.start, date.end, sec="GTDEM25Y Govt")
de.30y <- hist_yield_pull(date.start, date.end, sec="GTDEM30Y Govt")


usd.sw.1d <- hist_yield_pull(date.start, date.end, sec="US00O/N Index")
usd.sw.2d <- hist_yield_pull(date.start, date.end, sec="USDR2T Curncy")
usd.sw.1w <- hist_yield_pull(date.start, date.end, sec="US0001W Index")
usd.sw.1m <- hist_yield_pull(date.start, date.end, sec="US0001M Index")
usd.sw.2m <- hist_yield_pull(date.start, date.end, sec="US0002M Index")
usd.sw.3m <- hist_yield_pull(date.start, date.end, sec="US0003M Index")
usd.sw.4m <- hist_yield_pull(date.start, date.end, sec="USSWD CMPN Curncy")
usd.sw.5m <- hist_yield_pull(date.start, date.end, sec="USSWE Curncy")
usd.sw.6m <- hist_yield_pull(date.start, date.end, sec="USSWF CMPN Curncy")
usd.sw.7m <- hist_yield_pull(date.start, date.end, sec="USSWG Curncy")
usd.sw.8m <- hist_yield_pull(date.start, date.end, sec="USSWH Curncy")
usd.sw.9m <- hist_yield_pull(date.start, date.end, sec="USSWI Curncy")
usd.sw.10m <- hist_yield_pull(date.start, date.end, sec="USSWJ Curncy")
usd.sw.11m <- hist_yield_pull(date.start, date.end, sec="USSWK Curncy")
usd.sw.1y <- hist_yield_pull(date.start, date.end, sec="USSWAP1 Curncy")
usd.sw.2y <- hist_yield_pull(date.start, date.end, sec="USSWAP2 Curncy")
usd.sw.3y <- hist_yield_pull(date.start, date.end, sec="USSWAP3 Curncy")
usd.sw.4y <- hist_yield_pull(date.start, date.end, sec="USSWAP4 Curncy")
usd.sw.5y <- hist_yield_pull(date.start, date.end, sec="USSWAP5 Curncy")
usd.sw.6y <- hist_yield_pull(date.start, date.end, sec="USSWAP6 Curncy")
usd.sw.7y <- hist_yield_pull(date.start, date.end, sec="USSWAP7 Curncy")
usd.sw.8y <- hist_yield_pull(date.start, date.end, sec="USSWAP8 Curncy")
usd.sw.9y <- hist_yield_pull(date.start, date.end, sec="USSWAP9 Curncy")
usd.sw.10y <- hist_yield_pull(date.start, date.end, sec="USSWAP10 Curncy")
usd.sw.11y <- hist_yield_pull(date.start, date.end, sec="USSWAP11 Curncy")
usd.sw.12y <- hist_yield_pull(date.start, date.end, sec="USSWAP12 Curncy")
usd.sw.15y <- hist_yield_pull(date.start, date.end, sec="USSWAP15 Curncy")
usd.sw.20y <- hist_yield_pull(date.start, date.end, sec="USSWAP10 Curncy")
usd.sw.25y <- hist_yield_pull(date.start, date.end, sec="USSWAP25 Curncy")
usd.sw.30y <- hist_yield_pull(date.start, date.end, sec="USSWAP30 Curncy")

dowjones.fut <- hist_yield_pull(date.start, date.end, sec="DJIAFP Index")
sp.fut <- hist_yield_pull(date.start, date.end, sec="SP1 Index")

dowjones.industrial <- hist_yield_pull(date.start, date.end, sec="INDU Index")
dowjones.transport <- hist_yield_pull(date.start, date.end, sec="TRAN Index")
dowjones.utility <- hist_yield_pull(date.start, date.end, sec="UTIL Index")
dowjones.composite <- hist_yield_pull(date.start, date.end, sec="COMP Index")

sp.100 <- hist_yield_pull(date.start, date.end, sec="OEX Index")
sp.500 <- hist_yield_pull(date.start, date.end, sec="SPX Index")
sp.400mid <- hist_yield_pull(date.start, date.end, sec="MID Index")
sp.600small <- hist_yield_pull(date.start, date.end, sec="SML Index")
sp.1500comp <- hist_yield_pull(date.start, date.end, sec="SPR Index")

nyse.comp <- hist_yield_pull(date.start, date.end, sec="NYA Index")
nyse.us100 <- hist_yield_pull(date.start, date.end, sec="NYID Index")
nyse.intl100 <- hist_yield_pull(date.start, date.end, sec="NYIID Index")
nyse.tmt <- hist_yield_pull(date.start, date.end, sec="NYYID Index")
nyse.worldlead <- hist_yield_pull(date.start, date.end, sec="NYLID Index")
nyse.health <- hist_yield_pull(date.start, date.end, sec="NYP Index")
nyse.financial <- hist_yield_pull(date.start, date.end, sec="NYK Index")

nasdaq.composite <- hist_yield_pull(date.start, date.end, sec="CCMP Index")
nasdaq.100 <- hist_yield_pull(date.start, date.end, sec="NDX Index")
nasdaq.industrial <- hist_yield_pull(date.start, date.end, sec="CIND Index")
nasdaq.transportation <- hist_yield_pull(date.start, date.end, sec="CTRN Index")
nasdaq.telecom <- hist_yield_pull(date.start, date.end, sec="CUTL Index")
nasdaq.insurance <- hist_yield_pull(date.start, date.end, sec="CINS Index")
nasdaq.bank <- hist_yield_pull(date.start, date.end, sec="CBNK Index")
nasdaq.insurance <- hist_yield_pull(date.start, date.end, sec="CINS Index")
nasdaq.otherFin <- hist_yield_pull(date.start, date.end, sec="CFIN Index")
nasdaq.financial <- hist_yield_pull(date.start, date.end, sec="NDF Index")
nasdaq.computer <- hist_yield_pull(date.start, date.end, sec="IXK Index")
nasdaq.biotech <- hist_yield_pull(date.start, date.end, sec="NBI Index")
nasdaq.100pre <- hist_yield_pull(date.start, date.end, sec="QMI Index")
nasdaq.100post <- hist_yield_pull(date.start, date.end, sec="QIV Index")

russell.1000 <- hist_yield_pull(date.start, date.end, sec="RIY Index")
russell.2000 <- hist_yield_pull(date.start, date.end, sec="RTY Index")
russell.3000 <- hist_yield_pull(date.start, date.end, sec="RAY Index")

phila.oil <- hist_yield_pull(date.start, date.end, sec="OSX Index")
phila.gold <- hist_yield_pull(date.start, date.end, sec="XAU Index")
phila.semicon <- hist_yield_pull(date.start, date.end, sec="SOX Index")
phila.utility <- hist_yield_pull(date.start, date.end, sec="UTY Index")



canada.tsx.comp <- hist_yield_pull(date.start, date.end, sec="SPTSX Index")
canada.tsx.equity <- hist_yield_pull(date.start, date.end, sec="TXEQ Index")
canada.tsx.60 <- hist_yield_pull(date.start, date.end, sec="SPTSX60 Index")
canada.tsx.venture <- hist_yield_pull(date.start, date.end, sec="SPTSXVEN Index")

mexico.bmv.ipc <- hist_yield_pull(date.start, date.end, sec="MEXBOL Index")
mexico.bmv.inMex <- hist_yield_pull(date.start, date.end, sec="INMEX Index")
mexico.bmv.mc30 <- hist_yield_pull(date.start, date.end, sec="IMC30 Index")
mexico.bmv.irt <- hist_yield_pull(date.start, date.end, sec="IRT Index")
mexico.biva.pr <- hist_yield_pull(date.start, date.end, sec="FTBIVA Index")

panama.bvps.general <- hist_yield_pull(date.start, date.end, sec="BVPSBVPS Index")

argentina.merval <- hist_yield_pull(date.start, date.end, sec="MERVAL Index")
argentina.burcap <- hist_yield_pull(date.start, date.end, sec="BURCAP Index")
argentina.merval2 <- hist_yield_pull(date.start, date.end, sec="MAR Index")
argentina.ibg.bolsageneral <- hist_yield_pull(date.start, date.end, sec="IBG Index")

brazil.ibovespa <- hist_yield_pull(date.start, date.end, sec="IBOV Index")
brazil.ibrx <- hist_yield_pull(date.start, date.end, sec="IBX Index")
brazil.ibrx50 <- hist_yield_pull(date.start, date.end, sec="IBX50 Index")

chile.clx.select <- hist_yield_pull(date.start, date.end, sec="IPSA Index")
chile.clx.general <- hist_yield_pull(date.start, date.end, sec="IGPA Index")
chile.clx.inter10 <- hist_yield_pull(date.start, date.end, sec="INTER10 Index")
chile.clx.65 <- hist_yield_pull(date.start, date.end, sec="CHILE65 Index")
chile.clx.largecap <- hist_yield_pull(date.start, date.end, sec="CHLRGCAP Index")
chile.clx.smallcap <- hist_yield_pull(date.start, date.end, sec="CHSMLCAP Index")

venezuela.sm <- hist_yield_pull(date.start, date.end, sec="IBVC Index")

peru.gen <- hist_yield_pull(date.start, date.end, sec="SPBLPGPT Index")
peru.lima25 <- hist_yield_pull(date.start, date.end, sec="SPBL25PT Index")

colombia.colcap <- hist_yield_pull(date.start, date.end, sec="COLCAP Index")
colombia.colsc <- hist_yield_pull(date.start, date.end, sec="COLSC Index")
colombia.colir <- hist_yield_pull(date.start, date.end, sec="COLIR Index")
colombia.coleqty <- hist_yield_pull(date.start, date.end, sec="COLEQTY Index")

bermuda.stx <- hist_yield_pull(date.start, date.end, sec="BSX Index")



large_data_pull <- list(us.1m,
                        us.3m,
                        us.6m,
                        us.12m,
                        us.2y,
                        us.3y,
                        us.5y,
                        us.7y,
                        us.10y,
                        us.30y,
                        
                        de.1m,
                        de.3m,
                        de.6m,
                        de.1y,
                        de.2y,
                        de.3y,
                        de.4y,
                        de.5y,
                        de.6y,
                        de.7y,
                        de.8y,
                        de.9y,
                        de.10y,
                        de.15y,
                        de.20y,
                        de.25y,
                        de.30y,
                        
                        usd.sw.1d,
                        usd.sw.2d,
                        usd.sw.1w,
                        usd.sw.1m,
                        usd.sw.2m,
                        usd.sw.3m,
                        usd.sw.4m,
                        usd.sw.5m,
                        usd.sw.6m,
                        usd.sw.7m,
                        usd.sw.8m,
                        usd.sw.9m,
                        usd.sw.10m,
                        usd.sw.11m,
                        usd.sw.1y,
                        usd.sw.2y,
                        usd.sw.3y,
                        usd.sw.4y,
                        usd.sw.5y,
                        usd.sw.6y,
                        usd.sw.7y,
                        usd.sw.8y,
                        usd.sw.9y,
                        usd.sw.10y,
                        usd.sw.11y,
                        usd.sw.12y,
                        usd.sw.15y,
                        usd.sw.20y,
                        usd.sw.25y,
                        usd.sw.30y,
                        
                        dowjones.fut,
                        sp.fut,
                        
                        dowjones.industrial,
                        dowjones.transport,
                        dowjones.utility,
                        dowjones.composite,
                        
                        sp.100,
                        sp.500,
                        sp.400mid,
                        sp.600small,
                        sp.1500comp,
                        
                        nyse.comp,
                        nyse.us100,
                        nyse.intl100,
                        nyse.tmt,
                        nyse.worldlead,
                        nyse.health,
                        nyse.financial,
                        
                        nasdaq.composite,
                        nasdaq.100,
                        nasdaq.industrial,
                        nasdaq.transportation,
                        nasdaq.telecom,
                        nasdaq.insurance,
                        nasdaq.bank,
                        nasdaq.insurance,
                        nasdaq.otherFin,
                        nasdaq.financial,
                        nasdaq.computer,
                        nasdaq.biotech,
                        nasdaq.100pre,
                        nasdaq.100post,
                        
                        russell.1000,
                        russell.2000,
                        russell.3000,
                        
                        phila.oil,
                        phila.gold,
                        phila.semicon,
                        phila.utility,
                        
                        canada.tsx.comp,
                        canada.tsx.equity,
                        canada.tsx.60,
                        canada.tsx.venture,
                        mexico.bmv.ipc,
                        mexico.bmv.inMex,
                        mexico.bmv.mc30,
                        mexico.bmv.irt,
                        mexico.biva.pr,
                        panama.bvps.general,
                        argentina.merval,
                        argentina.burcap,
                        argentina.merval2,
                        argentina.ibg.bolsageneral,
                        brazil.ibovespa,
                        brazil.ibrx,
                        brazil.ibrx50,
                        chile.clx.select,
                        chile.clx.general,
                        chile.clx.inter10,
                        chile.clx.65,
                        chile.clx.largecap,
                        chile.clx.smallcap,
                        venezuela.sm,
                        peru.gen,
                        peru.lima25,
                        colombia.colcap,
                        colombia.colsc,
                        colombia.colir,
                        colombia.coleqty,
                        bermuda.stx
                        
                        
                        
)




saveRDS(large_data_pull, file="data_comp.rds")
# data <- readRDS("data_comp.rds")
