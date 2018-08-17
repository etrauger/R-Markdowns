#### FORMATTING R ENVIRONMENT ####
cat("\014")
rm(list=ls())
graphics.off()


#### LIBRARY IMPORT ####

## Data manipulation ##
if (!require(devtools)) install.packages('devtools'); library(devtools)
assignInNamespace("version_info", c(devtools:::version_info, list("3.5" = list(version_min = "3.3.0", version_max = "99.99.99", path = "bin"))), "devtools")
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

## Forecasting ##
if (!require(forecast)) install.packages('forecast'); library(forecast)
if (!require(prophet)) install.packages('prophet'); library(prophet)

## Regression / Clustering / Factors Analysis ##
if (!require(glmnet)) install.packages('glmnet'); library(glmnet)
if (!require(psych)) install.packages('psych'); library(psych)
if (!require(nFactors)) install.packages('nFactors'); library(nFactors)
if (!require(GPArotation)) install.packages('GPArotation'); library(GPArotation)
if (!require(kernlab)) install.packages('kernlab'); library(kernlab)
if (!require(FactoMineR)) install.packages('FactoMineR'); library(FactoMineR)
if (!require(factoextra)) install.packages('factoextra'); library(factoextra)
if (!require(fAssets)) install.packages('fAssets'); library(fAssets)
if (!require(cluster)) install.packages('cluster'); library(cluster)
if (!require(usdm)) install.packages('usdm'); library(usdm)
if (!require(fpc)) install.packages('fpc'); library(fpc)
if (!require(ClustOfVar)) install.packages('ClustOfVar'); library(ClustOfVar)
if (!require(pracma)) install.packages('pracma'); library(pracma)
if (!require(Hmisc)) install.packages('Hmisc'); library(Hmisc)
if (!require(mclust)) install.packages('mclust'); library(mclust)
if (!require(pls)) install.packages('pls'); library(pls)
if (!require(ridge)) install.packages('ridge'); library(ridge)
if (!require(quantmod)) install.packages('quantmod'); library(quantmod)
if (!require(randomForest)) install.packages('randomForest'); library(randomForest)

## Neural Networks / Deep Learning ##
# first, must install Python from:
#    https://www.anaconda.com/download/#windows
#    https://www.python.org/downloads/
if (!require(keras)) devtools::install_github("rstudio/keras") ; library(keras)
# install_tensorflow()

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

## Generate historical time series data (yield) ##
hist_yield_pull2 <- function(stDate, edDate, sec) {
  retdata <- xts()
  for (i in 1:length(sec)) {
    secobj <- sec[i]
    hold.dat <- get_hist_yield2(stDate, edDate, secobj)
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

## Get historical yield of security, converted to time series ##
get_hist_yield2 <- function(stDate, edDate, sec) {
  d <- bdh(securities=sec,
           fields = c("YLD_YTM_MID"),
           start.date=stDate,
           end.date=edDate,
           include.non.trading.days=FALSE)
  
  test1 <- as.data.frame(d$YLD_YTM_MID)
  names(test1) <- sec
  rownames(test1) <- d$date
  test1 <- as.xts(test1)
  return(test1)
}



US_1Y_termPremium <- function() {
  us <- hist_yield_pull(date.start, date.end, sec="USGG12M Index")
  ustp <- hist_yield_pull(date.start, date.end, sec="ACMTP01 Index")
  retvals <- merge.xts(us, ustp)
  names(retvals) <- c("1Y Yield", "1Y TermPremium")
  retvals <- na.locf(na.locf(retvals), fromLast=TRUE) * 100
  
  ## Make plot ##
  plot.zoo(retvals,
           main="US 1Y Yield vs. Term Premium",
           plot.type="s",
           col=c("black", "red"),
           lwd=2,
           xlab="",
           ylab="Value (bp)")
  gridlines <- seq(from=-500, to=1000, by = 50)
  abline(h=gridlines, lty=3, col="gray")
  
  legend("top", inset=0.03,
         legend=c("US 1Y Yield", "US 1Y Term Premium"),
         col=c("black", "red"),
         lty=1, lwd=2)
  
  ## return values ##
  return(retvals)
}

US_2Y_termPremium <- function() {
  us2y <- hist_yield_pull(date.start, date.end, sec="USGG2YR Index")
  us2ytp <- hist_yield_pull(date.start, date.end, sec="ACMTP02 Index")
  retvals <- merge.xts(us2y, us2ytp)
  names(retvals) <- c("2Y Yield", "2Y TermPremium")
  retvals <- na.locf(na.locf(retvals), fromLast=TRUE) * 100
  
  ## Make plot ##
  plot.zoo(retvals,
           main="US 2Y Yield vs. Term Premium",
           plot.type="s",
           col=c("black", "red"),
           lwd=2,
           xlab="",
           ylab="Value (bp)")
  gridlines <- seq(from=-500, to=1000, by = 50)
  abline(h=gridlines, lty=3, col="gray")
  
  legend("top", inset=0.03,
         legend=c("US 2Y Yield", "US 2Y Term Premium"),
         col=c("black", "red"),
         lty=1, lwd=2)
  
  ## return values ##
  return(retvals)
}

US_3Y_termPremium <- function() {
  us <- hist_yield_pull(date.start, date.end, sec="USGG3YR Index")
  ustp <- hist_yield_pull(date.start, date.end, sec="ACMTP03 Index")
  retvals <- merge.xts(us, ustp)
  names(retvals) <- c("3Y Yield", "3Y TermPremium")
  retvals <- na.locf(na.locf(retvals), fromLast=TRUE) * 100
  
  ## Make plot ##
  plot.zoo(retvals,
           main="US 3Y Yield vs. Term Premium",
           plot.type="s",
           col=c("black", "red"),
           lwd=2,
           xlab="",
           ylab="Value (bp)")
  gridlines <- seq(from=-500, to=1000, by = 50)
  abline(h=gridlines, lty=3, col="gray")
  
  legend("top", inset=0.03,
         legend=c("US 3Y Yield", "US 3Y Term Premium"),
         col=c("black", "red"),
         lty=1, lwd=2)
  
  ## return values ##
  return(retvals)
}

US_5Y_termPremium <- function() {
  us <- hist_yield_pull(date.start, date.end, sec="USGG5YR Index")
  ustp <- hist_yield_pull(date.start, date.end, sec="ACMTP05 Index")
  retvals <- merge.xts(us, ustp)
  names(retvals) <- c("5Y Yield", "5Y TermPremium")
  retvals <- na.locf(na.locf(retvals), fromLast=TRUE) * 100
  
  ## Make plot ##
  plot.zoo(retvals,
           main="US 5Y Yield vs. Term Premium",
           plot.type="s",
           col=c("black", "red"),
           lwd=2,
           xlab="",
           ylab="Value (bp)")
  gridlines <- seq(from=-500, to=1000, by = 50)
  abline(h=gridlines, lty=3, col="gray")
  
  legend("top", inset=0.03,
         legend=c("US 5Y Yield", "US 5Y Term Premium"),
         col=c("black", "red"),
         lty=1, lwd=2)
  
  ## return values ##
  return(retvals)
}


US_7Y_termPremium <- function() {
  us <- hist_yield_pull(date.start, date.end, sec="USGG7YR Index")
  ustp <- hist_yield_pull(date.start, date.end, sec="ACMTP07 Index")
  retvals <- merge.xts(us, ustp)
  names(retvals) <- c("7Y Yield", "7Y TermPremium")
  retvals <- na.locf(na.locf(retvals), fromLast=TRUE) * 100
  
  ## Make plot ##
  plot.zoo(retvals,
           main="US 7Y Yield vs. Term Premium",
           plot.type="s",
           col=c("black", "red"),
           lwd=2,
           xlab="",
           ylab="Value (bp)")
  gridlines <- seq(from=-500, to=1000, by = 50)
  abline(h=gridlines, lty=3, col="gray")
  
  legend("top", inset=0.03,
         legend=c("US 7Y Yield", "US 7Y Term Premium"),
         col=c("black", "red"),
         lty=1, lwd=2)
  
  ## return values ##
  return(retvals)
}


US_10Y_termPremium <- function() {
  us <- hist_yield_pull(date.start, date.end, sec="USGG10YR Index")
  ustp <- hist_yield_pull(date.start, date.end, sec="ACMTP10 Index")
  retvals <- merge.xts(us, ustp)
  names(retvals) <- c("10Y Yield", "10Y TermPremium")
  retvals <- na.locf(na.locf(retvals), fromLast=TRUE) * 100
  
  ## Make plot ##
  plot.zoo(retvals,
           main="US 10Y Yield vs. Term Premium",
           plot.type="s",
           col=c("black", "red"),
           lwd=2,
           xlab="",
           ylab="Value (bp)")
  gridlines <- seq(from=-500, to=1000, by = 50)
  abline(h=gridlines, lty=3, col="gray")
  
  legend("top", inset=0.03,
         legend=c("US 10Y Yield", "US 10Y Term Premium"),
         col=c("black", "red"),
         lty=1, lwd=2)
  
  ## return values ##
  return(retvals)
}


gen_alldata <- function() {
  TP.1y <- US_1Y_termPremium()
  TP.2y <- US_2Y_termPremium()
  TP.3y <- US_3Y_termPremium()
  TP.5y <- US_5Y_termPremium()
  TP.7y <- US_7Y_termPremium()
  TP.10y <- US_10Y_termPremium()
  
  alldata <- merge.xts(TP.1y, TP.2y, TP.3y, TP.5y, TP.7y, TP.10y)
  alldata <- na.locf(na.locf(alldata), fromLast=TRUE)
  return(alldata)
}



# function for creating features from lagged variables
lag_variables_to_features <- function(data, num_lags=1) {
  d <- embed(data[, -ncol(data)], num_lags+1) # this automatically drops NA, assumes target in last column
  d <- cbind(d, data[(num_lags+1):nrow(data), ncol(data)]) # add column for target, dropping num_lags
  return(d)
}





#### ANALYSIS ####

## Set dates ##
date.end <- Sys.Date()-1
date.start <- date.end  - 360*10
training.percent <- 0.95

## Generate data matrix ##
data <- gen_alldata()
diffed <- diff(data)[-1,]
trn <- round(training.percent * nrow(data))
train.data <- data[1:trn,]
test.data <- data[(trn+1):nrow(data),]


## First, get relationship using regression ##
init.regression <- lm(X10Y.Yield ~., data=train.data)
init.predict <- predict(init.regression, test.data)
init.predict <- as.xts(init.predict)
init.plot <- merge.xts(train.data$X10Y.Yield, test.data$X10Y.Yield, init.predict)
plot.zoo(init.plot,
         main="Regression-Predicted 10Y Yield",
         plot.type="s",
         col=c("black", "green", "red"),
         lwd=2,
         xlab="",
         ylab="Value (bp)")
gridlines <- seq(from=-500, to=1000, by = 50)
abline(h=gridlines, lty=3, col="gray")

legend("top", inset=0.03,
       legend=c("Training", "Testing", "Predicted"),
       col=c("black", "green", "red"),
       lty=1, lwd=2)
init.rmsep <- sqrt(mean((test.data$X10Y.Yield - init.predict)^2))
text(x=par()$usr[2], y=par()$usr[4], paste("RMSEP =", init.rmsep), adj = c( 1, 1 ))


## Normalize data using mean/deviation of training dataset ##
data <- data.matrix(data)
train.mean <- apply(train.data, 2, mean)
train.std <- apply(train.data, 2, sd)
data <- scale(data, center=train.mean, scale=train.std)

## Break out into training and testing dataset ##
x.train <- data[1:trn, -which(colnames(data)=="X10Y.Yield")]
y.train <- data[1:trn, which(colnames(data)=="X10Y.Yield")]
x.test <- data[(trn+1):nrow(data), -which(colnames(data)=="X10Y.Yield")]
y.test <- data[(trn+1):nrow(data), which(colnames(data)=="X10Y.Yield")]

## Define model specifications ##
mod1 <- keras_model_sequential()
mod1 %>%
  layer_dense(units=100, input_shape=c(ncol(x.train))) %>%
  layer_dense(units=75) %>%
  layer_dense(units=70) %>%
  layer_dense(units=65) %>%
  layer_dense(units=60) %>%
  layer_dense(units=50, activation= 'selu') %>%
  layer_dense(units=25, activation= 'softmax') %>%
  layer_dense(units=1)

mod1 %>% compile(
  loss="mae",
  optimizer="rmsprop"
)


## Training dataset ##
mod1.history <- mod1 %>% fit(
  x.train, y.train,
  epochs=200,
  batch_size=10
)

mod1.preds <- mod1 %>% predict(x.test)
mod1.preds <- mod1.preds[,1] 
mod1.preds <- mod1.preds * train.std[which(colnames(data)=="X10Y.Yield")]
mod1.preds <- mod1.preds + train.mean[which(colnames(data)=="X10Y.Yield")]

mod1.preds <- as.xts(mod1.preds, order.by=index(test.data))
mod1.plot <- merge.xts(train.data$X10Y.Yield, test.data$X10Y.Yield, init.predict, mod1.preds)
y.test <- y.test * train.std[which(colnames(data)=="X10Y.Yield")] + train.mean[which(colnames(data)=="X10Y.Yield")]

plot.zoo(mod1.plot[(nrow(data)*(0.9*training.percent)):nrow(data),],
         main="Predicted 10Y Yield",
         plot.type="s",
         col=c("black", "green", "red", "orange"),
         lwd=2,
         xlab="",
         ylab="Value (bp)")
gridlines <- seq(from=-500, to=1000, by = 50)
abline(h=gridlines, lty=3, col="gray")

legend("top", inset=0.03,
       legend=c("Training", "Testing", "Regression Predicted", "Neural Net Predicted"),
       col=c("black", "green", "red", "orange"),
       lty=1, lwd=2)
mod1.rmsep <- sqrt(mean((y.test - mod1.preds)^2))
text(x=par()$usr[2], y=par()$usr[4], paste("RMSEP(Regression) =", init.rmsep, "\n",
                                           "RMSEP(NeuralNet) =", mod1.rmsep, "\n"), adj = c( 1, 1 ))














# 
# 
# 
# 
# 
# 
# ## Data generator ##
# # data = The original array of floating-point data, normalized
# # lookback = How many timesteps back the input data should go
# # delay = How many timesteps in the future the target should be
# # min_index and max_index = Indices in the data array that delimit which timesteps to draw from. 
# # shuffle = Whether to shuffle the samples or draw them in chronological order
# # batch_size = The number of samples per batch
# # step = The period, in timesteps, at which you sample data
# data_generator <- function(data, lookback, delay, min_index, max_index,
#                       shuffle = FALSE, batch_size = 128, step = 1) {
#   if (is.null(max_index))
#     max_index <- nrow(data) - delay - 1
#   i <- min_index + lookback
#   function() {
#     if (shuffle) {
#       rows <- sample(c((min_index+lookback):max_index), size = batch_size)
#     } else {
#       if (i + batch_size >= max_index)
#         i <<- min_index + lookback
#       rows <- c(i:min(i+batch_size-1, max_index))
#       i <<- i + length(rows)
#     }
#     
#     samples <- array(0, dim = c(length(rows), 
#                                 lookback / step,
#                                 dim(data)[[-1]]))
#     targets <- array(0, dim = c(length(rows)))
#     
#     for (j in 1:length(rows)) {
#       indices <- seq(rows[[j]] - lookback, rows[[j]]-1, 
#                      length.out = dim(samples)[[2]])
#       samples[j,,] <- data[indices,]
#       targets[[j]] <- data[rows[[j]] + delay,2]
#     }            
#     
#     list(samples, targets)
#   }
# }
# 
# ## Create three generators ##
# # train.gen = used for training, the first portion of data
# # val.gen = used for valudation, the portion after training is used
# # test_gen = used for testing, the portion after validation
# 
# lookback <- 100
# step <- 1
# delay <- 30
# batch.size <- 5
# train.min <- 1
# train.max <- trn
# test.min <- trn+1
# test.max <- nrow(data)
# 
# train.gen <- data_generator(data=data,
#                             lookback=lookback,
#                             delay=delay,
#                             min_index=train.min,
#                             max_index=train.max,
#                             shuffle=TRUE,
#                             step=step,
#                             batch_size=batch.size)
# 
# 
# test.gen <- data_generator(data=data,
#                            lookback=lookback,
#                            delay=delay,
#                            min_index=test.min,
#                            max_index=test.max,
#                            step=step,
#                            batch_size=batch.size)
# 
# val.steps <- (val.max - val.min - lookback) / batch.size
# test.steps <- (test.max - test.min - lookback) / batch.size
# 
# 
# 
