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
if (!require(reticulate)) install_github("rstudio/reticulate"); library(randomForest)

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
if (!require(bsts)) install.packages('bsts'); library(bsts)

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
library(reticulate)

## Neural Networks / Deep Learning ##
# first, must install Python from:
#    https://www.anaconda.com/download/#windows
if (!require(keras)) devtools::install_github("rstudio/keras") ; library(keras)
if (!require(tensorflow)) devtools::install_github("rstudio/tensorflow") ; library(tensorflow)
# keras::install_keras(tensorflow='gpu')

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



## Function formatting, variables to predict ##
format_outcome <- function(inpdata, forecast_days) {
  outdata <- inpdata
  datname <- colnames(inpdata)
  labels_for_output <- c(paste(datname, "_t_0", sep=""))
  for (k in 1:forecast_days) {
    kday_lag <- lag(inpdata, k=k)
    outdata <- merge.xts(outdata, kday_lag)
    newlab <- paste(datname, "_t_", k, sep="")
    labels_for_output <- c(labels_for_output, newlab)
  }
  colnames(outdata) <- labels_for_output
  outdata <- removeNA(outdata)
  return(outdata)
}


format_input <- function(inpdata, forecast_days, lag_days) {
  mandata <- inpdata
  lagstart <- forecast_days+1
  outputdata <- list()
  for (r in 1:ncol(inpdata)) {
    datlab <- colnames(inpdata)[r]
    labels_for_output <- c()
    data_append <- NULL
    for (m in lagstart:lag_days)  {
      kday_lag <- lag(inpdata[,r], k=m)
      data_append <- merge.xts(data_append, kday_lag)
      newlabel <- paste(datlab, "_t_", m, sep="")
      labels_for_output <- c(labels_for_output, newlabel)
    }
    colnames(data_append) <- labels_for_output
    data_append <- removeNA(data_append)
    outputdata <- list.append(outputdata, data_append)
  }
  return(outputdata)
}


## Function formatting, to be used to predict unknown future values ##
format_prediction <- function(inpdata, forecast_days, lag_days) {
  datname <- colnames(inpdata)
  labels_for_output <- c(paste(datname, "_t_0", sep=""))
  lagstart <- 1
  lagend <- lag_days - (forecast_days)
  outputdata <- list()
  for (r in 1:ncol(inpdata)) {
    datlab <- colnames(inpdata)[r]
    labels_for_output <- c()
    data_append <- NULL
    for (m in lagstart:lagend)  {
      kday_lag <- lag(inpdata[,r], k=m)
      data_append <- merge.xts(data_append, kday_lag)
      newlabel <- paste(datlab, "_t_", m, sep="")
      labels_for_output <- c(labels_for_output, newlabel)
    }
    colnames(data_append) <- labels_for_output
    data_append <- removeNA(data_append)
    outputdata <- list.append(outputdata, data_append)
  }
  
  x <- array(0, dim=c(nrow(outputdata[[1]]), ncol(outputdata[[1]]), length(outputdata)))
  for(j in 1:length(outputdata)) {
    x[,,j] <- outputdata[[j]]
  }
  outputdata <- x
  outputdata <- outputdata[nrow(outputdata),,]
  outputdata <- array(outputdata, dim=c(1, dim(outputdata)[1], dim(outputdata)[2]))
  return(outputdata)
}


loss_mean

#### INPUT ####

## Initial Variables ##
# date variables
date.end <- Sys.Date()-1
date.start <- date.end  - 360*10
# percentage of training / validation data
training.percent <- 0.70       # 70%
validation.percent <- 0.20     # 20%
# number of days to look in the past
nlags <- 60
# number of days to forecast
forecast_days <- 15

#### ANALYSIS ####

## Generate data matrix ##
# data <- gen_alldata()
# saveRDS(data1, file="DLData_yields.rds")
data <- readRDS("DLData_yields.rds")

## Create indicator / objective matrices ##
outcome_data <- data$X10Y.Yield
predictor_data <- data
target_data <- format_outcome(outcome_data, forecast_days)
indicator_data <- format_input(data, forecast_days, nlags)
newstart <- as.Date(min(index(indicator_data[[1]])))
target_data <- target_data[paste(newstart, "/", sep="")]

## Coerce indicator data to 3d array ##
x <- array(0, dim=c(nrow(indicator_data[[1]]), ncol(indicator_data[[1]]), length(indicator_data)))
for(j in 1:length(indicator_data)) {
  x[,,j] <- indicator_data[[j]]
}
indicator_data <- x
rm(x)

## save vector of all dates for later use ##
alldates <- index(target_data)

## Find mean/std for normalization ##
trn <- floor(nrow(target_data) * training.percent)

Y.means <- colMeans(target_data[1:trn,])
Y.sdevs <- colStdevs(target_data[1:trn,])
X.means <- colMeans(indicator_data[1:trn,,])
X.sdevs <- array(0, dim=c(dim(indicator_data)[2], dim(indicator_data)[3]))
for (j in 1:dim(indicator_data)[3]) {
  hd <- colStdevs(indicator_data[,,j])
  X.sdevs[,j] <- hd
}

## Normalize ##
target_data <- (target_data - Y.means) / Y.sdevs
for (j in 1:dim(indicator_data)[3]) {
  indicator_data[,,j] <- (indicator_data[,,j] - X.means[,j]) / X.sdevs[,j]
}



## training data ##
X.train <- indicator_data[1:trn,,]
Y.train <- target_data[1:trn,]

vl <- floor((training.percent+validation.percent)*nrow(target_data))
X.val <- indicator_data[(trn+1):vl,,]
Y.val <- target_data[(trn+1):vl,]

ts <- nrow(target_data)
X.test <- indicator_data[(vl+1):ts,,]
Y.test <- target_data[(vl+1):ts,]


## Define Model with kernel regularizer ##
mod1 <- keras_model_sequential()
factor_rows <- dim(X.train)[1]
factor_cols <- dim(X.train)[2]
factor_depth <- dim(X.train)[3]

mod1 %>%
  layer_dense(units=2*(factor_cols*factor_depth), 
              input_shape=c(factor_cols, factor_depth)) %>%
  layer_activation_leaky_relu() %>%
  layer_dropout(rate=0.1) %>%
  layer_dense(units=round((1/8)*(factor_cols*factor_depth))) %>%
  layer_activation_leaky_relu() %>%
  layer_dropout(rate=0.1) %>%
  layer_dense(units=round((1/16)*(factor_cols*factor_depth))) %>%
  layer_activation_leaky_relu() %>%
  layer_dropout(rate=0.1) %>%
  layer_dense(units=round((1/8)*(factor_cols*factor_depth))) %>%
  layer_activation_leaky_relu() %>%
  layer_dropout(rate=0.1) %>%
  layer_dense(units=factor_cols*factor_depth) %>%
  layer_activation_leaky_relu() %>%
  layer_dropout(rate=0.1) %>%
  
  layer_cudnn_lstm(units=factor_cols*factor_depth,
                   return_sequences=TRUE) %>%
  layer_batch_normalization() %>%
  layer_dropout(rate=0.1) %>%
  
  layer_cudnn_lstm(units=factor_cols*factor_depth,
                   return_sequences = TRUE) %>%
  layer_batch_normalization() %>%
  layer_dropout(rate=0.1) %>%
  
  layer_cudnn_lstm(units=factor_cols*factor_depth) %>%
  layer_batch_normalization() %>%
  layer_dropout(rate=0.1) %>%
  
  layer_dense(units=ncol(Y.train))


## Compile Model ##
mod1 %>% compile(
  loss='mse',
  optimizer=optimizer_nadam(lr=0.0001),
  metrics=c('mae')
)

## Add callback to optimize for smallest loss in model ##
flpath <- "C:/Users/etrauger/Documents/mod1.hdf5"
checkpoint <- callback_model_checkpoint(filepath = flpath,
                                        verbose=1, period=25,
                                        save_best_only=TRUE,
                                        save_weights_only=FALSE,
                                        mode='auto')

## Input values ##
eps <- 2500
maximum_height <- 0.001
minimum_height <- 0.000003
number_of_spikes <- 10


decay_rate <- 1/(eps/number_of_spikes)
cycle <- function(f) function(x) f(x - floor(x))
xform <- function(f, offset=0, rate=1, base=0, height=1)
  function(x) base + height*f((x-offset)*rate)
spike <- function(x) (x >= 0) * exp(-x)
f <- xform(cycle(xform(spike, rate=10)), height=maximum_height, rate=decay_rate, base=minimum_height)

lr_schedule <- function(epoch, lr) {
  lrn <- f(epoch)
  return(lrn)
}

# # plot learning rate evolution
# ln <- NULL
# for (i in 1:eps) {
#   blah <- lr_schedule(i)
#   ln[i] <- blah
# }
# plot(ln, type="l")


## Add callback to automatically adjust learning rate downward when training reaches plateau ##
reduce_lr <- callback_learning_rate_scheduler(lr_schedule)

## Add callback to stop computing if the computed value does not improve ##
earlystop <- callback_early_stopping(monitor = "val_loss", mode='auto', patience=round(2.5*eps/number_of_spikes))


## Fit model using trainig data, validate with validation data ##
mod1.hst <- mod1 %>% fit(
  x=X.train, y=Y.train,
  epochs=eps, batch_size=round(factor_rows/16),
  validation_data = list(X.val, Y.val),
  view_metrics=TRUE,
  # shuffle=FALSE, callbacks = list(checkpoint, reduce_lr)
  # shuffle=TRUE, callbacks = list(checkpoint, reduce_lr, earlystop)
  shuffle=FALSE, callbacks = list(checkpoint)
  # shuffle=TRUE
)


## Plot loss graph ##
plot(mod1.hst, xlim=c(-1,2))

## Select best-performing model for further analysis ##
rm(mod1)
mod1 <- keras:::keras$models$load_model(flpath)
mod1 %>% evaluate(X.test, Y.test)

## Check predictive ability of model
mod1.preds <- mod1 %>% predict(X.test)

colnames(mod1.preds) <- colnames(Y.test)
mod1.preds <- mod1.preds*Y.sdevs + Y.means
mod1.preds <- as.xts(mod1.preds, order.by=index(Y.test))
Y.test <- Y.test*Y.sdevs + Y.means

errors <- (Y.test - mod1.preds)

## Check distribution of Errors ##
plotcolors <- colorRampPalette(c("blue", "orange"))
plotcolors <- plotcolors(ncol(errors))
legend_labels <- c()
for (j in 1:ncol(errors)) {
  if (j==1) {
    plot(density(errors[,j]),
         col=plotcolors[j],
         ylim=c(0, 0.06),
         xlab="Error of Prediction (bp)",
         ylab="Density")
  } else {
    lines(density(errors[,j]),
          col=plotcolors[j])
  }
  lab <- ncol(errors)-j+1
  lab <- paste(lab, " days in future; mean = ", format(colMeans(errors)[j], digits=4), sep="")
  legend_labels <- c(legend_labels, lab)
}
legend("topright", 
       legend=legend_labels,
       inset=0.03,
       col=plotcolors, 
       lwd=2)

error_means <- colMeans(errors)
error_devs  <- colStdevs(errors)

## Format data for future prediction ##
prediction_input <- format_prediction(data, forecast_days, nlags)
for (j in 1:dim(prediction_input)[3]) {
  prediction_input[,,j] <- (prediction_input[,,j] - X.means[,j]) / X.sdevs[,j]
}


## Predict and plot future predictions ##
new_preds <- mod1 %>% predict(prediction_input)
new_preds <- (new_preds * Y.sdevs) + Y.means #+ error_means
# new_preds <- (new_preds * error_devs) + error_means
new_preds <- t(as.matrix(new_preds))
topdeviation <- new_preds + colStdevs(errors)
bottomdeviation <- new_preds - colStdevs(errors)
new_preds <- cbind(new_preds, bottomdeviation, topdeviation)
newindex <- as.Date(alldates[length(alldates)])+1
newindexset <- seq.Date(from=as.Date(newindex), length.out=length(new_preds)*2, by="day")
newindexset <- newindexset[isWeekday(newindexset)]
newindexset <- newindexset[1:length(new_preds[,1])]
newindexset <- rev(newindexset)

new_preds <- as.xts(new_preds, order.by=newindexset)
plot.obj <- merge.xts(Y.test[,1], new_preds[,1])
index(plot.obj) <- as.Date(index(plot.obj))

## Plotting results ##
plot.zoo(plot.obj,
         main="Predicted 10Y Yield vs. Actual",
         plot.type="s",
         col=c("black", "green"),
         lwd=2,
         lty=1,
         xlab="",
         ylab="Value (bp)")
gridlines <- seq(from=-100, to=1000, by =  10)
abline(h=gridlines, lty=3, col="gray")

polygon(c(index(new_preds), rev(index(new_preds))), 
        c(as.numeric(new_preds[,3]), as.numeric(rev(new_preds[,2]))),
        col = adjustcolor("gray",alpha.f=0.5), 
        border = FALSE)

legend("topleft", inset=0.03,
       legend=c("Actual", "Future Prediction"),
       col=c("black", "green"),
       lty=1, 
       lwd=2)













