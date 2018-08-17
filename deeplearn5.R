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


## Function formatting, variables to predict ##
format_output <- function(inpdata) {
  outdata <- inpdata
  
}




#### ANALYSIS ####

## Set dates ##
date.end <- Sys.Date()-1
date.start <- date.end  - 360*10
training.percent <- 0.9
nlags <- 50

## Generate data matrix ##
# data <- gen_alldata()
# saveRDS(data1, file="DLData_yields.rds")
data <- readRDS("DLData_yields.rds")

## Manipulate Data Matrix ##
diffed <- diff(data)[-1,]
trn <- round(training.percent * nrow(data))
mod1.data <- lag_variables_to_features(data, nlags)
mod1.data <- as.matrix(mod1.data)
colnames(mod1.data) <- NULL
mod1.data <- as.data.frame(mod1.data)
colnames(mod1.data)[ncol(mod1.data)] <- "X10Y.Yield"
train.data <- mod1.data[1:trn,]
test.data <- mod1.data[(trn+1):nrow(mod1.data),]

## First, get relationship using regression ##
init.regression <- lm(X10Y.Yield ~., data=train.data)
init.predict <- predict(init.regression, test.data)
init.predict <- as.xts(init.predict)
init.rmsep <- sqrt(mean((test.data$X10Y.Yield - init.predict)^2))


## Now, look at ML algorithm to solve ##
# create lags as features
mod1.data <- lag_variables_to_features(data, nlags)
mod1.data <- data.matrix(mod1.data)
train.data <- mod1.data[1:trn,]
train.mean <- apply(train.data, 2, mean)
train.std <- apply(train.data, 2, sd)
mod1.data <- scale(mod1.data, center=train.mean, scale=train.std)

trn.ed <- floor(trn*0.8)
X.train <- mod1.data[1:trn.ed, -ncol(mod1.data)]
Y.train <- mod1.data[1:trn.ed, ncol(mod1.data)]

vl <- trn.ed+1
X.val <- mod1.data[vl:trn, -ncol(mod1.data)]
Y.val <- mod1.data[vl:trn, ncol(mod1.data)]

ts <- trn+1
X.test <- mod1.data[ts:nrow(mod1.data), -ncol(mod1.data)]
Y.test <- mod1.data[ts:nrow(mod1.data), ncol(mod1.data)]


## Define Model with kernel regularizer ##
mod1 <- keras_model_sequential()
nnn <- ncol(X.train)
mod1 %>%
  layer_dense(units=nnn+20, input_shape=nnn) %>%
  layer_activation_leaky_relu() %>%
  layer_dense(units=nnn) %>%
  layer_activation_leaky_relu() %>%
  layer_dense(units=nnn*5) %>%
  layer_activation_leaky_relu() %>%
  layer_dense(units=nnn) %>%
  layer_activation_leaky_relu() %>%
  
  # autoencoder stack
  layer_dense(units=round(nnn/4), activation='relu') %>%
  layer_dense(units=round(nnn/16), activation='relu') %>%
  layer_dense(units=4, activation='relu') %>%
  layer_dense(units=round(nnn/16), activation='relu') %>%
  layer_dense(units=round(nnn/4), activation='relu') %>%
  layer_dense(units=nnn, activation='relu') %>%
  layer_dropout(rate=0.3) %>%
  
  layer_dense(units=round(nnn/4), activation='relu') %>%
  layer_dense(units=round(nnn/16), activation='relu') %>%
  layer_dense(units=4, activation='relu') %>%
  layer_dense(units=round(nnn/16), activation='relu') %>%
  layer_dense(units=round(nnn/4), activation='relu') %>%
  layer_dense(units=nnn, activation='relu') %>%
  
  layer_dense(units=nnn*2, activation='relu') %>%
  layer_dropout(rate=0.3) %>%
  
  layer_dense(units=nnn, activation='relu') %>%
  layer_dense(units=nnn*5, activation='relu') %>%
  layer_dropout(rate=0.3) %>%
  
  layer_dense(units=nnn, activation='relu') %>%
  layer_dense(units=round(nnn/4), activation='relu') %>%
  layer_dense(units=1)

## Compile Model ##
mod1 %>% compile(
  loss='mse',
  optimizer=optimizer_adagrad(lr=0.0005),
  metrics=c('mae')
)

## Add callback to optimize for smallest loss in model ##
flpath <- "C:/Users/etrauger/Documents/mod1.hdf5"
checkpoint <- callback_model_checkpoint(filepath = flpath,
                                        verbose=1,
                                        save_best_only=TRUE,
                                        save_weights_only=FALSE,
                                        mode='auto')

## Input values ##
eps <- 3000
maximum_height <- 0.001
minimum_height <- 0.0000001
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

# plot learning rate evolution
ln <- NULL
for (i in 1:eps) {
  blah <- lr_schedule(i)
  ln[i] <- blah
}
plot(ln, type="l")


## Add callback to automatically adjust learning rate downward when training reaches plateau ##
reduce_lr <- callback_learning_rate_scheduler(lr_schedule)

## Add callback to stop computing if the computed value does not improve ##
earlystop <- callback_early_stopping(monitor = "val_loss", mode='auto', patience=round(2.5*eps/number_of_spikes))


## Fit model using trainig data, validate with validation data ##
mod1.hst <- mod1 %>% fit(
  x=X.train, y=Y.train,
  epochs=eps, batch_size=nrow(X.train),
  validation_data = list(X.val, Y.val),
  view_metrics=FALSE,
  shuffle=TRUE, callbacks = list(checkpoint, reduce_lr, earlystop)
  # shuffle=TRUE, callbacks = list(checkpoint)
)


## Plot loss graph ##
plot(mod1.hst, xlim=c(-1,2))

## Select best-performing model for further analysis ##
rm(mod1)
mod1 <- keras:::keras$models$load_model(flpath)
mod1 %>% evaluate(X.test, Y.test)

## Check predictive ability of model
mod1.data <- lag_variables_to_features(data, nlags)
train.data <- mod1.data[1:trn,]
test.data <- mod1.data[(trn+1):nrow(mod1.data),]
mod1.preds <- mod1 %>% predict(X.test)
mod1.preds <- mod1.preds[,1] 
mod1.preds <- mod1.preds * train.std[length(train.std)]
mod1.preds <- mod1.preds + train.mean[length(train.mean)]

## Calculating RMSEP for NN method ##
mod1.preds <- as.xts(mod1.preds, order.by=index(test.data))
mod1.plot <- merge.xts(train.data[,ncol(train.data)], test.data[,ncol(test.data)], init.predict, mod1.preds)
Y.test <- Y.test * train.std[length(train.std)] + train.mean[length(train.mean)]
mod1.rmsep <- sqrt(mean((Y.test - mod1.preds)^2))

## Plotting results ##
plot.zoo(mod1.plot[(nrow(mod1.data)*(0.8*training.percent)):nrow(mod1.data),],
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
text(x=par()$usr[2], y=par()$usr[4], paste("RMSEP (Regression) =", init.rmsep, "\n",
                                           "RMSEP (Neural Net) =", mod1.rmsep, "\n"), adj = c( 1, 1 ))








