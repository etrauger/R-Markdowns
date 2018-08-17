#### FORMATTING R ENVIRONMENT ####
cat("\014")
rm(list=ls())
dev.off()


#### LIBRARY IMPORT ####

#### LIBRARY IMPORT ####
## Data manipulation ##
library(stringr)
library(lubridate)
library(qcc)
library(dplyr)
library(xts)
library(zoo)
library(timeDate)
library(bizdays)
library(data.table)
library(quantmod)
library(readxl)
library(readr)
library(rlist)
library(stats)
library(sqldf)

## Data Visualization ##
library(ggplot2)
library(ggfortify)
library(GGally)
library(corrplot)
library(gganimate)
library(animation)
library(magick)
library(mosaic)
library(plotrix)
library(shape)

## Modeling / Forecasting ##
library(randomForest)
library(glmnet)
library(caret)
library(forecast)
library(prophet)
library(psych)
library(nFactors)
library(GPArotation)
library(kernlab)
library(usdm)
library(FactoMineR)
library(factoextra)
library(fAssets)
library(cluster)
library(fpc)
library(ClustOfVar)
library(Hmisc)
library(mclust)
library(ClusterR)
library(pls)
library(ridge)


## Interaction with Bloomberg ##
library(Rblpapi)
blpConnect()


## Other ##



#### INITIALIZATION ####
## Start and end date ##
date.end <- Sys.Date()
date.start <- date.end - 360

## Input names of securities desired ##
futures <- c( "ED1 COMB Comdty" = 1,
              "ED2 COMB Comdty" = 1,
              "ED3 COMB Comdty" = 1,
              "ED4 COMB Comdty" = 1,
              "ED5 COMB Comdty" = 1,
              "ED6 COMB Comdty" = 1,
              "ED7 COMB Comdty" = 1,
              "ED8 COMB Comdty" = 1,
              "ED9 COMB Comdty" = 1,
              "ED10 COMB Comdty" = 1,
              "ED11 COMB Comdty" = 1,
              "ED12 COMB Comdty" = 1,
              "ED13 COMB Comdty" = 1,
              "ED14 COMB Comdty" = 1
)


OIS.swaps <- c( "USSOA Curncy" = 1,
                "USSOB Curncy" = 1,
                "USSOC Curncy" = 1,
                "USSOD Curncy" = 1,
                "USSOE Curncy" = 1,
                "USSOF Curncy" = 1,
                "USSOG Curncy" = 1,
                "USSOH Curncy" = 1,
                "USSOI Curncy" = 1,
                "USSOJ Curncy" = 1,
                "USSOK Curncy" = 1,
                "USSO1 Curncy" = 1,
                "USSO1C Curncy" = 1,
                "USSO1F Curncy" = 1,
                "USSO1I Curncy" = 1,
                "USSO2 Curncy" = 1,
                "USSOFED1 ICPL Curncy" = 1,
                "USSOFED2 ICPL Curncy" = 1,
                "USSOFED3 ICPL Curncy" = 1,
                "USSOFED4 ICPL Curncy" = 1,
                "USSOFED5 ICPL Curncy" = 1,
                "USSOFED6 ICPL Curncy" = 1
)


FRA.libor <- c( "USFR0AD CMPN Curncy" = 1,
                "USFR0BE CMPN Curncy" = 1,
                "USFR0CF CMPN Curncy" = 1,
                "USFR0DG CMPN Curncy" = 1,
                "USFR0EH CMPN Curncy" = 1,
                "USFR0FI CMPN Curncy" = 1,
                "USFR0GJ CMPN Curncy" = 1,
                "USFR0HK CMPN Curncy" = 1,
                "USFR0I1 CMPN Curncy" = 1
)


FRA.OIS.spread <- c( "USFOSC1 CMPN Curncy" = 1,
                     "USFOSC2 CMPN Curncy" = 1,
                     "USFOSC3 CMPN Curncy" = 1,
                     "USFOSC4 CMPN Curncy" = 1,
                     "USFOSC5 CMPN Curncy" = 1,
                     "USFOSC6 CMPN Curncy" = 1,
                     "USFOSC7 CMPN Curncy" = 1
)


swaps <- c( # "USSW1 CMPN Curncy" = 1,
  "USSW2 CMPN Curncy" = 1,
  # "USSW3 CMPN Curncy" = 1,
  # "USSW4 CMPN Curncy" = 1,
  "USSW5 CMPN Curncy" = 1,
  # "USSW6 CMPN Curncy" = 1,
  # "USSW7 CMPN Curncy" = 1,
  # "USSW8 CMPN Curncy" = 1,
  # "USSW9 CMPN Curncy" = 1,
  "USSW10 CMPN Curncy" = 1,
  # "USSW12 CMPN Curncy" = 1,
  # "USSW15 CMPN Curncy" = 1,
  # "USSW20 CMPN Curncy" = 1,
  # "USSW25 CMPN Curncy" = 1,
  "USSW30 CMPN Curncy" = 1
  # "USSWBV1 CMPN Curncy" = 1,
  # "USSWCV1 CMPN Curncy" = 1,
  # "USSWDV1 CMPN Curncy" = 1,
  # "USSWEV1 CMPN Curncy" = 1,
  # "USSWFV1 CMPN Curncy" = 1,
  # "USSWGV1 CMPN Curncy" = 1,
  # "USSWHV1 CMPN Curncy" = 1,
  # "USSWIV1 CMPN Curncy" = 1,
  # "USSWJV1 CMPN Curncy" = 1,
  # "USSWKV1 CMPN Curncy" = 1,
  # "USSW1V1 CMPN Curncy" = 1,
  # "USSW1CV1 CMPN Curncy" = 1,
  # "USSW1FV1 CMPN Curncy" = 1,
  # "USSW2V1 CMPN Curncy" = 1
)


bbti <- c( "USSWAP2 CBBT Curncy" = 1,
           "USSWAP3 CBBT Curncy" = 1,
           "USSWAP4 CBBT Curncy" = 1,
           "USSWAP5 CBBT Curncy" = 1,
           "USSWAP7 CBBT Curncy" = 1,
           "USSWAP10 CBBT Curncy" = 1,
           "USSWAP12 CBBT Curncy" = 1,
           "USSWAP15 CBBT Curncy" = 1,
           "USSWAP20 CBBT Curncy" = 1,
           "USSWAP25 CBBT Curncy" = 1,
           "USSWAP30 CBBT Curncy" = 1
)


benchmark.bond.yields <- c( "CT2 Govt" = 1,
                            "CT3 Govt" = 1,
                            "CT5 Govt" = 1,
                            "CT7 Govt" = 1,
                            "CT30 Govt" = 1
)


swap.butterfly <- c( "US030405 CBBT Curncy" = 1,
                     "US101215 CBBT Curncy" = 1,
                     "US101530 CBBT Curncy" = 1,
                     "US102030 CBBT Curncy" = 1,
                     "US202530 CBBT Curncy" = 1
                     
)

swap.spreads <- c( "USSP2 CMPN Curncy" = 1,
                   "USSP3 CMPN Curncy" = 1,
                   "USSP4 CMPN Curncy" = 1,
                   "USSP5 CMPN Curncy" = 1,
                   "USSP7 CMPN Curncy" = 1,
                   "USSP10 CMPN Curncy" = 1,
                   "USSP12 CMPN Curncy" = 1,
                   "USSP15 CMPN Curncy" = 1,
                   "USSP20 CMPN Curncy" = 1,
                   "USSP25 CMPN Curncy" = 1,
                   "USSP30 CMPN Curncy" = 1
)


basis.swaps <- c( "USBG2 CMPN Curncy" = 1,
                  "USBG3 CMPN Curncy" = 1,
                  "USBG5 CMPN Curncy" = 1,
                  "USBG7 CMPN Curncy" = 1,
                  "USBG10 CMPN Curncy" = 1,
                  "USBG15 CMPN Curncy" = 1,
                  "USBG20 CMPN Curncy" = 1,
                  "USBG30 CMPN Curncy" = 1,
                  
                  "USBCF CMPN Curncy" = 1,
                  "USBC1 CMPN Curncy" = 1,
                  "USBC2 CMPN Curncy" = 1,
                  "USBC3 CMPN Curncy" = 1,
                  "USBC4 CMPN Curncy" = 1,
                  "USBC5 CMPN Curncy" = 1,
                  "USBC7 CMPN Curncy" = 1,
                  "USBC10 CMPN Curncy" = 1,
                  "USBC15 CMPN Curncy" = 1,
                  "USBC20 CMPN Curncy" = 1,
                  "USBC30 CMPN Curncy" = 1,
                  
                  "USBAAF CMPN Curncy" = 1,
                  "USBA1 CMPN Curncy" = 1,
                  "USBA2 CMPN Curncy" = 1,
                  "USBA3 CMPN Curncy" = 1,
                  "USBA4 CMPN Curncy" = 1,
                  "USBA5 CMPN Curncy" = 1,
                  "USBA7 CMPN Curncy" = 1,
                  "USBA10 CMPN Curncy" = 1,
                  "USBA15 CMPN Curncy" = 1,
                  "USBA20 CMPN Curncy" = 1,
                  "USBA30 CMPN Curncy" = 1
)


forward.swaps <- c( "USFS0C2 CMPN Curncy" = 1,
                    "USFS0C3 CMPN Curncy" = 1,
                    "USFS0C4 CMPN Curncy" = 1,
                    "USFS0C5 CMPN Curncy" = 1,
                    "USFS0C7 CMPN Curncy" = 1,
                    "USFS0C10 CMPN Curncy" = 1,
                    "USFS0C12 CMPN Curncy" = 1,
                    "USFS0C15 CMPN Curncy" = 1,
                    "USFS0C20 CMPN Curncy" = 1,
                    
                    "USFS0F2 CMPN Curncy" = 1,
                    "USFS0F3 CMPN Curncy" = 1,
                    "USFS0F4 CMPN Curncy" = 1,
                    "USFS0F5 CMPN Curncy" = 1,
                    "USFS0F7 CMPN Curncy" = 1,
                    "USFS0F10 CMPN Curncy" = 1,
                    "USFS0F12 CMPN Curncy" = 1,
                    "USFS0F15 CMPN Curncy" = 1,
                    "USFS0F20 CMPN Curncy" = 1
)


## Input portfolio of trades desired ##

trades <- c(  "ED12 COMB Comdty" = 1000,
              "USGG3YR Index" = -1000, 
              "USGG10YR Index" = 50,
              "USGG30YR Index" = -50
)



#### FUNCTIONS ####

## Get current bid/ask for security ##
get_current_bidask <- function(sec) {
  d <- bdp(securities=sec, 
           fields=c("PX_BID", "PX_ASK", "TIME"))
  test1 <- as.data.frame(d)
  return(test1)
}

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
  retdata <- diff(retdata)
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





#### ANALYSIS ####

run_FactorsAnalysis <- function() {
  
  ## Set up portfolio information ##
  # decide which indicators to use
  secs <- c(swaps, swap.butterfly)
  
  # pull weights and security names
  sec.weights <- secs
  sec.names <- names(secs)
  port.weights <- trades
  port.names <- names(trades)
  
  # generate time series of factors and trade portfolio
  testdat <- hist_timeseries_pull(date.start, date.end, sec.names)
  testport <- hist_yield_pull(date.start, date.end, port.names)
  
  # align beginning dates of portfolios
  aligned.port <- format_port(testdat, testport, port.weights, sec.weights)
  
  # check for #N/A values in the dataset
  nacheck <- check_for_NA(aligned.port)
  
  
  ## First run basic linear regression ##
  
  # simple linear regression
  test.reg1 <- lm(portfolio~. , data=aligned.port)
  reg.coef <- summary(test.reg1)$coefficients
  reg.coef <- reg.coef[,1]
  reg.R2 <- 100* summary(test.reg1)$adj.r.squared
  
  # Convert residuals to predicted values
  preds <- aligned.port$portfolio - test.reg1$residuals
  colnames(preds) <- c("Predicted Values (Non-weighted)")
  
  ## Check predictions by training / testing dataset ##
  # set up training and testing datasets
  range.train <- 1:round(0.8*nrow(aligned.port))
  # range.train <- 1:(nrow(aligned.port)-1)
  train.data <- aligned.port[range.train,]
  test.data <- aligned.port[-range.train,]
  
  # fit model and run prediction
  fit.train1 <- lm(portfolio ~., data=train.data)
  predictions1 <- predict(fit.train1, test.data)
  
  # mean squared error of prediction
  fit1.ActPred <- xts(cbind(actuals=test.data$portfolio, predicteds=predictions1))
  reg.mse <- mean((fit1.ActPred$portfolio - fit1.ActPred$predicteds)^2)
  
  reg.output <- c(reg.coef, "R2"=reg.R2, "MSE"=reg.mse)
  reg.output <- reg.output[-1]
  
  
  
  
  ## Running Principal Components Regression ##
  
  # run PCR (Principal Components Regression)
  pcr.a <- pcr(portfolio ~., data=aligned.port, 
               validation="LOO", ncomp=4, jackknife=TRUE, scale=TRUE)
  
  # print data information, validation, variance explained (adjusted R-squared)
  pcr.coef <- coef(pcr.a)[,1,1] / pcr.a$scale
  yve <- 100 * drop(R2(pcr.a, estimate = "train", intercept = FALSE)$val)
  pcr.R2 <- last(yve)
  
  # run train/test protocol to predict future values
  pcr.train <- pcr(portfolio ~., data=train.data, validation="CV", ncomp=4, scale=TRUE)
  pcr.pred <- predict(pcr.train, test.data[,-1], ncomp=4, scale=TRUE)
  
  # print mean squared error of prediction 
  pcr.mse <- mean((test.data[,1] - pcr.pred[,1,1])^2)
  
  pcr.output <- c(pcr.coef, "R2"=pcr.R2, "MSE"=pcr.mse)
  
  
  
  ## Partial Least Squares Regression ##
  # run PLS (Partial Least Squares Regression)
  pls.a <- plsr(portfolio ~., data=aligned.port,
                validation="LOO", ncomp=4, jackknife=TRUE, scale=TRUE)
  
  # print data information, validation, variance explained (adjusted R-squared)
  pls.coef <- coef(pls.a)[,1,1] / pls.a$scale
  yve <- 100 * drop(R2(pls.a, estimate = "train", intercept = FALSE)$val)
  pls.R2 <- last(yve)
  
  # run train/test protocol to predict future values
  pls.train <- plsr(portfolio ~., data=train.data, validation="LOO", ncomp=4, scale=TRUE)
  pls.pred <- predict(pls.train, test.data[,-1], ncomp=4, scale=TRUE)
  
  # print mean squared error of prediction 
  pls.mse <- mean((test.data[,1] - pls.pred[,1,1])^2)
  
  pls.output <- c(pls.coef, "R2"=pls.R2, "MSE"=pls.mse)
  
  
  
  
  ## Ridge Regression ##
  # find optimal penalization constant, alpha
  alpha.num <- 50
  alpha.vals <- seq(from=0, to=1, length.out=alpha.num)
  alpha.optimal <- 0
  mse.opt <- Inf
  
  for (i in 1:alpha.num) {
    rr.train <- cv.glmnet(x=as.matrix(train.data[,-1]), 
                          y=as.matrix(train.data[,1]), 
                          family="gaussian",
                          alpha= alpha.vals[i], 
                          standardize=TRUE, 
                          nfolds=20, 
                          nlambda=500)
    rr.pred <- predict(rr.train, as.matrix(test.data[,-1]),
                       s=rr.train$lambda.1se, standardize=TRUE)
    mse.test <- mean((test.data[,1] - rr.pred)^2)
    
    if (mse.test <= mse.opt) {
      mse.opt <- mse.test
      alpha.optimal <- alpha.vals[i]
    }
    
  }
  
  # run Ridge with optimal alpha
  rr.a <- cv.glmnet(x=as.matrix(aligned.port[,-1]), 
                    y=as.matrix(aligned.port[,1]), 
                    family="gaussian",
                    alpha=alpha.optimal, 
                    standardize=TRUE, 
                    nfolds=20, 
                    nlambda=500)
  
  # print data information, validation, variance explained (adjusted R-squared)
  rr.coef <- c(as.matrix(coef(rr.a, s="lambda.1se")))
  names(rr.coef) <- coef(rr.a, s="lambda.1se")@Dimnames[[1]]
  
  # adjusted R-squared
  rr.R2 <- 100* rr.a$glmnet.fit$dev.ratio[which(rr.a$glmnet.fit$lambda == rr.a$lambda.1se)]
  
  # print mean squared error of prediction 
  rr.mse <- mse.opt
  
  rr.output <- c(rr.coef, "R2"=rr.R2, "MSE"=rr.mse)
  rr.output <- rr.output[-1]
  
  ## Do formatting for Output ##
  total.output <- cbind("Reg"=reg.output, "PCR"=pcr.output, "PLS"=pls.output, "Ridge"=rr.output)
  total.output <- as.data.frame(total.output)
  
  return(total.output)
}



coef_evolution <- function(plusdays) {
  # setup
  date.end <- Sys.Date() - plusdays
  date.start <- date.end - 360
  tsport <- run_FactorsAnalysis()
  
  reg.coef <- as.data.frame(t(tsport$Reg))
  colnames(reg.coef) <- rownames(tsport)
  rownames(reg.coef) <- date.end
  reg.coef <- as.xts(reg.coef)
  
  pcr.coef <- as.data.frame(t(tsport$PCR))
  colnames(pcr.coef) <- rownames(tsport)
  rownames(pcr.coef) <- date.end
  pcr.coef <- as.xts(pcr.coef)
  
  pls.coef <- as.data.frame(t(tsport$PLS))
  colnames(pls.coef) <- rownames(tsport)
  rownames(pls.coef) <- date.end
  pls.coef <- as.xts(pls.coef)
  
  rr.coef <- as.data.frame(t(tsport$Ridge))
  colnames(rr.coef) <- rownames(tsport)
  rownames(rr.coef) <- date.end
  rr.coef <- as.xts(rr.coef)
  
  for(i in 1:plusdays) {
    date.end <- Sys.Date() - plusdays + i
    date.start <- date.end - 360
    port <- run_FactorsAnalysis()
    
    # update regression
    reg1 <- as.data.frame(t(port$Reg))
    colnames(reg1) <- rownames(port)
    rownames(reg1) <- date.end
    reg1 <- as.xts(reg1)
    reg.coef <- rbind(reg.coef, reg1)
    
    # update pcr
    pcr1 <- as.data.frame(t(port$PCR))
    colnames(pcr1) <- rownames(port)
    rownames(pcr1) <- date.end
    pcr1 <- as.xts(pcr1)
    pcr.coef <- rbind(pcr.coef, pcr1)
    
    # update pls
    pls1 <- as.data.frame(t(port$PLS))
    colnames(pls1) <- rownames(port)
    rownames(pls1) <- date.end
    pls1 <- as.xts(pls1)
    pls.coef <- rbind(pls.coef, pls1)
    
    # update ridge
    rr1 <- as.data.frame(t(port$Ridge))
    colnames(rr1) <- rownames(port)
    rownames(rr1) <- date.end
    rr1 <- as.xts(rr1)
    rr.coef <- rbind(rr.coef, rr1)
    
    print(date.end)
  }
  
  retlist <- list("Regression"=reg.coef, "PCR"=pcr.coef, "PLS"=pls.coef, "Ridge"=rr.coef)
  return (retlist)
  
}


genplots <- function() {
  # Generate Plots to analyze how much coefficients move DoD
  abc <- print( plot.xts(regression.coef/lag(regression.coef, -1) - 1, 
              lwd=1,
              legend.loc="bottomright",
              main="Daily Coefficient Changes, Regression Analysis"))
  title(ylab="DoD % Change in Coefficient Value")
  
  print( plot(PCR.coef/lag(PCR.coef, -1) - 1, 
              main="Daily Coefficient Changes, PCR Analysis",
              lwd=1,
              legend.loc="bottomright") )
  title(ylab="DoD % Change in Coefficient Value")
  
  print( plot(PLS.coef/lag(PLS.coef, -1) - 1, 
              main="Daily Coefficient Changes, PLS Analysis",
              lwd=1,
              legend.loc="bottomright") )
  title(ylab="DoD % Change in Coefficient Value")
  
  print( plot(ridge.coef/lag(ridge.coef, -1) - 1, 
              main="Daily Coefficient Changes, Ridge Regression Analysis",
              lwd=1,
              legend.loc="bottomright") )
  title(ylab="DoD % Change in Coefficient Value")
  
  
  ## Generate output text ##
  cat("\n");cat("\n");cat("\n")
  cat("---------------Percentage of y-Variance Explained---------------")
  cat("\n");cat("\n")
  cat("      Regression Average: ", mean(regression_coefficients$R2), "\n")
  cat("               Deviation: ", sd(regression_coefficients$R2), "\n")
  cat("\n")
  cat("             PCR Average: ", mean(PCR_coefficients$R2), "\n")
  cat("               Deviation: ", sd(PCR_coefficients$R2), "\n")
  cat("\n")
  cat("             PLS Average: ", mean(PLS_coefficients$R2), "\n")
  cat("               Deviation: ", sd(PLS_coefficients$R2), "\n")
  cat("\n")
  cat("Ridge Regression Average: ", mean(ridge_coefficients$R2), "\n")
  cat("               Deviation: ", sd(ridge_coefficients$R2), "\n")
  
  cat("\n");cat("\n");cat("\n")
  cat("---------------Root Mean-Squared-Error (of Prediction) Comparison---------------")
  cat("\n");cat("\n")
  cat("      Regression Average: ", mean(sqrt(regression_coefficients$MSE)), "\n")
  cat("               Deviation: ", sd(sqrt(regression_coefficients$MSE)), "\n")
  cat("\n")
  cat("             PCR Average: ", mean(sqrt(PCR_coefficients$MSE)), "\n")
  cat("               Deviation: ", sd(sqrt(PCR_coefficients$MSE)), "\n")
  cat("\n")
  cat("             PLS Average: ", mean(sqrt(PLS_coefficients$MSE)), "\n")
  cat("               Deviation: ", sd(sqrt(PLS_coefficients$MSE)), "\n")
  cat("\n")
  cat("Ridge Regression Average: ", mean(sqrt(ridge_coefficients$MSE)), "\n")
  cat("               Deviation: ", sd(sqrt(ridge_coefficients$MSE)), "\n")
  
  
  cat("\n");cat("\n");cat("\n");cat("\n");cat("\n")
  cat("---------------Beta Prediction Comparison---------------")
  for(i in 1:ncol(regression.coef)) {
    cat("\n");cat("\n");cat("\n"); 
    cat("&&& Beta Values for Factor -", colnames(regression.coef[,i]), " &&&", "\n")
    cat("      Regression Average: ", mean(regression.coef[,i]), "\n")
    cat("                      SD: ", sd(regression.coef[,i]), "\n")
    cat("             PCR Average: ", mean(PCR.coef[,i]), "\n")
    cat("                      SD: ", sd(PCR.coef[,i]), "\n")
    cat("             PLS Average: ", mean(PLS.coef[,i]), "\n")
    cat("                      SD: ", sd(PLS.coef[,i]), "\n")
    cat("Ridge Regression Average: ", mean(ridge.coef[,i]), "\n")
    cat("                      SD: ", sd(ridge.coef[,i]), "\n")
    
  }
}



# Run analysis to gather data
return_dataframe <- coef_evolution(30)

# Get coefficients data for each of my methods
regression_coefficients <- return_dataframe$Regression
n <- ncol(regression_coefficients)
regression.coef <- regression_coefficients[,1:(n-2)]
PCR_coefficients <- return_dataframe$PCR
n <- ncol(PCR_coefficients)
PCR.coef <- PCR_coefficients[,1:(n-2)]
PLS_coefficients <- return_dataframe$PLS
n <- ncol(PLS_coefficients)
PLS.coef <- PLS_coefficients[,1:(n-2)]
ridge_coefficients <- return_dataframe$Ridge
n <- ncol(ridge_coefficients)
ridge.coef <- ridge_coefficients[,1:(n-2)]


genplots()


