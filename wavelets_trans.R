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
if (!require(WaveletComp)) install.packages("WaveletComp") ; library(WaveletComp)




## Interaction with Bloomberg ##
if (!require(Rblpapi)) install.packages('Rblpapi'); library(Rblpapi)
blpConnect()




data <- readRDS("DLData_yields.rds")
data <- as.data.frame(data)

trans.data <- NULL
for (q in 1:ncol(data)) {
  wt1 <- analyze.wavelet(my.data=data, my.series=q, loess.span=0.003)
  newplot <- as.xts(wt1$series)[,2]
  trans.data <- merge.xts(trans.data, newplot)
}


plot(as.xts(data))
plot(trans.data)

