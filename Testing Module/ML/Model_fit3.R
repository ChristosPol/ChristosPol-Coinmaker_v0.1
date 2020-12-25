# Idea is to build a robust model selecting the best parameters
# Use this model to predict t+1 of the time series
# Dataset
df <- klines[[1]]

# Create prediction variable
df$prediction <- shift(df$close, type = "lead")
df$UPDOWN <- ifelse(df$close-df$prediction > 0, "DOWN", "UP")

df <- na.omit(df)
df$UPDOWN[is.na(df$UPDOWN)] <- "NEUTRAL"
df$UPDOWN <- as.factor(df$UPDOWN)

# df$movement <- round(((df$prediction - df$close) / df$prediction )*100, 3)
df$returns <- c(diff(df$close), 0)

df$enter[df$UPDOWN == "UP"] <- 1
df$enter[df$UPDOWN == "DOWN"] <- -1

df$profits <- NA
df$profits <-  df$returns * df$enter
sum(df$profits)
sum(df$profits[df$UPDOWN =="UP"])
sum(df$profits[df$UPDOWN =="DOWN"])


# Add candle type
df$candle_type <- NA

df$candle_type[df$close > df$open] <- "bullish"
df$candle_type[df$close < df$open] <- "bearish"
df$candle_type[is.na(df$candle_type)] <- "neutral"
df$candle_type <- as.factor(df$candle_type)

# Indicators
# Trend, momentum volume volatility

# Trend - Strength and direction of a trend (EMA, MACD, lagged, ADX)
# Momentum - Oscilators  Relative strengths (0-100) Overbought oversold (RSI, STohastic)
# Volume OBV, CHaikin oscillator
# Volatility indicators  rate of price changes regardless of their direction Bollinger - ATR
# Using max 2-3 indicators

# EMAs
df$EMA_1 <- EMA(df$close, n = 5)
df$EMA_2 <- EMA(df$close, n = 7)
df$EMA_3 <- EMA(df$close, n = 10)
df$EMA_4 <- EMA(df$close, n = 15)
df$EMA_5 <- EMA(df$close, n = 21)
df$EMA_6 <- EMA(df$close, n = 50)
df$EMA_7 <- EMA(df$close, n = 100)
df$EMA_8 <- EMA(df$close, n = 150)
df$EMA_9 <- EMA(df$close, n = 200)

df$roc_1 <- momentum(df$close, n = 1)
df$roc_2 <- momentum(df$close, n = 3)
df$roc_3 <- momentum(df$close, n = 9)
df$roc_4 <- momentum(df$close, n = 15)
df$roc_5 <- momentum(df$close, n = 20)
df$roc_6 <- momentum(df$close, n = 30)
df$roc_7 <- momentum(df$close, n = 50)
df$roc_8 <- momentum(df$close, n = 100)



# Candle description
df$HL <- df$high - df$low
df$OC <- df$open - df$close

# RSI
df$RSI_1 <- RSI(df$close, n = 5)
df$RSI_2 <- RSI(df$close, n = 10)
df$RSI_3 <- RSI(df$close, n = 14)
df$RSI_4 <- RSI(df$close, n = 20)
df$RSI_5 <- RSI(df$close, n = 25)

# Standard deviation
df$sd_1 <- rollapplyr(df$close, 5, sd, fill = NA)
df$sd_2 <- rollapplyr(df$close, 10, sd, fill = NA)
df$sd_3 <- rollapplyr(df$close, 14, sd, fill = NA)
df$sd_4 <- rollapplyr(df$close, 20, sd, fill = NA)

# MACD default
macd <- MACD(df[, "close"])
df <- cbind(df,macd)

# MFI
df$mfi_1 <- MFI(df[, c("high", "low", "close")], df[, "volume"],
                n = 5)
df$mfi_2 <- MFI(df[, c("high", "low", "close")], df[, "volume"],
                n = 10)
df$mfi_3 <- MFI(df[, c("high", "low", "close")], df[, "volume"],
                n = 14)
df$mfi_4 <- MFI(df[, c("high", "low", "close")], df[, "volume"],
                n = 20)

# OBV default
df$obv <- OBV(df[, "close"], df[, "volume"])
# df$obv <- EMA(obv, n = 25)

# Bollinger
bollinger_1 <- BBands(df[ ,c("high", "low", "close")], n = 10,
                      sd = 1)
colnames(bollinger_1) <- paste(colnames(bollinger_1), "1", sep = "_")
#
#
bollinger_2 <- BBands(df[ ,c("high", "low", "close")], n = 10,
                      sd = 2)
colnames(bollinger_2) <- paste(colnames(bollinger_2), "2", sep = "_")
#
#
bollinger_3 <- BBands(df[ ,c("high", "low", "close")], n = 20,
                      sd = 1)
colnames(bollinger_3) <- paste(colnames(bollinger_3), "3", sep = "_")
#
#
bollinger_4 <- BBands(df[ ,c("high", "low", "close")], n = 20,
                      sd = 2)
colnames(bollinger_4) <- paste(colnames(bollinger_4), "4", sep = "_")


# df <- cbind(df, bollinger_1, bollinger_2, bollinger_3, bollinger_4)
df <- cbind(df, bollinger_4)

# Stohastic default
stochOSC <- stoch(df[, c("high", "low", "close")])
df <- cbind(df, stochOSC)

df$williams <- WPR(df[, c("high", "low", "close")], n = 14)



# ADX
ADX_1 <- ADX(df[,c("high","low","close")], n = 5)
colnames(ADX_1) <- paste(colnames(ADX_1), "1", sep = "_")

ADX_2 <- ADX(df[,c("high","low","close")], n = 10)
colnames(ADX_2) <- paste(colnames(ADX_2), "2", sep = "_")

ADX_3 <- ADX(df[,c("high","low","close")], n = 14)
colnames(ADX_3) <- paste(colnames(ADX_3), "3", sep = "_")

ADX_4 <- ADX(df[,c("high","low","close")], n = 20)
colnames(ADX_4) <- paste(colnames(ADX_4), "4", sep = "_")
df <- cbind(df, ADX_1, ADX_2, ADX_3, ADX_4)
# df <- cbind(df, ADX_3)

# Aroon

AROON_1 <- aroon(df[, c("high", "low")], n = 5)
colnames(AROON_1) <- paste(colnames(AROON_1), "1", sep = "_")

AROON_2 <- aroon(df[, c("high", "low")], n = 10)
colnames(AROON_2) <- paste(colnames(AROON_2), "2", sep = "_")

AROON_3 <- aroon(df[, c("high", "low")], n = 18)
colnames(AROON_3) <- paste(colnames(AROON_3), "3", sep = "_")

AROON_4 <- aroon(df[, c("high", "low")], n = 25)
colnames(AROON_4) <- paste(colnames(AROON_4), "4", sep = "_")

df <- cbind(df, AROON_1, AROON_2, AROON_3, AROON_4)
# 
df$ch_vol_1 <- chaikinVolatility(df[,c("high","low")], n = 5)
df$ch_vol_2 <- chaikinVolatility(df[,c("high","low")], n = 10)
df$ch_vol_3 <- chaikinVolatility(df[,c("high","low")], n = 15)
df$ch_vol_4 <- chaikinVolatility(df[,c("high","low")], n = 20)

df$ch_ad <- chaikinAD(df[, c("high","low","close")], df[,"volume"])

df$cci_1 <- CCI(df[, c("high","low","close")], n = 5)
df$cci_2 <- CCI(df[, c("high","low","close")], n = 10)
df$cci_3 <- CCI(df[, c("high","low","close")], n = 15)
df$cci_4 <- CCI(df[, c("high","low","close")], n = 20)

atr_1 <- ATR(df[, c("high","low","close")], n=5)
colnames(atr_1) <- paste(colnames(atr_1), "1", sep = "_")

atr_2 <- ATR(df[, c("high","low","close")], n=10)
colnames(atr_2) <- paste(colnames(atr_2), "2", sep = "_")

atr_3 <- ATR(df[, c("high","low","close")], n=14)
colnames(atr_3) <- paste(colnames(atr_3), "3", sep = "_")

atr_4 <- ATR(df[, c("high","low","close")], n=20)
colnames(atr_4) <- paste(colnames(atr_4), "4", sep = "_")

df <- cbind(df, atr_1, atr_2, atr_3, atr_4)
# df <- cbind(df, atr_3)

df$clv <- CLV(df[,c("high","low","close")])

df$cmf <- CMF(df[,c("high","low","close")], df[,c("volume")])

df$cmo <- CMO(df[,"close"])

df$hma_1 <- HMA(df[,"close"], n =10)
df$hma_2 <- HMA(df[,"close"], n =30)
df$hma_3 <- HMA(df[,"close"], n =70)
df$hma_4 <- HMA(df[,"close"], n =120)
df$hma_5 <- HMA(df[,"close"], n =200)


df$priceDPO <- DPO(df[,"close"])

df$volumeDPO <- DPO(df[,"volume"])
ohlc <- df[,c("open","high","low","close")]
df$vClose <- volatility(ohlc, calc="close")
df$vClose0 <- volatility(ohlc, calc="close", mean0=TRUE)
df$vGK <- volatility(ohlc, calc="garman")

# kst <- KST(df[,"close"])

# pbands.close <- PBands( df[,"close"] )
df$sar <- SAR(df[,c("high","low")])

tdi <- TDI(df[,"close"], n=30)
df <- cbind(df, tdi)

df$vhf.close <- VHF(df[,"close"])





# df$pos <- as.factor(df$pos)

# Define datasets
# train_data <- df[full_date_time <= "2020-10-23 05:00:00 CET"]
# test_data <- df[full_date_time >= "2020-11-07 13:00:00 CET"]
# df$pos <- as.factor(df$pos)
# df$id <- as.factor(df$id)
# train_data <- df[full_date_time < "2020-08-20 00:00:00", ]
# test_data <- df[full_date_time >= "2020-08-20 00:00:00", ]

train_data <- df[1:45000, ]
test_data <- df[45001:nrow(df), ]

# train_data <- df[full_date_time <= "2020-11-18 05:00:00 CET"]
# test_data <- df[full_date_time > "2020-11-18 05:00:00 CET"]

predictors <- colnames(df)[!colnames(df) %in% c("interval", "full_date_time", "prediction",
                                                "movement", "returns","positive", "groups", "pos", "act",
                                                "profits_opt", "id", "UPDOWN", "enter", "profits")]
vars <- colnames(df)[!colnames(df) %in% c("interval", "full_date_time", "prediction",
                                          "movement", "positive", "groups", "pos", "close", "act", "returns",
                                          "enter_opt","candle_type", "candle_type", "enter", "profits",
                                          "profits_opt", "id", "UPDOWN")]
fmla <- as.formula(paste("UPDOWN ~ ", paste(predictors, collapse= "+")))

predictors <- c("UPDOWN", predictors)
train_data <- train_data[, ..predictors]
# train_data <- cbind(train_data[,c(1, 2)], scale(train_data[,-c(1, 2)]))
# View(train_data)
# 10.87
# Fit a classifier
model <- randomForest(fmla,
                      data = na.omit(train_data), 
                      importance = T,
                      ntree = 500, do.trace = T, mtry = 50)
DOWN 14233  7237   0.3370750
UP    6751 16567   0.2895188
# model <- svm(fmla,
#                       data = na.omit(train_data))


# df1 <- na.omit(train_data)
# x <- df1[, -1]
# y <- df1$UPDOWN
# 
# bestmtry <- tuneRF(x = x, y = y, stepFactor=0.1, improve=1e-8, ntreeTry=500, mtryStart=50)
# 
# metric <- "Accuracy"
# control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
# tunegrid <- expand.grid(.mtry=c(20:25))
# rf_gridsearch <- train(fmla, data=na.omit(train_data), method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
# print(rf_gridsearch)
# plot(rf_gridsearch)


# library(mlbench)
# library(randomForest)
# 
# y = y
# X = x
# 
# nvar = ncol(X)
# nrep = 1
# rf.list = lapply(1:nvar,function(i.mtry) {
#   oob.errs = replicate(nrep,{
#     oob.err = tail(randomForest(X,y,mtry=i.mtry,ntree=200, do.trace = T)$err.rate[,1],1)})
# })
# plot(replicate(nrep,1:nvar),do.call(rbind,rf.list),col="#12345678",
#      xlab="mtry",ylab="oob.err",main="tuning mtry by oob.err")
# rep.mean = sapply(rf.list,mean)
# rep.sd = sapply(rf.list,sd)
# points(1:nvar,rep.mean,type="l",col=3)
# points(1:nvar,rep.mean+rep.sd,type="l",col=2)
# points(1:nvar,rep.mean-rep.sd,type="l",col=2)


plot(model)
View(importance(model))
importance(model, type = 1)
# imp_df <- as.data.frame(?importance(model))
# imp_df$IncNodePurity <- round(imp_df$IncNodePurity, 2)
# imp_df$`%IncMSE` <- round(imp_df$`%IncMSE`, 2)
# 
# predictors1 <- rownames(imp_df)[which(imp_df$`%IncMSE` >= 10)]
# 
# fmla <- as.formula(paste("prediction ~ ", paste(predictors1, collapse= "+")))
# model <- randomForest(fmla,
#                       data = na.omit(train_data), importance = T, ntree = 500)
# 
# 

fit1 <- predict(model, test_data)


eval1 <- cbind(test_data, fit1)

eval1 <- eval1[, -..vars]
eval1 <- na.omit(eval1)
# eval1$enter <- ifelse(eval1$fit1 == "UP", 1, NA)
# 
# eval1$profits_fit <- NA
# eval1$profits_fit <-  eval1$returns*eval1$enter
# sum(eval1$profits_fit, na.rm =T)



eval1$enter_fit[eval1$fit1 == "UP"] <- 1
eval1$enter_fit[eval1$fit1 == "DOWN"] <- -1

eval1$profits_fit <- NA
eval1$profits_fit <-  eval1$returns * eval1$enter_fit
sum(eval1$profits_fit, na.rm = T)
sum(eval1$profits_fit[eval1$fit1 =="UP"])
sum(eval1$profits_fit[eval1$fit1 =="DOWN"])


sum(eval1$profits_fit[eval1$fit1 =="UP"]) / sum(eval1$profits[eval1$UPDOWN =="UP"])
sum(eval1$profits_fit[eval1$fit1 =="DOWN"]) / sum(eval1$profits[eval1$UPDOWN =="DOWN"])
sum(eval1$profits_fit) / sum(eval1$profits)

table(eval1$enter == eval1$enter_fit)/sum(table(eval1$enter == eval1$enter_fit))
table(eval1$enter_fit, eval1$enter)/sum(table(eval1$enter_fit, eval1$enter))


P <- length(eval1$enter[eval1$enter == 1])
N <- length(eval1$enter[eval1$enter == -1])

TP <- length(eval1$enter_fit[eval1$enter_fit == 1 & eval1$enter == 1])
TN <- length(eval1$enter_fit[eval1$enter_fit == -1 & eval1$enter == -1])
TPR <- TP/P
TNR <- TN/N
table(eval1$enter_fit, eval1$enter)
