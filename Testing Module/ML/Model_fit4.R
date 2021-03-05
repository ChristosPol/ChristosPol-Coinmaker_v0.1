# Idea is to build a robust model selecting the best parameters
# Use this model to predict t+1 of the time series
# Dataset
df <- klines[[1]]

df$returns <- c(diff(df$close), 0)

smoothingSpline = smooth.spline(df[, close] ~ as.numeric(rownames(df)) , spar = 0.4)
df[, spline := predict(smoothingSpline)$y]
df[, deriv := predict(smoothingSpline, deriv = 1)$y]

df[, spline_pred := shift(predict(smoothingSpline)$y, type ="lead")]
df[, deriv_pred := shift(predict(smoothingSpline, deriv = 1)$y, type ="lead")]


p1 <- ggplot(data = df, aes(x = full_date_time, y = close))+
  geom_line() + 
  geom_line(aes(x = full_date_time, y = spline, col = "red")) + theme(legend.position = "none")

p2 <- ggplot(data = df, aes(x = full_date_time, y = deriv))+
  geom_line()+ geom_hline(yintercept=0, col ="red")

grid.arrange(p1, p2)

df$pos <- NA
df$pos[df$deriv_pred > 0 ] <- "enter"
df$pos[is.na(df$pos)] <- "no_action"
df$profits_opt <- NA
# 
df$profits_opt[df$pos =="enter"] <-  df$returns[df$pos =="enter"]
# View(df)
# View(df)
# 
sum(df$profits_opt, na.rm = T)

ggplot(data = df, aes(x = full_date_time, y = close, colour=(pos == "enter")))+
  geom_line(aes(group=1))+ theme(legend.position = "none")

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
# df$EMA_1 <- EMA(df$close, n = 5)
# df$EMA_2 <- EMA(df$close, n = 7)
# df$EMA_3 <- EMA(df$close, n = 10)
# df$EMA_4 <- EMA(df$close, n = 15)
df$EMA_5 <- EMA(df$close, n = 21)
# df$EMA_6 <- EMA(df$close, n = 50)
df$EMA_7 <- EMA(df$close, n = 100)
# df$EMA_8 <- EMA(df$close, n = 150)
df$EMA_9 <- EMA(df$close, n = 200)
# df$EMA_10 <- EMA(df$close, n = 250)
# df$EMA_11 <- EMA(df$close, n = 300)
# df$EMA_12 <- EMA(df$close, n = 350)
# df$EMA_13 <- EMA(df$close, n = 400)
# df$EMA_14 <- EMA(df$close, n = 450)
# df$EMA_15 <- EMA(df$close, n = 500)
# df$EMA_16 <- EMA(df$close, n = 600)


df$roc_1 <- momentum(df$close, n = 1)
# df$roc_2 <- momentum(df$close, n = 3)
# df$roc_3 <- momentum(df$close, n = 9)
# df$roc_4 <- momentum(df$close, n = 15)
# df$roc_5 <- momentum(df$close, n = 20)
# df$roc_6 <- momentum(df$close, n = 30)
# df$roc_7 <- momentum(df$close, n = 50)
# df$roc_8 <- momentum(df$close, n = 100)
# df$roc_9 <- momentum(df$close, n = 120)
# df$roc_10 <- momentum(df$close, n = 150)
# df$roc_11 <- momentum(df$close, n = 200)
# 
# 
# 
# # Candle description
df$HL <- df$high - df$low
df$OC <- df$open - df$close
# 
# # RSI
df$RSI_1 <- RSI(df$close, n = 5)
# df$RSI_2 <- RSI(df$close, n = 10)
df$RSI_3 <- RSI(df$close, n = 14)
# df$RSI_4 <- RSI(df$close, n = 20)
# df$RSI_5 <- RSI(df$close, n = 25)
# df$RSI_6 <- RSI(df$close, n = 1)
# df$RSI_7 <- RSI(df$close, n = 2)
# df$RSI_8 <- RSI(df$close, n = 7)
# df$RSI_9 <- RSI(df$close, n = 40)
# df$RSI_10 <- RSI(df$close, n = 50)
# df$RSI_11 <- RSI(df$close, n = 70)
# 
# # Standard deviation
# df$sd_1 <- rollapplyr(df$close, 5, sd, fill = NA)
# df$sd_2 <- rollapplyr(df$close, 10, sd, fill = NA)
# df$sd_3 <- rollapplyr(df$close, 14, sd, fill = NA)
# df$sd_4 <- rollapplyr(df$close, 20, sd, fill = NA)
# df$sd_5 <- rollapplyr(df$close, 50, sd, fill = NA)
# df$sd_6 <- rollapplyr(df$close, 75, sd, fill = NA)
# df$sd_7 <- rollapplyr(df$close, 100, sd, fill = NA)
# df$sd_8 <- rollapplyr(df$close, 200, sd, fill = NA)
# 
# 
# 
# # MACD default
# macd <- MACD(df[, "close"])
# df <- cbind(df,macd)
# 
# # MFI
# df$mfi_1 <- MFI(df[, c("high", "low", "close")], df[, "volume"],
#                 n = 5)
# df$mfi_2 <- MFI(df[, c("high", "low", "close")], df[, "volume"],
#                 n = 10)
df$mfi_3 <- MFI(df[, c("high", "low", "close")], df[, "volume"],
                n = 14)
# df$mfi_4 <- MFI(df[, c("high", "low", "close")], df[, "volume"],
#                 n = 20)
# 
# # OBV default
df$obv <- OBV(df[, "close"], df[, "volume"])
df$obv_ema1 <- SMA(df$obv, n = 10)
df$obv_ema2 <- SMA(df$obv, n = 25)
df$obv_ema3 <- SMA(df$obv, n = 75)
df$obv_ema4 <- SMA(df$obv, n = 120)
df$obv_ema5 <- SMA(df$obv, n = 175)
# 
# 
# # Bollinger
# bollinger_1 <- BBands(df[ ,c("high", "low", "close")], n = 10,
#                       sd = 1)
# colnames(bollinger_1) <- paste(colnames(bollinger_1), "1", sep = "_")
# #
# #
# bollinger_2 <- BBands(df[ ,c("high", "low", "close")], n = 10,
#                       sd = 2)
# colnames(bollinger_2) <- paste(colnames(bollinger_2), "2", sep = "_")
# #
# #
# bollinger_3 <- BBands(df[ ,c("high", "low", "close")], n = 20,
#                       sd = 1)
# colnames(bollinger_3) <- paste(colnames(bollinger_3), "3", sep = "_")
# #
# #
# bollinger_4 <- BBands(df[ ,c("high", "low", "close")], n = 20,
#                       sd = 2)
# colnames(bollinger_4) <- paste(colnames(bollinger_4), "4", sep = "_")
# 
# 
# # df <- cbind(df, bollinger_1, bollinger_2, bollinger_3, bollinger_4)
# df <- cbind(df, bollinger_4)
# 
# # Stohastic default
# stochOSC <- stoch(df[, c("high", "low", "close")])
# df <- cbind(df, stochOSC)
# 
# df$williams_1 <- WPR(df[, c("high", "low", "close")], n = 5)
# df$williams_2 <- WPR(df[, c("high", "low", "close")], n = 10)
# df$williams_3 <- WPR(df[, c("high", "low", "close")], n = 14)
# df$williams_4 <- WPR(df[, c("high", "low", "close")], n = 20)
# 
# 
# 
# # ADX
# ADX_1 <- ADX(df[,c("high","low","close")], n = 5)
# colnames(ADX_1) <- paste(colnames(ADX_1), "1", sep = "_")
# 
# ADX_2 <- ADX(df[,c("high","low","close")], n = 10)
# colnames(ADX_2) <- paste(colnames(ADX_2), "2", sep = "_")
# 
# ADX_3 <- ADX(df[,c("high","low","close")], n = 14)
# colnames(ADX_3) <- paste(colnames(ADX_3), "3", sep = "_")
# 
# ADX_4 <- ADX(df[,c("high","low","close")], n = 20)
# colnames(ADX_4) <- paste(colnames(ADX_4), "4", sep = "_")
# df <- cbind(df, ADX_1, ADX_2, ADX_3, ADX_4)
# # df <- cbind(df, ADX_3)
# 
# # Aroon
# 
# AROON_1 <- aroon(df[, c("high", "low")], n = 5)
# colnames(AROON_1) <- paste(colnames(AROON_1), "1", sep = "_")
# 
# AROON_2 <- aroon(df[, c("high", "low")], n = 10)
# colnames(AROON_2) <- paste(colnames(AROON_2), "2", sep = "_")
# 
# AROON_3 <- aroon(df[, c("high", "low")], n = 18)
# colnames(AROON_3) <- paste(colnames(AROON_3), "3", sep = "_")
# 
# AROON_4 <- aroon(df[, c("high", "low")], n = 25)
# colnames(AROON_4) <- paste(colnames(AROON_4), "4", sep = "_")
# 
# df <- cbind(df, AROON_1, AROON_2, AROON_3, AROON_4)
# # 
# df$ch_vol_1 <- chaikinVolatility(df[,c("high","low")], n = 5)
# df$ch_vol_2 <- chaikinVolatility(df[,c("high","low")], n = 10)
# df$ch_vol_3 <- chaikinVolatility(df[,c("high","low")], n = 15)
# df$ch_vol_4 <- chaikinVolatility(df[,c("high","low")], n = 20)
# 
# df$ch_ad <- chaikinAD(df[, c("high","low","close")], df[,"volume"])
# 
# df$cci_1 <- CCI(df[, c("high","low","close")], n = 5)
# df$cci_2 <- CCI(df[, c("high","low","close")], n = 10)
# df$cci_3 <- CCI(df[, c("high","low","close")], n = 15)
# df$cci_4 <- CCI(df[, c("high","low","close")], n = 20)
# 
# atr_1 <- ATR(df[, c("high","low","close")], n=5)
# colnames(atr_1) <- paste(colnames(atr_1), "1", sep = "_")
# 
# atr_2 <- ATR(df[, c("high","low","close")], n=10)
# colnames(atr_2) <- paste(colnames(atr_2), "2", sep = "_")
# 
# atr_3 <- ATR(df[, c("high","low","close")], n=14)
# colnames(atr_3) <- paste(colnames(atr_3), "3", sep = "_")
# 
# atr_4 <- ATR(df[, c("high","low","close")], n=20)
# colnames(atr_4) <- paste(colnames(atr_4), "4", sep = "_")
# 
# df <- cbind(df, atr_1, atr_2, atr_3, atr_4)
# # df <- cbind(df, atr_3)
# 
# df$clv <- CLV(df[,c("high","low","close")])
# 
# df$cmf <- CMF(df[,c("high","low","close")], df[,c("volume")])
# 
# df$cmo <- CMO(df[,"close"])
# 
# df$hma_1 <- HMA(df[,"close"], n =10)
# df$hma_2 <- HMA(df[,"close"], n =30)
# df$hma_3 <- HMA(df[,"close"], n =70)
# df$hma_4 <- HMA(df[,"close"], n =120)
# df$hma_5 <- HMA(df[,"close"], n =200)
# df$hma_6 <- HMA(df[,"close"], n =250)
# df$hma_7 <- HMA(df[,"close"], n =300)
# df$hma_8 <- HMA(df[,"close"], n =400)
# 
# 
# # df$priceDPO <- DPO(df[,"close"])
# # df$volumeDPO <- DPO(df[,"volume"])
# 
# ohlc <- df[,c("open","high","low","close")]
# df$vClose <- volatility(ohlc, calc="close")
# df$vClose0 <- volatility(ohlc, calc="close", mean0=TRUE)
# df$vGK <- volatility(ohlc, calc="garman")
# 
# # kst <- KST(df[,"close"])
# 
# # pbands.close <- PBands( df[,"close"] )
# df$sar <- SAR(df[,c("high","low")])
# 
# tdi <- TDI(df[,"close"], n=30)
# df <- cbind(df, tdi)
# 
# df$vhf.close <- VHF(df[,"close"])
# 
df$weekday <- wday(df$full_date_time, getOption("lubridate.week.start", 1))
df$monthday <- mday(df$full_date_time)
df$yearday <- yday(df$full_date_time)
# 




# Define datasets
# train_data <- df[full_date_time <= "2020-10-23 05:00:00 CET"]
# test_data <- df[full_date_time >= "2020-11-07 13:00:00 CET"]
df$pos <- as.factor(df$pos)
df$id <- as.factor(df$id)
# train_data <- df[full_date_time < "2020-06-01 00:00:00", ]
# test_data <- df[full_date_time >= "2020-06-01 00:00:00", ]
dim(df)
train_data <- df[1:45000, ]
test_data <- df[45001:nrow(df), ]

# train_data <- df[full_date_time <= "2020-11-18 05:00:00 CET"]
# test_data <- df[full_date_time > "2020-11-18 05:00:00 CET"]

predictors <- colnames(df)[!colnames(df) %in% c("interval", "full_date_time", "prediction","spline_pred", "deriv_pred",
                                                "movement", "returns","positive", "groups", "pos", "act",
                                                "profits_opt", "id", "UPDOWN")]
vars <- colnames(df)[!colnames(df) %in% c("interval", "full_date_time", "prediction",
                                          "movement", "positive", "groups", "pos", "close", "act", "returns",
                                          "enter_opt","candle_type", "candle_type",
                                          "profits_opt", "id")]
fmla <- as.formula(paste("pos ~ ", paste(predictors, collapse= "+")))

predictors <- c("pos", predictors)
train_data <- train_data[, ..predictors]

# Fit a classifier
model <- randomForest(fmla,
                      data = na.omit(train_data), 
                      importance = T,
                      ntree = 2000, do.trace = T)

# model <- svm(fmla,
#                       data = na.omit(train_data))
# plot(model)

# summary(model)
# names(model)
# model$confusion

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
# eval1$movement_fit <- round(((eval1$fit1 - eval1$close) / eval1$fit1 )*100, 3)
# eval1$enter_fit[eval1$movement_fit > 1] <- 1
# eval1$profits_fit <- eval1$enter_fit * eval1$returns
# View(eval1)
# 16.06002 big one
# plot(eval1$prediction, eval1$fit1)
eval1$profits_fit <- NA
eval1$profits_fit[eval1$fit1 == "enter"] <-  eval1$returns[eval1$fit1 =="enter"]
sum(eval1$profits_fit, na.rm = T)/sum(eval1$profits_opt, na.rm = T)
sum(eval1$profits_fit, na.rm = T)


act <- ggplot(data = eval1, aes(x = full_date_time, y = close, colour=(pos == "enter")))+
  geom_line(aes(group=1)) #+ geom_line(data = eval1, aes(x = full_date_time, y = spline, col ="blue"))

fitted <- ggplot(data = eval1, aes(x = full_date_time, y = close, colour=(fit1 == "enter")))+
  geom_line(aes(group=1))

library(gridExtra)
grid.arrange(act, fitted)

# plot(density(eval1$profits_fit[!is.na(eval1$profits_fit)]))
# mean(eval1$profits_fit[!is.na(eval1$profits_fit)])
[1] 449.4

0.0154026





sum(eval1$profits_fit, na.rm = T)/sum(eval1$profits_opt, na.rm = T)
# simple model
0.02142171



View(eval1)

eval1$enter_opt[eval1$pos =="enter"] <- 1

eval1$profits <- eval1$enter* eval1$returns
eval1$profits_opt <- eval1$enter_opt* eval1$returns
View(eval1)
0.01583262
0.01894105
0.01185712
0.02572584
View(eval1)
eval1 <- eval1[!is.na(prediction), ]
table(eval1$fit1 == eval1$pos)
View(eval1[, c("fit1", "pos")])

metrics <- data.frame(MAPE = mape(eval1$prediction, eval1$fit1),
                      MSE = mean((eval1$prediction - eval1$fit1)^2),
                      MAE = mae(eval1$prediction, eval1$fit1),
                      RMSE = rmse(eval1$prediction, eval1$fit1),
                      RSQ = cor(eval1$prediction, eval1$fit1)^2)

plot(eval1$prediction, eval1$fit1)
abline(0,1, col ="red")
varImpPlot(model)
saveRDS(parameter_results, file = paste0("/media/chris/DATA/Documents/Bot_Trading/ML_parameters",
                                         paste0("/" ,pair,"_", intervals,"_", Sys.time(), ".rds")))
