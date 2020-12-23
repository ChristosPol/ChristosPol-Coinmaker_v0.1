# Idea is to build a robust model selecting the best parameters
# Use this model to predict t+1 of the time series
# Dataset
df <- klines[[1]]

# Create prediction variable
df$prediction <- shift(df$close, type = "lead")
df$movement <- round(((df$prediction - df$close) / df$prediction )*100, 3)
df$returns <- c(diff(df$close), 0)



df$positive <-  ifelse(df$movement > 0, "positive", "negative")

lens <- rle(df$positive)$lengths
reps <- 1:length(lens)
df$groups <- as.character(rep(reps, lens))

df <- as.data.table(df %>% group_by(groups) %>% mutate(act = sum(movement)) %>% ungroup())
df[act > 1, pos := "enter"]

df$pos[is.na(df$pos)] <- "no_action"


df$profits_opt <- NA
df$profits_opt[df$pos =="enter"] <-  df$returns[df$pos =="enter"]

sum(df$profits_opt, na.rm = T)

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
obv <- OBV(df[, "close"], df[, "volume"])
df$obv <- EMA(obv, n = 25)

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


df <- cbind(df, bollinger_1, bollinger_2, bollinger_3, bollinger_4)

# Stohastic default
stochOSC <- stoch(df[, c("high", "low", "close")])
df <- cbind(df, stochOSC)

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

# df$pos <- as.factor(df$pos)

# Define datasets
# train_data <- df[full_date_time <= "2020-10-23 05:00:00 CET"]
# test_data <- df[full_date_time >= "2020-11-07 13:00:00 CET"]
df$pos <- as.factor(df$pos)
train_data <- df[full_date_time < "2020-08-20 00:00:00", ]
test_data <- df[full_date_time >= "2020-08-20 00:00:00", ]


# train_data <- df[full_date_time <= "2020-11-18 05:00:00 CET"]
# test_data <- df[full_date_time > "2020-11-18 05:00:00 CET"]

predictors <- colnames(df)[!colnames(df) %in% c("interval", "full_date_time", "prediction",
                                                "movement", "returns","positive", "groups", "pos", "act",
                                                "profits_opt")]
vars <- colnames(df)[!colnames(df) %in% c("interval", "full_date_time", "prediction",
                                          "movement", "positive", "groups", "pos", "close", "act", "returns",
                                          "enter_opt","candle_type", "candle_type",
                                          "profits_opt")]
fmla <- as.formula(paste("prediction ~ ", paste(predictors, collapse= "+")))

predictors <- c("prediction", predictors)
train_data <- train_data[, ..predictors]

# Fit a classifier
model <- randomForest(fmla,
                        data = na.omit(train_data), 
                      importance = T,
                      ntree = 5000, do.trace = T)
summary(model)
names(model)
model$confusion

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
eval1$profits_fit <- NA
eval1$profits_fit[eval1$fit1 == "enter"] <-  eval1$returns[eval1$fit1 =="enter"]
sum(eval1$profits_fit, na.rm = T)/sum(eval1$profits_opt, na.rm = T)




View(eval1)

View(eval1)

plot(density(eval1$profits_fit[!is.na(eval1$profits_fit)]))
mean(eval1$profits_fit[!is.na(eval1$profits_fit)])



eval1$movement_fit <- round(((eval1$fit1 - eval1$close) / eval1$fit1 )*100, 3)
eval1$enter_fit[eval1$movement_fit > 3] <- 1

eval1$profits_fit <- eval1$enter_fit * eval1$returns 



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
