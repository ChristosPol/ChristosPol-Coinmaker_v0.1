library(randomForest)
library(e1071)
# initial_budget <- 200
# plot.it <- F

df <- klines[[1]]
# Need to calculate predictors (add more)---------------------------------------

# EMAs
df$EMA_7 <- EMA(df$close, n = 7)
df$EMA_14 <- EMA(df$close, n = 14)
df$EMA_21 <- EMA(df$close, n = 21)
df$EMA_100 <- EMA(df$close, n = 100)

# RSI
df$RSI <- RSI(df$close, n = 14)

df$HL <- df$high - df$low
df$OC <- df$open - df$close

df$sd_14 <- rollapplyr(df$close, 14, sd, fill = NA)
# # MACD
macd <- MACD(df[, "close"])
df <- cbind(df,macd)
# 
# MFI
df$mfi <- MFI(df[,c("high","low","close")], df[,"volume"])
# 
# # OBV
df$obv <- OBV(df[, "close"], df[, "volume"])
# 
bollinger <- BBands(df[,c("high","low","close")])
df <- cbind(df, bollinger)
# 
stochOSC <- stoch(df[,c("high","low","close")])
df <- cbind(df, stochOSC)


# Create UP and DOWN variable
df$returns <- c(lag(diff(df$close)), 0)

# df$UPDOWN <- as.factor(ifelse(df$returns > 0, "UP", "DOWN"))

# Create prediction variable
df$prediction <- shift(df$close, type ="lead")

# Fit the model and test it against test data ----------------------------------

# test data
test_n <- ceiling(nrow(df) / 300)
test_data <- tail(df, test_n)

# train data
train_data <- head(df, (nrow(df) - test_n))

# Fit a classifier
model1 <- randomForest(prediction ~ volume + RSI + close + EMA_7 +
                         EMA_14 + EMA_21 + EMA_100 +
                         HL + macd + signal + mfi + obv + dn + mavg +
                         up + pctB + fastK + fastD + slowD + OC + sd_14,
                       data = na.omit(train_data), importance = T, ntree = 500)

fit1 <- predict(model1, test_data)

eval1 <- cbind(test_data, fit1)
plot(eval1$prediction, eval1$fit1)
abline(a = 0, b =1)

plot(eval1$prediction, type ="b", pch =19,cex =0.8)
lines(eval1$fit1, type ="b", col ="red", pch =19,cex =0.8)
abline(v = 70)
# eval1$actions <- ifelse(eval1$fit1 == "UP", 1, 0)
# eval1$position <- eval1$action
# eval1$profits <- eval1$returns * eval1$position
# sum(eval1$profits, na.rm = T)

# table(eval1$prediction, eval1$fit)
# sum(eval1$fit1 == eval1$UPDOWN)/nrow(eval1)

# importance(model1)
# importance(model1, type =1)
varImpPlot(model1,type=2)
initial_budget <- 200

# select period of data 
candles_recent <- as.data.table(klines[[1]])
candles_recent <- candles_recent[-c(1:20000), ]
# training data here is not the traditional training set, but more of a 
# placeholder initial dataset to calculate the indicators
train_n <- ceiling(nrow(candles_recent) / 2)
train_n <- 400
train_data <- candles_recent[1:train_n, ]
test_data <- candles_recent[(train_n + 1):nrow(candles_recent), ]

# Train and test datasets
train_data[, c("prediction",
               "movement",
               "RSI",
               "EMA_7",
               "EMA_14",
               "EMA_21",
               "EMA_100",
               "HL",
               "macd",
               "signal",
               "mfi",
               "obv",
               "dn",
               "mavg",
               "up",
               "pctB",
               "fastK",
               "fastD",
               "slowD",
               "OC",
               "sd_14",
               "action",
               "Units",
               "Price",
               "id") := list(NA, NA, NA, NA, NA, NA,
                             NA, NA, NA, NA, NA, NA,
                             NA, NA, NA, NA, NA,
                             NA, NA, NA, NA, NA,
                             NA, NA, NA) ]

test_data[, c("prediction",
              "movement",
              "RSI",
              "EMA_7",
              "EMA_14",
              "EMA_21",
              "EMA_100",
              "HL",
              "macd",
              "signal",
              "mfi",
              "obv",
              "dn",
              "mavg",
              "up",
              "pctB",
              "fastK",
              "fastD",
              "slowD",
              "OC",
              "sd_14",
              "action",
              "Units",
              "Price",
              "id") := list(NA, NA, NA, NA, NA, NA,
                            NA, NA, NA, NA, NA, NA,
                            NA, NA, NA, NA, NA,
                            NA, NA, NA, NA, NA,
                            NA, NA, NA) ]

# Going intro the loop for test data -----------------------------------------
for (i in 1:nrow(test_data)){
  
  fut <- rbind(train_data, test_data[i, ])
  
  # EMAs
  fut$EMA_7 <- EMA(fut$close, n = 7)
  fut$EMA_14 <- EMA(fut$close, n = 14)
  fut$EMA_21 <- EMA(fut$close, n = 21)
  fut$EMA_100 <- EMA(fut$close, n = 100)
  
  # RSI
  fut$RSI <- RSI(fut$close, n = 14)
  
  fut$HL <- fut$high - fut$low
  fut$OC <- fut$open - fut$close
  
  fut$sd_14 <- rollapplyr(fut$close, 14, sd, fill = NA)
  # # MACD
  macd <- MACD(fut[, "close"])
  fut$macd <- macd[, 1]
  fut$signal <- macd[, 2]
  
  # 
  # MFI
  fut$mfi <- MFI(fut[,c("high","low","close")], fut[,"volume"])
  # 
  # # OBV
  fut$obv <- OBV(fut[, "close"], fut[, "volume"])
  # 
  bollinger <- BBands(fut[,c("high","low","close")])
  fut$dn <- bollinger[, 1]
  fut$mavg <- bollinger[, 2]
  fut$up <- bollinger[, 3]
  fut$pctB <- bollinger[, 4]
  
  # 
  stochOSC <- stoch(fut[,c("high","low","close")])
  fut$fastK <- stochOSC[, 1]
  fut$fastD <- stochOSC[, 2]
  fut$slowD <- stochOSC[, 3]
  
  
  
  fit1 <- predict(model1, newdata = tail(fut, 1)) 
  fut$prediction[nrow(fut)] <- fit1
  fut$movement[nrow(fut)] <- round(((fut$prediction[nrow(fut)] - fut$close[nrow(fut)]) / fut$prediction[nrow(fut)] )*100,2)
  
  train_data <- fut

  # Deciding upon action -----------------------------------------------------
  # Buy condition
  if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
       fut$movement[nrow(fut)] >= 0.50 ) {
    
    fut$action[nrow(fut)] <- "buy"
    fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
    fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)] - (0.0026 * fut$Units[nrow(fut)] * fut$close[nrow(fut)])
    # fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
    fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
    
    # Sell condition
  } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") & (
    fut$movement[nrow(fut)] <= -0.50         )) {
    
    fut$action[nrow(fut)] <- "sell"
    fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
    fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]  - (0.0026 * fut$Units[nrow(fut)] * fut$close[nrow(fut)])
    # fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
    fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
    initial_budget <- fut$Price[nrow(fut)]
    
    # Keep condition
  } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   &
              ( fut$movement[nrow(fut)] > -0.50 | fut$movement[nrow(fut)] < 0.50  )) {
    
    fut$action[nrow(fut)] <- "keep"
    fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
    fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
    
  } else {
    
    fut$action[nrow(fut)] <- "no action"
    
  }
  
  train_data <- fut
  # print(i)
 
  print(i)
}
myresult <- train_data
initial_budget <- 200
calculate_profits(myresult)
View(myresult)

eval1 <- cbind(na.omit(test_data), fit1)
View(eval1)
# eval1$actions <- ifelse(eval1$fit1 == "UP", 1, 0)
# eval1$position <- eval1$action
# eval1$profits <- eval1$returns * eval1$position
# sum(eval1$profits, na.rm = T)
table(eval1$UPDOWN, eval1$fit)
sum(eval1$fit1 == eval1$UPDOWN)/nrow(eval1)
# Try xgboost and svm

model2 <- svm(UPDOWN ~volume+RSI+close+macd+signal+mfi, data = na.omit(train_data))
fit2 <- predict(model2, na.omit(test_data)) 

eval2 <- cbind(na.omit(test_data), fit2)
table(eval2$UPDOWN, eval2$fit)

sum(eval2$fit2 == eval2$UPDOWN)/nrow(eval2)
View(eval2)

# ------------------------------------------------------------------------------
dat <- klines[[1]]
un_max <-  unique(rollmax(dat$close, 500))
un_min <-  unique(rollapplyr(dat$close, 500, min, fill = NA))


segment_buy <- dat[close %in% un_max, ]
segment_sell <- dat[close %in% un_min, ]


p1 <- ggplot(data= dat, aes(x=full_date_time, y=close)) +
  geom_line(alpha = 0.5) +
  geom_point(data = segment_buy, aes(x=full_date_time, y=close),
             color ="green", size = 2) +
  geom_point(data = segment_sell, aes(x=full_date_time, y=close),
             color ="red", size = 2);p1
# geom_line(aes(x=full_date_time, y=Mean), color ="green", size = 0.2)+
# geom_line(aes(x=full_date_time, y=Upper), color ="red", size = 0.2)+
# geom_line(aes(x=full_date_time, y=Lower), color ="red", size = 0.2);p1
