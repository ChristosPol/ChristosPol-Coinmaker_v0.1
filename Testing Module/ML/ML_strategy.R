# Training model dates
# train_data <- fut[full_date_time <= "2020-11-18 05:00:00 CET"]
# test_data <- fut[full_date_time > "2020-11-18 05:00:00 CET"]
paraller_exec <- FALSE
initial_budget <- 200
# stoploss_ult <- 0.03
# takeprofit <- 0.05
fut <- klines[[1]]

# select period of data 


placeholder_fut <- fut[1:5000, ]
smoothingSpline = smooth.spline(placeholder_fut[, close] ~ as.numeric(rownames(placeholder_fut)) , spar = 0.3)
placeholder_fut[, spline := predict(smoothingSpline)$y]
placeholder_fut[, deriv := predict(smoothingSpline, deriv = 1)$y]

# Add candle type
placeholder_fut$candle_type <- NA

placeholder_fut$candle_type[placeholder_fut$close > placeholder_fut$open] <- "bullish"
placeholder_fut$candle_type[placeholder_fut$close < placeholder_fut$open] <- "bearish"
placeholder_fut$candle_type[is.na(placeholder_fut$candle_type)] <- "neutral"
placeholder_fut$candle_type <- as.factor(placeholder_fut$candle_type)


placeholder_fut[, EMA_7 := SMA(close, n = 100)]
placeholder_fut$EMA_5 <- EMA(placeholder_fut$close, n = 21)
placeholder_fut$EMA_9 <- EMA(placeholder_fut$close, n = 200)
placeholder_fut$roc_1 <- momentum(placeholder_fut$close, n = 1)
placeholder_fut$HL <- placeholder_fut$high - placeholder_fut$low
placeholder_fut$OC <- placeholder_fut$open - placeholder_fut$close
placeholder_fut$RSI_1 <- RSI(placeholder_fut$close, n = 5)
placeholder_fut$RSI_3 <- RSI(placeholder_fut$close, n = 14)
placeholder_fut$mfi_3 <- MFI(placeholder_fut[, c("high", "low", "close")], placeholder_fut[, "volume"],
                n = 14)
placeholder_fut$obv <- OBV(placeholder_fut[, "close"], placeholder_fut[, "volume"])
placeholder_fut$obv_ema1 <- SMA(placeholder_fut$obv, n = 10)
placeholder_fut$obv_ema2 <- SMA(placeholder_fut$obv, n = 25)
placeholder_fut$obv_ema3 <- SMA(placeholder_fut$obv, n = 75)
placeholder_fut$obv_ema4 <- SMA(placeholder_fut$obv, n = 120)
placeholder_fut$obv_ema5 <- SMA(placeholder_fut$obv, n = 175)

placeholder_fut$weekday <- wday(placeholder_fut$full_date_time, getOption("lubridate.week.start", 1))
placeholder_fut$monthday <- mday(placeholder_fut$full_date_time)
placeholder_fut$yearday <- yday(placeholder_fut$full_date_time)


evaluating_fut <- fut[5001:nrow(fut), ]
# placeholder_fut <- fut[60000:61000, ]

# placeholder_fut <- placeholder_fut[57000:58000, ]


# Train and test datasets
placeholder_fut[, c("action",
                   "Units",
                   "Price",
                    "id") := list(NA, NA, NA, NA) ]
cols <- colnames(placeholder_fut)[!colnames(placeholder_fut) %in% colnames(evaluating_fut)]

evaluating_fut[, eval(cols) := list(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                    NA, NA, NA, NA, NA)]

# Going intro the loop for test data -----------------------------------------
for (i in 1:nrow(test_data)){
  
  fut <- rbind(placeholder_fut, evaluating_fut[i, ])
  
  smoothingSpline = smooth.spline(fut[, close] ~ as.numeric(rownames(fut)) , spar = 0.3)
  fut[, spline := predict(smoothingSpline)$y]
  fut[, deriv := predict(smoothingSpline, deriv = 1)$y]
  
  
  # Add candle type
  fut$candle_type <- NA
  
  fut$candle_type[fut$close > fut$open] <- "bullish"
  fut$candle_type[fut$close < fut$open] <- "bearish"
  fut$candle_type[is.na(fut$candle_type)] <- "neutral"
  fut$candle_type <- as.factor(fut$candle_type)
  
  
  fut[, EMA_7 := SMA(close, n = 100)]
  fut$EMA_5 <- EMA(fut$close, n = 21)
  fut$EMA_9 <- EMA(fut$close, n = 200)
  fut$roc_1 <- momentum(fut$close, n = 1)
  fut$HL <- fut$high - fut$low
  fut$OC <- fut$open - fut$close
  fut$RSI_1 <- RSI(fut$close, n = 5)
  fut$RSI_3 <- RSI(fut$close, n = 14)
  fut$mfi_3 <- MFI(fut[, c("high", "low", "close")], fut[, "volume"],
                               n = 14)
  fut$obv <- OBV(fut[, "close"], fut[, "volume"])
  fut$obv_ema1 <- SMA(fut$obv, n = 10)
  fut$obv_ema2 <- SMA(fut$obv, n = 25)
  fut$obv_ema3 <- SMA(fut$obv, n = 75)
  fut$obv_ema4 <- SMA(fut$obv, n = 120)
  fut$obv_ema5 <- SMA(fut$obv, n = 175)
  
  fut$weekday <- wday(fut$full_date_time, getOption("lubridate.week.start", 1))
  fut$monthday <- mday(fut$full_date_time)
  fut$yearday <- yday(fut$full_date_time)
  
  
  ######
  
  fit1 <- predict(model, newdata = tail(fut, 1))
  
  # Deciding upon action -----------------------------------------------------
  # Buy condition
  if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
       fit1 == "enter" ) {
    
    fut$action[nrow(fut)] <- "buy"
    fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
    fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)] - (0.0026 * fut$Units[nrow(fut)] * fut$close[nrow(fut)])
    # fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
    fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
    
    # Sell condition
  } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") & (
    fit1 == "no_action"     )) {
    
    fut$action[nrow(fut)] <- "sell"
    fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
    fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]  - (0.0026 * fut$Units[nrow(fut)] * fut$close[nrow(fut)])
    # fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
    fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
    initial_budget <- fut$Price[nrow(fut)]
    
    # Keep condition
  } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   &
              (  fit1 == "enter"  )) {
    
    fut$action[nrow(fut)] <- "keep"
    fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
    fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
    
  } else {
    
    fut$action[nrow(fut)] <- "no action"
    
  }
  
  placeholder_fut <- fut
  # print(i)
  
  print(i)
}

myresult <- placeholder_fut
myresult <- myresult[24001:nrow(myresult)]
View(myresult)
ggplot(data = myresult, aes(x = full_date_time, y = close, colour=(action %in% c("buy", "keep"))))+
  geom_line(aes(group=1))+ theme(legend.position = "none")


initial_budget <- 200
calculate_profits(myresult)
View(myresult)
View(myresult[, -..vars])
cols <- c("interval", "high", "low", "open", "close", "volume",
  "full_date_time", "prediction", "UPDOWN", "UPDOWN_PRE",
"action", "Units","Price","id")
View(myresult[, ..cols])
# ------------------------------------------------------------------------------


segment_buy <- myresult[action %in% "buy", ]
segment_sell <- myresult[action %in% "sell", ]


p1 <- ggplot(data= myresult, aes(x=full_date_time, y=close)) +
  geom_line(alpha = 0.5) +
  geom_point(data = segment_buy, aes(x=full_date_time, y=close),
             color ="green", size = 2) +
  geom_point(data = segment_sell, aes(x=full_date_time, y=close),
             color ="red", size = 2);p1
# geom_line(aes(x=full_date_time, y=Mean), color ="green", size = 0.2)+
# geom_line(aes(x=full_date_time, y=Upper), color ="red", size = 0.2)+
# geom_line(aes(x=full_date_time, y=Lower), color ="red", size = 0.2);p1
View(myresult)

