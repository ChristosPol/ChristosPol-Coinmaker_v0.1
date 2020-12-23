# Training model dates
# train_data <- df[full_date_time <= "2020-11-18 05:00:00 CET"]
# test_data <- df[full_date_time > "2020-11-18 05:00:00 CET"]
paraller_exec <- FALSE
initial_budget <- 200
stoploss_ult <- 0.03
takeprofit <- 0.05
df <- klines[[1]]

# select period of data 

# placeholder initial dataset to calculate the indicators
# df_testing <- df[full_date_time > "2020-11-18 05:00:00 CET"]

# placeholder_df <- df_testing[1:100, ]
# evaluating_df <- df_testing[101:nrow(df_testing), ]

df$prediction_actual <- shift(df$close, type = "lead")
placeholder_df <- df[19000:20000, ]

evaluating_df <- df[20001:nrow(df), ]
# placeholder_df <- placeholder_df[23000:24000, ]
# Get a first visual
# df <- evaluating_df
# fig <- df %>% plot_ly(x = ~full_date_time , type="candlestick",
#                       open = ~open, close = ~close,
#                       high = ~high, low = ~low) 
# fig <- fig %>% layout(title = pair,
#                       xaxis = list(rangeslider = list(visible = F)))
# fig


# Train and test datasets
placeholder_df[, c("prediction","movement", "exit_condition",
                   "tp", "ult_sl",
                   "RSI_1", "RSI_2", "RSI_3", "RSI_4", "RSI_5",
                   "EMA_1", "EMA_2", "EMA_3","EMA_4","EMA_5", "EMA_6",
                   "HL","macd","signal",
                   "mfi_1","mfi_2","mfi_3","mfi_4",
                   "obv",
                   "dn_1","dn_2","dn_3","dn_4",
                   "mavg_1","mavg_2","mavg_3","mavg_4",
                   "up_1","up_2","up_3","up_4",
                   "pctB_1","pctB_2","pctB_3","pctB_4",
                   "fastK",
                   "fastD",
                   "slowD",
                   "OC",
                   "sd_1","sd_2","sd_3","sd_4",
                   "action",
                   "Units",
                   "Price",
                    "id") := list(NA, NA, NA, NA, NA, NA,NA,NA,NA,NA,NA,
                                  NA, NA, NA, NA, NA, NA,NA,NA,NA,NA,
                                  NA, NA, NA, NA, NA, NA,NA,NA,NA,NA,
                                  NA, NA, NA, NA, NA, NA,NA,NA,NA,NA,
                                  NA, NA, NA, NA, NA, NA,NA,NA,NA, NA, NA) ]

evaluating_df[, c("prediction","movement","exit_condition",
                  "tp", "ult_sl",
                  "RSI_1", "RSI_2", "RSI_3", "RSI_4", "RSI_5",
                  "EMA_1", "EMA_2", "EMA_3","EMA_4","EMA_5", "EMA_6",
                  "HL","macd","signal",
                  "mfi_1","mfi_2","mfi_3","mfi_4",
                  "obv",
                  "dn_1","dn_2","dn_3","dn_4",
                  "mavg_1","mavg_2","mavg_3","mavg_4",
                  "up_1","up_2","up_3","up_4",
                  "pctB_1","pctB_2","pctB_3","pctB_4",
                  "fastK",
                  "fastD",
                  "slowD",
                  "OC",
                  "sd_1","sd_2","sd_3","sd_4",
                  "action",
                  "Units",
                  "Price",
                  "id") := list(NA, NA, NA, NA, NA, NA,NA,NA,NA,NA,NA,
                                NA, NA, NA, NA, NA, NA,NA,NA,NA,NA,
                                NA, NA, NA, NA, NA, NA,NA,NA,NA,NA,
                                NA, NA, NA, NA, NA, NA,NA,NA,NA,NA,
                                NA, NA, NA, NA, NA, NA,NA,NA,NA, NA, NA) ]

# Going intro the loop for test data -----------------------------------------
for (i in 1:nrow(test_data)){
  
  fut <- rbind(placeholder_df, evaluating_df[i, ])
  
  # EMAs
  fut$EMA_1 <- EMA(fut$close, n = 5)
  fut$EMA_2 <- EMA(fut$close, n = 7)
  fut$EMA_3 <- EMA(fut$close, n = 10)
  fut$EMA_4 <- EMA(fut$close, n = 15)
  fut$EMA_5 <- EMA(fut$close, n = 21)
  fut$EMA_6 <- EMA(fut$close, n = 50)
  
  # Candle description
  fut$HL <- fut$high - fut$low
  fut$OC <- fut$open - fut$close
  
  # RSI
  fut$RSI_1 <- RSI(fut$close, n = 5)
  fut$RSI_2 <- RSI(fut$close, n = 10)
  fut$RSI_3 <- RSI(fut$close, n = 14)
  fut$RSI_4 <- RSI(fut$close, n = 20)
  fut$RSI_5 <- RSI(fut$close, n = 25)
  
  # Standard deviation
  fut$sd_1 <- rollapplyr(fut$close, 5, sd, fill = NA)
  fut$sd_2 <- rollapplyr(fut$close, 10, sd, fill = NA)
  fut$sd_3 <- rollapplyr(fut$close, 14, sd, fill = NA)
  fut$sd_4 <- rollapplyr(fut$close, 20, sd, fill = NA)
  
  # # MACD
  macd <- MACD(fut[, "close"])
  fut$macd <- macd[, 1]
  fut$signal <- macd[, 2]
  
  # MFI

  fut$mfi_1 <- MFI(fut[, c("high", "low", "close")], fut[, "volume"],
                  n = 5)
  fut$mfi_2 <- MFI(fut[, c("high", "low", "close")], fut[, "volume"],
                  n = 10)
  fut$mfi_3 <- MFI(fut[, c("high", "low", "close")], fut[, "volume"],
                  n = 14)
  fut$mfi_4 <- MFI(fut[, c("high", "low", "close")], fut[, "volume"],
                  n = 20)
  # # OBV
  fut$obv <- OBV(fut[, "close"], fut[, "volume"])
  
  # -------------------------------------------------
  # Bollinger
  bollinger_1 <- BBands(fut[ ,c("high", "low", "close")], n = 10,
                        sd = 1)
  colnames(bollinger_1) <- paste(colnames(bollinger_1), "1", sep = "_")
  fut$dn_1 <- bollinger_1[, 1]
  fut$mavg_1 <- bollinger_1[, 2]
  fut$up_1 <- bollinger_1[, 3]
  fut$pctB_1 <- bollinger_1[, 4]
  
  bollinger_2 <- BBands(fut[ ,c("high", "low", "close")], n = 10,
                        sd = 2)
  colnames(bollinger_2) <- paste(colnames(bollinger_2), "2", sep = "_")
  fut$dn_2 <- bollinger_2[, 1]
  fut$mavg_2 <- bollinger_2[, 2]
  fut$up_2 <- bollinger_2[, 3]
  fut$pctB_2 <- bollinger_2[, 4]
  
  bollinger_3 <- BBands(fut[ ,c("high", "low", "close")], n = 20,
                        sd = 1)
  colnames(bollinger_3) <- paste(colnames(bollinger_3), "3", sep = "_")
  
  fut$dn_3 <- bollinger_3[, 1]
  fut$mavg_3 <- bollinger_3[, 2]
  fut$up_3 <- bollinger_3[, 3]
  fut$pctB_3 <- bollinger_3[, 4]
  
  bollinger_4 <- BBands(fut[ ,c("high", "low", "close")], n = 20,
                        sd = 2)
  colnames(bollinger_4) <- paste(colnames(bollinger_4), "4", sep = "_")
  fut$dn_4 <- bollinger_4[, 1]
  fut$mavg_4 <- bollinger_4[, 2]
  fut$up_4 <- bollinger_4[, 3]
  fut$pctB_4 <- bollinger_4[, 4]
  
  #----------------------------------------------------
   
  stochOSC <- stoch(fut[,c("high","low","close")])
  fut$fastK <- stochOSC[, 1]
  fut$fastD <- stochOSC[, 2]
  fut$slowD <- stochOSC[, 3]
  
  fit1 <- predict(model, newdata = tail(fut, 1)) 
  fut$prediction[nrow(fut)] <- fit1
  fut$movement[nrow(fut)] <- round(((fut$prediction[nrow(fut)] - fut$close[nrow(fut)]) / fut$prediction[nrow(fut)] )*100,2)
  # print(fut$movement[nrow(fut)])
  # print(fut$prediction[nrow(fut)])
  
  
  # Exit condition for takeprofit  - Fixed
  tp <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) + takeprofit * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)
  
  if (length(tp) == 0) {
    tp <- 0
  }
  
  # Ultimate stop loss
  ult_sl <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) - stoploss_ult * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)
  
  if (length(ult_sl) == 0) {
    ult_sl <- 0
  }
  fut$tp[nrow(fut)] <- tp
  fut$ult_sl[nrow(fut)] <- ult_sl
  
  fut$exit_condition[nrow(fut)] <-  fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]
  
  
  # Deciding upon action -----------------------------------------------------
  # Buy condition
  if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
       fut$movement[nrow(fut)] >= 0.70 ) {
    
    fut$action[nrow(fut)] <- "buy"
    fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
    fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)] - (0.0026 * fut$Units[nrow(fut)] * fut$close[nrow(fut)])
    # fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
    fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
    
    # Sell condition
  } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") & (
    fut$exit_condition[nrow(fut)] == TRUE  | fut$prediction[nrow(fut)] <= -0.5       )) {
    
    fut$action[nrow(fut)] <- "sell"
    fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
    fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]  - (0.0026 * fut$Units[nrow(fut)] * fut$close[nrow(fut)])
    # fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
    fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
    initial_budget <- fut$Price[nrow(fut)]
    
    # Keep condition
  } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   &
              ( fut$exit_condition[nrow(fut)] == FALSE  )) {
    
    fut$action[nrow(fut)] <- "keep"
    fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
    fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
    
  } else {
    
    fut$action[nrow(fut)] <- "no action"
    
  }
  
  placeholder_df <- fut
  # print(i)
  
  print(i)
}
myresult <- placeholder_df
initial_budget <- 200
calculate_profits(myresult)
View(myresult)
View(myresult[, -..vars])

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

