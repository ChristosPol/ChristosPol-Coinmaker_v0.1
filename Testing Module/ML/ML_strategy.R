# Training model dates
# train_data <- fut[full_date_time <= "2020-11-18 05:00:00 CET"]
# test_data <- fut[full_date_time > "2020-11-18 05:00:00 CET"]
paraller_exec <- FALSE
initial_budget <- 200
# stoploss_ult <- 0.03
# takeprofit <- 0.05
fut <- klines[[1]]

# select period of data 

fut$prediction <- shift(fut$close, type = "lead")
fut$UPDOWN <- ifelse(fut$close-fut$prediction > 0, "DOWN", "UP")


placeholder_fut <- fut[1:61000, ]

evaluating_fut <- fut[61001:nrow(fut), ]
placeholder_fut <- fut[60000:61000, ]

# placeholder_fut <- placeholder_fut[57000:58000, ]


# Train and test datasets
placeholder_fut[, c("UPDOWN_PRE","candle_type",   "EMA_1",        
                   "EMA_2", "EMA_3", "EMA_4",         "EMA_5",
                   "EMA_6", "EMA_7", "EMA_8",         "EMA_9",
                   "roc_1", "roc_2", "roc_3" ,        "roc_4",
                   "roc_5", "roc_6", "roc_7",         "roc_8" ,      
                   "HL", "OC", "RSI_1",         "RSI_2",       
                   "RSI_3", "RSI_4", "RSI_5",         "sd_1",        
                   "sd_2", "sd_3", "sd_4",          "macd",        
                   "signal", "mfi_1", "mfi_2",         "mfi_3",       
                   "mfi_4", "obv", "dn_4",          "mavg_4",      
                   "up_4", "pctB_4", "fastK",         "fastD",       
                   "slowD", "williams", "DIp_1",         "DIn_1",       
                   "DX_1", "ADX_1", "DIp_2",         "DIn_2",       
                   "DX_2", "ADX_2", "DIp_3",         "DIn_3",       
                   "DX_3", "ADX_3", "DIp_4",         "DIn_4",       
                   "DX_4", "ADX_4", "aroonUp_1",     "aroonDn_1",   
                   "oscillator_1", "aroonUp_2",     "aroonDn_2",    "oscillator_2",
                   "aroonUp_3", "aroonDn_3",     "oscillator_3", "aroonUp_4" ,  
                   "aroonDn_4", "oscillator_4",  "ch_vol_1",     "ch_vol_2"  ,  
                   "ch_vol_3", "ch_vol_4",      "ch_ad",        "cci_1"  ,     
                   "cci_2", "cci_3",         "cci_4" ,       "tr_1"   ,     
                   "atr_1", "trueHigh_1",    "trueLow_1",    "tr_2"  ,      
                   "atr_2", "trueHigh_2" ,   "trueLow_2",    "tr_3"  ,      
                   "atr_3", "trueHigh_3",    "trueLow_3",    "tr_4" ,       
                   "atr_4", "trueHigh_4" ,   "trueLow_4",    "clv" ,        
                   "cmf", "cmo"  ,         "hma_1"   ,     "hma_2" ,      
                   "hma_3", "hma_4"  ,       "hma_5" ,       "vClose" ,     
                   "vClose0", "vGK"  ,         "sar" ,         "tdi" ,        
                   "di", "vhf.close" ,
                   "action",
                   "Units",
                   "Price",
                    "id") := list(NA,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                  NA, NA, NA, NA, NA, NA) ]

evaluating_fut[, c("UPDOWN_PRE","candle_type",   "EMA_1",        
                  "EMA_2", "EMA_3", "EMA_4",         "EMA_5",
                  "EMA_6", "EMA_7", "EMA_8",         "EMA_9",
                  "roc_1", "roc_2", "roc_3" ,        "roc_4",
                  "roc_5", "roc_6", "roc_7",         "roc_8" ,      
                  "HL", "OC", "RSI_1",         "RSI_2",       
                  "RSI_3", "RSI_4", "RSI_5",         "sd_1",        
                  "sd_2", "sd_3", "sd_4",          "macd",        
                  "signal", "mfi_1", "mfi_2",         "mfi_3",       
                  "mfi_4", "obv", "dn_4",          "mavg_4",      
                  "up_4", "pctB_4", "fastK",         "fastD",       
                  "slowD", "williams", "DIp_1",         "DIn_1",       
                  "DX_1", "ADX_1", "DIp_2",         "DIn_2",       
                  "DX_2", "ADX_2", "DIp_3",         "DIn_3",       
                  "DX_3", "ADX_3", "DIp_4",         "DIn_4",       
                  "DX_4", "ADX_4", "aroonUp_1",     "aroonDn_1",   
                  "oscillator_1", "aroonUp_2",     "aroonDn_2",    "oscillator_2",
                  "aroonUp_3", "aroonDn_3",     "oscillator_3", "aroonUp_4" ,  
                  "aroonDn_4", "oscillator_4",  "ch_vol_1",     "ch_vol_2"  ,  
                  "ch_vol_3", "ch_vol_4",      "ch_ad",        "cci_1"  ,     
                  "cci_2", "cci_3",         "cci_4" ,       "tr_1"   ,     
                  "atr_1", "trueHigh_1",    "trueLow_1",    "tr_2"  ,      
                  "atr_2", "trueHigh_2" ,   "trueLow_2",    "tr_3"  ,      
                  "atr_3", "trueHigh_3",    "trueLow_3",    "tr_4" ,       
                  "atr_4", "trueHigh_4" ,   "trueLow_4",    "clv" ,        
                  "cmf", "cmo"  ,         "hma_1"   ,     "hma_2" ,      
                  "hma_3", "hma_4"  ,       "hma_5" ,       "vClose" ,     
                  "vClose0", "vGK"  ,         "sar" ,         "tdi" ,        
                  "di", "vhf.close" ,
                  "action",
                  "Units",
                  "Price",
                  "id") := list(NA,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                NA, NA, NA, NA, NA, NA) ]

# Going intro the loop for test data -----------------------------------------
for (i in 1:nrow(test_data)){
  
  fut <- rbind(placeholder_fut, evaluating_fut[i, ])
  
  # EMAs
  fut$EMA_1 <- EMA(fut$close, n = 5)
  fut$EMA_2 <- EMA(fut$close, n = 7)
  fut$EMA_3 <- EMA(fut$close, n = 10)
  fut$EMA_4 <- EMA(fut$close, n = 15)
  fut$EMA_5 <- EMA(fut$close, n = 21)
  fut$EMA_6 <- EMA(fut$close, n = 50)
  fut$EMA_7 <- EMA(fut$close, n = 100)
  fut$EMA_8 <- EMA(fut$close, n = 150)
  fut$EMA_9 <- EMA(fut$close, n = 200)
  
  fut$roc_1 <- momentum(fut$close, n = 1)
  fut$roc_2 <- momentum(fut$close, n = 3)
  fut$roc_3 <- momentum(fut$close, n = 9)
  fut$roc_4 <- momentum(fut$close, n = 15)
  fut$roc_5 <- momentum(fut$close, n = 20)
  fut$roc_6 <- momentum(fut$close, n = 30)
  fut$roc_7 <- momentum(fut$close, n = 50)
  fut$roc_8 <- momentum(fut$close, n = 100)
  
  fut$candle_type[fut$close > fut$open] <- "bullish"
  fut$candle_type[fut$close < fut$open] <- "bearish"
  fut$candle_type[is.na(fut$candle_type)] <- "neutral"
  fut$candle_type <- as.factor(fut$candle_type)
  
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
  rm(macd)
  gc()
  
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
  # bollinger_1 <- BBands(fut[ ,c("high", "low", "close")], n = 10,
  #                       sd = 1)
  # colnames(bollinger_1) <- paste(colnames(bollinger_1), "1", sep = "_")
  # fut$dn_1 <- bollinger_1[, 1]
  # fut$mavg_1 <- bollinger_1[, 2]
  # fut$up_1 <- bollinger_1[, 3]
  # fut$pctB_1 <- bollinger_1[, 4]
  # 
  # bollinger_2 <- BBands(fut[ ,c("high", "low", "close")], n = 10,
  #                       sd = 2)
  # colnames(bollinger_2) <- paste(colnames(bollinger_2), "2", sep = "_")
  # fut$dn_2 <- bollinger_2[, 1]
  # fut$mavg_2 <- bollinger_2[, 2]
  # fut$up_2 <- bollinger_2[, 3]
  # fut$pctB_2 <- bollinger_2[, 4]
  # 
  # bollinger_3 <- BBands(fut[ ,c("high", "low", "close")], n = 20,
  #                       sd = 1)
  # colnames(bollinger_3) <- paste(colnames(bollinger_3), "3", sep = "_")
  # 
  # fut$dn_3 <- bollinger_3[, 1]
  # fut$mavg_3 <- bollinger_3[, 2]
  # fut$up_3 <- bollinger_3[, 3]
  # fut$pctB_3 <- bollinger_3[, 4]
  # 
  bollinger_4 <- BBands(fut[ ,c("high", "low", "close")], n = 20,
                        sd = 2)
  colnames(bollinger_4) <- paste(colnames(bollinger_4), "4", sep = "_")
  fut$dn_4 <- bollinger_4[, 1]
  fut$mavg_4 <- bollinger_4[, 2]
  fut$up_4 <- bollinger_4[, 3]
  fut$pctB_4 <- bollinger_4[, 4]
  rm(bollinger_4)
  gc()
  #----------------------------------------------------
   
  stochOSC <- stoch(fut[,c("high","low","close")])
  fut$fastK <- stochOSC[, 1]
  fut$fastD <- stochOSC[, 2]
  fut$slowD <- stochOSC[, 3]
  rm(stochOSC)
  gc()
  
  
  fut$williams <- WPR(fut[, c("high", "low", "close")], n = 14)
  
  # ADX
  ADX_1 <- ADX(fut[,c("high","low","close")], n = 5)
  colnames(ADX_1) <- paste(colnames(ADX_1), "1", sep = "_")
  
  fut$DIp_1 <- ADX_1[, 1]
  fut$DIn_1 <- ADX_1[, 2]
  fut$DX_1 <- ADX_1[, 3]
  fut$ADX_1 <- ADX_1[, 4]
  rm(ADX_1)
  
  
  ADX_2 <- ADX(fut[,c("high","low","close")], n = 10)
  colnames(ADX_2) <- paste(colnames(ADX_2), "2", sep = "_")
  
  fut$DIp_2 <- ADX_2[, 1]
  fut$DIn_2 <- ADX_2[, 2]
  fut$DX_2 <- ADX_2[, 3]
  fut$ADX_2 <- ADX_2[, 4]
  
  rm(ADX_2)
  ADX_3 <- ADX(fut[,c("high","low","close")], n = 14)
  colnames(ADX_3) <- paste(colnames(ADX_3), "3", sep = "_")
  
  fut$DIp_3 <- ADX_3[, 1]
  fut$DIn_3 <- ADX_3[, 2]
  fut$DX_3 <- ADX_3[, 3]
  fut$ADX_3 <- ADX_3[, 4]
  
  rm(ADX_3)
  ADX_4 <- ADX(fut[,c("high","low","close")], n = 20)
  colnames(ADX_4) <- paste(colnames(ADX_4), "4", sep = "_")
  
  fut$DIp_4 <- ADX_4[, 1]
  fut$DIn_4 <- ADX_4[, 2]
  fut$DX_4 <- ADX_4[, 3]
  fut$ADX_4 <- ADX_4[, 4]
  
  rm(ADX_4)
  AROON_1 <- aroon(fut[, c("high", "low")], n = 5)
  colnames(AROON_1) <- paste(colnames(AROON_1), "1", sep = "_")
  fut$aroonUp_1 <- AROON_1[, 1]
  fut$aroonDn_1 <- AROON_1[, 2]
  fut$oscillator_1 <- AROON_1[, 3]
  rm(AROON_1)
  
  AROON_2 <- aroon(fut[, c("high", "low")], n = 10)
  colnames(AROON_2) <- paste(colnames(AROON_2), "2", sep = "_")
  
  fut$aroonUp_2 <- AROON_2[, 1]
  fut$aroonDn_2 <- AROON_2[, 2]
  fut$oscillator_2 <- AROON_2[, 3]
  rm(AROON_2)
  
  AROON_3 <- aroon(fut[, c("high", "low")], n = 18)
  colnames(AROON_3) <- paste(colnames(AROON_3), "3", sep = "_")
  
  fut$aroonUp_3 <- AROON_3[, 1]
  fut$aroonDn_3 <- AROON_3[, 2]
  fut$oscillator_3 <- AROON_3[, 3]
  rm(AROON_3)
  AROON_4 <- aroon(fut[, c("high", "low")], n = 25)
  colnames(AROON_4) <- paste(colnames(AROON_4), "4", sep = "_")
  
  fut$aroonUp_4 <- AROON_4[, 1]
  fut$aroonDn_4 <- AROON_4[, 2]
  fut$oscillator_4 <- AROON_4[, 3]
  rm(AROON_4)
  fut$ch_vol_1 <- chaikinVolatility(fut[,c("high","low")], n = 5)
  fut$ch_vol_2 <- chaikinVolatility(fut[,c("high","low")], n = 10)
  fut$ch_vol_3 <- chaikinVolatility(fut[,c("high","low")], n = 15)
  fut$ch_vol_4 <- chaikinVolatility(fut[,c("high","low")], n = 20)
  
  fut$ch_ad <- chaikinAD(fut[, c("high","low","close")], fut[,"volume"])
  
  fut$cci_1 <- CCI(fut[, c("high","low","close")], n = 5)
  fut$cci_2 <- CCI(fut[, c("high","low","close")], n = 10)
  fut$cci_3 <- CCI(fut[, c("high","low","close")], n = 15)
  fut$cci_4 <- CCI(fut[, c("high","low","close")], n = 20)
  
  
  atr_1 <- ATR(fut[, c("high","low","close")], n=5)
  colnames(atr_1) <- paste(colnames(atr_1), "1", sep = "_")
  fut$tr_1 <- atr_1[, 1]
  fut$atr_1 <- atr_1[, 2]
  fut$trueHigh_1 <- atr_1[, 3]
  fut$trueLow_1 <- atr_1[, 4]
  
  rm(atr_1)
  atr_2 <- ATR(fut[, c("high","low","close")], n=10)
  colnames(atr_2) <- paste(colnames(atr_2), "2", sep = "_")
  
  fut$tr_2 <- atr_2[, 1]
  fut$atr_2 <- atr_2[, 2]
  fut$trueHigh_2 <- atr_2[, 3]
  fut$trueLow_2 <- atr_2[, 4]
  
  rm(atr_2)
  atr_3 <- ATR(fut[, c("high","low","close")], n=14)
  colnames(atr_3) <- paste(colnames(atr_3), "3", sep = "_")
  
  fut$tr_3 <- atr_3[, 1]
  fut$atr_3 <- atr_3[, 2]
  fut$trueHigh_3 <- atr_3[, 3]
  fut$trueLow_3 <- atr_3[, 4]
  
  rm(atr_3)
  atr_4 <- ATR(fut[, c("high","low","close")], n=20)
  colnames(atr_4) <- paste(colnames(atr_4), "4", sep = "_")
  
  fut$tr_4 <- atr_4[, 1]
  fut$atr_4 <- atr_4[, 2]
  fut$trueHigh_4 <- atr_4[, 3]
  fut$trueLow_4 <- atr_4[, 4]
  
  rm(atr_4)
  fut$clv <- CLV(fut[,c("high","low","close")])
  
  fut$cmf <- CMF(fut[,c("high","low","close")], fut[,c("volume")])
  
  fut$cmo <- CMO(fut[,"close"])
  
  fut$hma_1 <- HMA(fut[,"close"], n =10)
  fut$hma_2 <- HMA(fut[,"close"], n =30)
  fut$hma_3 <- HMA(fut[,"close"], n =70)
  fut$hma_4 <- HMA(fut[,"close"], n =120)
  fut$hma_5 <- HMA(fut[,"close"], n =200)
  
  
  ohlc <- fut[,c("open","high","low","close")]
  fut$vClose <- volatility(ohlc, calc="close")
  fut$vClose0 <- volatility(ohlc, calc="close", mean0=TRUE)
  fut$vGK <- volatility(ohlc, calc="garman")
  
  # kst <- KST(df[,"close"])
  
  # pbands.close <- PBands( df[,"close"] )
  fut$sar <- SAR(fut[, c("high","low")])
  
  tdi <- TDI(fut[,"close"], n=30)
  
  fut$tdi <- tdi[, 1]
  fut$di <- tdi[, 2]
  
  fut$vhf.close <- VHF(fut[,"close"])
  gc()
  
  ######
  
  fit1 <- predict(model, newdata = tail(fut, 1)) 
  fut$UPDOWN_PRE[nrow(fut)] <- as.character(fit1)
  # fut$prediction[nrow(fut)] <- fit1
  # fut$movement[nrow(fut)] <- round(((fut$prediction[nrow(fut)] - fut$close[nrow(fut)]) / fut$prediction[nrow(fut)] )*100,2)
  # 
  
  # # Exit condition for takeprofit  - Fixed
  # tp <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) + takeprofit * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)
  # 
  # if (length(tp) == 0) {
  #   tp <- 0
  # }
  # 
  # # Ultimate stop loss
  # ult_sl <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) - stoploss_ult * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)
  # 
  # if (length(ult_sl) == 0) {
  #   ult_sl <- 0
  # }
  # fut$tp[nrow(fut)] <- tp
  # fut$ult_sl[nrow(fut)] <- ult_sl
  # 
  # fut$exit_condition[nrow(fut)] <-  fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]
  # 
  
  # Deciding upon action -----------------------------------------------------
  # Buy condition
  if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
       fit1 == "UP" ) {
    
    fut$action[nrow(fut)] <- "buy"
    fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
    fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)] - (0.0026 * fut$Units[nrow(fut)] * fut$close[nrow(fut)])
    # fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
    fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
    
    # Sell condition
  } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") & (
    fit1 == "DOWN"     )) {
    
    fut$action[nrow(fut)] <- "sell"
    fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
    fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]  - (0.0026 * fut$Units[nrow(fut)] * fut$close[nrow(fut)])
    # fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
    fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
    initial_budget <- fut$Price[nrow(fut)]
    
    # Keep condition
  } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   &
              (  fit1 == "UP"  )) {
    
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

