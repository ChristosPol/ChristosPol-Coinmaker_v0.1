# Get a pair with positive trend
# Calculate bollinger bands and SMA 50 and go long
# Combine with RSI and volume ?

initial_budget <- 500

# # select period of data 
candles_recent <- candles
# 
# # Plot if like 
# plot_candlesticks(dta = candles_recent, Ns = nrow(candles_recent), asset = pair)
# plot_candlesticks(dta = candles_recent, Ns = 100, asset = pair)
# 
# # N Training Data
train_n <- ceiling(nrow(candles_recent) / 20)
train_data <- candles_recent[1:train_n, ]
# 
# # Test, same 
test_data <- candles_recent[(train_n + 1):nrow(candles_recent), ]

RSI_Period <-10
RSI_below <-30
EMA_volume <-10
takeprofit <-0.05
stoploss_trail <-0.02
stoploss_ult <-0.02
times_vol <-1
bollinger_period <-10
Pure_RSI_Volume_Trailing <- function(RSI_Period, RSI_below, EMA_volume, takeprofit, stoploss_trail,stoploss_ult, times_vol, bollinger_period) {
  
  # Train and test datasets
  train_data[, c("RSI",
                 "EMA_volume",
                 "dn",
                 "mavg",
                 "up",
                 "pctB",
                 "exit_value",
                 "exit_condition",
                 "crossover_volume",
                 "crossover_RSI",
                 "crossover_bollinger",
                 "action",
                 "Units",
                 "Price",
                 "tp",
                 "ult_sl",
                 "trail_sl",
                 "id") := list(NA, NA,NA,NA,NA,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]
  
  test_data[, c("RSI",
                "EMA_volume",
                "dn",
                "mavg",
                "up",
                "pctB",
                "exit_value",
                "exit_condition",
                "crossover_volume",
                "crossover_RSI",
                "crossover_bollinger",
                "action",
                "Units",
                "Price",
                "tp",
                "ult_sl",
                "trail_sl",
                "id") := list(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]
  
  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){
    
    fut <- rbind(train_data, test_data[i, ])
    
    # Technical indicators -----------------------------------------------------
    
    fut$RSI <- RSI(fut$close, n = RSI_Period)
    fut$EMA_volume <- EMA(fut$volume, n = EMA_volume)
    fut$dn <- BBands(fut$close, n =bollinger_period, maType = "SMA")[, 1]
    fut$mavg <- BBands(fut$close, n =bollinger_period, maType = "SMA")[, 2]
    fut$up <- BBands(fut$close, n =bollinger_period, maType = "SMA")[, 3]
    fut$pctB <- BBands(fut$close, n =bollinger_period, maType = "SMA")[, 4]
    
    
    
    # Volume Crossing of volume over the SMA(volume, n_periods)
    fut$crossover_volume[nrow(fut)] <- ifelse(fut$volume[nrow(fut)] > fut$EMA_volume[nrow(fut)] * times_vol ,
                                              "volume_higher", "volume_lower")
    
    
    # RSI Crossing of upper or lower bounds
    fut$crossover_RSI[nrow(fut)] <- ifelse(fut$RSI[nrow(fut)] < RSI_below ,
                                           "RSI_lower", "RSI_higher")
    
    # Bollinger Crossing of lower bounds
    fut$crossover_bollinger[nrow(fut)] <- ifelse(fut$close[nrow(fut)] < fut$dn[nrow(fut)] ,
                                           "bollinger_lower", "bollinger_higher")
    
    
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
    
    
    # Trailing stop loss
    
    # id_trail <- tail(fut$id[!is.na(fut$id)], 1)
    # 
    # if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){
    #   
    #   trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
    #   
    #   if(trail_sl > max(fut$trail_sl[which(fut$id == id_trail)+1], na.rm =T)){
    #     trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
    #     
    #   }else{
    #     
    #     trail_sl <- max(fut$trail_sl[which(fut$id == id_trail)+1])
    #   }
    #   
    #   
    # 
    # } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1]) ){
    #   
    #   trail_sl <- max(fut$trail_sl[which(fut$id == id_trail)])
    # 
    # } else {
    #   
    #   trail_sl <-0 
    # } 
    # 
    
    # if(length(trail_sl) == 0 ){
    #   
    #   trail_sl <- 0
    # }
    
    # Trailing stop loss
    # browser()
    if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){
      
      trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      if( trail_sl < tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)){
        trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
        
      }else {
        trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
        
      }
      
      
    } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1] ) ){
      
      trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
    } else {
      
      trail_sl <-0
    }
    
    
    if(length(trail_sl) == 0 ){
      
      trail_sl <- 0
    }
    # browser()
    
    
    
    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl
    fut$trail_sl[nrow(fut)] <- trail_sl
    
    
    fut$exit_condition[nrow(fut)] <- fut$trail_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]
    
    
    # Deciding upon action -----------------------------------------------------
    
    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         fut$crossover_bollinger[nrow(fut)] == "bollinger_lower" &  fut$crossover_RSI[nrow(fut)] == "RSI_lower"&  fut$pctB[nrow(fut)] < -0.05 ) {
      
      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
      
      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") & (
      fut$exit_condition[nrow(fut)] == TRUE  )) {
      
      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]
      
      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   & 
                fut$exit_condition[nrow(fut)] == FALSE  ) {
      
      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      
    } else {
      
      fut$action[nrow(fut)] <- "no action"
      
    }
    
    train_data <- fut
    print(i)
  }
  return(train_data)
}
unique(train_data$Price)
