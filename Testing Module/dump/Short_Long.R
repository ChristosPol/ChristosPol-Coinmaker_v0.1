initial_budget <- 500

# # select period of data 
candles_recent <- candles
# 
# # Plot if like 
# plot_candlesticks(dta = candles_recent, Ns = nrow(candles_recent), asset = pair)
# plot_candlesticks(dta = candles_recent, Ns = 100, asset = pair)
# 
# # N Training Data
train_n <- ceiling(nrow(candles_recent) / 30)
train_data <- candles_recent[1:train_n, ]
# 
# # Test, same 
test_data <- candles_recent[(train_n + 1):nrow(candles_recent), ]

RSI_Period <- 10
RSI_below <- 25
RSI_above <- 80
EMA_volume <- 10
takeprofit <- 0.04
stoploss_trail <- 0.02
stoploss_ult <- 0.02
times_vol <- 2

Pure_RSI_Volume_Trailing <- function(RSI_Period, RSI_below, EMA_volume, takeprofit, stoploss_trail,stoploss_ult, times_vol) {
  
  # Train and test datasets
  train_data[, c("RSI",
                 "EMA_volume",
                 "exit_value",
                 "exit_condition_long",
                 "exit_condition_short",
                 "crossover_volume",
                 "crossover_RSI",
                 "action",
                 "Units",
                 "Price",
                 "tp_long",
                 "ult_sl_long",
                 "trail_sl_long",
                 "tp_short",
                 "ult_sl_short",
                 "trail_sl_short",
                 "id") := list(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]
  
  test_data[, c("RSI",
                "EMA_volume",
                "exit_value",
                "exit_condition_long",
                "exit_condition_short",
                "crossover_volume",
                "crossover_RSI",
                "action",
                "Units",
                "Price",
                "tp_long",
                "ult_sl_long",
                "trail_sl_long",
                "tp_short",
                "ult_sl_short",
                "trail_sl_short",
                "id") := list(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]
  
  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){
    
    fut <- rbind(train_data, test_data[i, ])
    
    # RSI and Volume
    fut$RSI <- RSI(fut$close, n = RSI_Period)
    fut$EMA_volume <- EMA(fut$volume, n = EMA_volume)
    
    # Technical indicators -----------------------------------------------------
    
    
    # Volume Crossing of volume over the SMA(volume, n_periods)
    fut$crossover_volume[nrow(fut)] <- ifelse(fut$volume[nrow(fut)] > fut$EMA_volume[nrow(fut)] * times_vol ,
                                              "volume_higher", "volume_lower")
    
    
    # RSI Crossing of upper or lower bounds
    if(fut$RSI[nrow(fut)] < RSI_below){
      
      fut$crossover_RSI[nrow(fut)] <- "RSI_lower"
      
    } else if (fut$RSI[nrow(fut)] > RSI_above){
      
      fut$crossover_RSI[nrow(fut)] <- "RSI_above"
    } else {
      
      fut$crossover_RSI[nrow(fut)] <- "RSI_between"
    }
    
    
    # Exit condition for long trades -------------------------------------------
    tp_long <- tail(fut$close[fut$action == "enter_long"][!is.na(fut$close[fut$action == "enter_long"])], 1) + takeprofit * tail(fut$close[fut$action == "enter_long"][!is.na(fut$close[fut$action == "enter_long"])], 1)
    
    if (length(tp_long) == 0) {
      tp_long <- 0
    }
    
    # Ultimate stop loss
    ult_sl_long <- tail(fut$close[fut$action == "enter_long"][!is.na(fut$close[fut$action == "enter_long"])], 1) - stoploss_ult * tail(fut$close[fut$action == "enter_long"][!is.na(fut$close[fut$action == "enter_long"])], 1)
    
    if (length(ult_sl_long) == 0) {
      ult_sl_long <- 0
    }
    
    
    
    if (fut$action[nrow(fut)-1] %in% c("enter_long", "keep_long") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){
      
      trail_sl_long <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      if( trail_sl_long < tail(fut$trail_sl_long[!is.na(fut$trail_sl_long)], 1)){
        trail_sl_long <- tail(fut$trail_sl_long[!is.na(fut$trail_sl_long)], 1)
        
      }else {
        trail_sl_long <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
        
      }
      
      
    } else if (fut$action[nrow(fut)-1] %in% c("enter_long", "keep_long") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1] ) ){
      
      trail_sl_long <- tail(fut$trail_sl_long[!is.na(fut$trail_sl_long)], 1)
    } else {
      
      trail_sl_long <-0
    }
    

    # if(length(trail_sl_long) == 0 ){
    #   
    #   trail_sl_long <- 0
    # }

    
    
    fut$tp_long[nrow(fut)] <- tp_long
    fut$ult_sl_long[nrow(fut)] <- ult_sl_long
    fut$trail_sl_long[nrow(fut)] <- trail_sl_long
    
    # Exit condition for short trades ------------------------------------------
    tp_short <- tail(fut$close[fut$action == "enter_short"][!is.na(fut$close[fut$action == "enter_short"])], 1) - takeprofit * tail(fut$close[fut$action == "enter_short"][!is.na(fut$close[fut$action == "enter_short"])], 1)
    
    if (length(tp_short) == 0) {
      tp_short <- 0
    }
    
    # Ultimate stop loss
    ult_sl_short <- tail(fut$close[fut$action == "enter_short"][!is.na(fut$close[fut$action == "enter_short"])], 1) + stoploss_ult * tail(fut$close[fut$action == "enter_short"][!is.na(fut$close[fut$action == "enter_short"])], 1)
    
    if (length(ult_sl_short) == 0) {
      ult_sl_short <- 0
    }
    
    
    
    if (fut$action[nrow(fut)-1] %in% c("enter_short", "keep_short") & ( fut$close[nrow(fut)] < fut$close[nrow(fut)-1] )  ){
      
      trail_sl_short <- fut$close[nrow(fut)] + stoploss_trail * fut$close[nrow(fut)]
      
      if( trail_sl_short > tail(fut$trail_sl_short[!is.na(fut$trail_sl_short)], 1)){
        trail_sl_short <- tail(fut$trail_sl_short[!is.na(fut$trail_sl_short)], 1)
        
      } else {
        trail_sl_short <- fut$close[nrow(fut)] + stoploss_trail * fut$close[nrow(fut)]
        
      }
      
      
    } else if (fut$action[nrow(fut)-1] %in% c("enter_short", "keep_short") & ( fut$close[nrow(fut)] >= fut$close[nrow(fut)-1] ) ){
      
      trail_sl_short <- tail(fut$trail_sl_short[!is.na(fut$trail_sl_short)], 1)
    } else {
      
      trail_sl_short <-1000000
    }
    
    
    # if(length(trail_sl_short) == 0 ){
    #   
    #   trail_sl_short <- 0
    # }

    
    
    fut$tp_short[nrow(fut)] <- tp_short
    fut$ult_sl_short[nrow(fut)] <- ult_sl_short
    fut$trail_sl_short[nrow(fut)] <- trail_sl_short
    
    
    fut$exit_condition_long[nrow(fut)] <- fut$trail_sl_long[nrow(fut)] > fut$close[nrow(fut)] | fut$ult_sl_long[nrow(fut)] > fut$close[nrow(fut)] | fut$tp_long[nrow(fut)] < fut$close[nrow(fut)]
    fut$exit_condition_short[nrow(fut)] <- fut$trail_sl_short[nrow(fut)] < fut$close[nrow(fut)] | fut$ult_sl_short[nrow(fut)] < fut$close[nrow(fut)] | fut$tp_short[nrow(fut)] > fut$close[nrow(fut)]
    
    
    # Deciding upon action -----------------------------------------------------
    
    # Enter long position ------------------------------------------------------
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("exit_long", "no action", "exit_short")) &
         fut$crossover_volume[nrow(fut)] == "volume_higher" &  fut$crossover_RSI[nrow(fut)] == "RSI_lower" ) {
      
      fut$action[nrow(fut)] <- "enter_long"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
      
      # Exit long position -----------------------------------------------------
    } else if (fut$action[nrow(fut) - 1] %in% c("keep_long", "enter_long") & (
      fut$exit_condition_long[nrow(fut)] == TRUE  )) {
      
      fut$action[nrow(fut)] <- "exit_long"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]
      
      # Keep long position -----------------------------------------------------
    } else if ( fut$action[nrow(fut) - 1] %in% c("enter_long", "keep_long")   & 
                fut$exit_condition_long[nrow(fut)] == FALSE  ) {
      
      fut$action[nrow(fut)] <- "keep_long"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
    
      # Enter short position ---------------------------------------------------  
    } else if ((is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("exit_long", "no action", "exit_short")) &
               fut$crossover_volume[nrow(fut)] == "volume_higher" &  fut$crossover_RSI[nrow(fut)] == "RSI_above" ) {
      
      fut$action[nrow(fut)] <- "enter_short"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
      
      # Exit short position ----------------------------------------------------
    } else if(fut$action[nrow(fut) - 1] %in% c("keep_short", "enter_short") & (
      fut$exit_condition_short[nrow(fut)] == TRUE  )) {
      
      fut$action[nrow(fut)] <- "exit_short"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]
      
      # Keep short position ----------------------------------------------------
    } else if(fut$action[nrow(fut) - 1] %in% c("enter_short", "keep_short")   & 
              fut$exit_condition_short[nrow(fut)] == FALSE  ) {
      
      
      fut$action[nrow(fut)] <- "keep_short"
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
View(train_data[train_data$action %in% c("enter_long","keep_long","exit_long", "enter_short","keep_short","exit_short"), ])


calculate_profits()
dataset <- train_data
calcu <- dataset[action %in% c("enter_short", "exit_short"), ]
calcu <- subset(calcu,  !calcu$id %in% names(which(table(calcu$id) ==1)))
if (nrow(calcu) > 0) {
  
  profit <- c()
  profit_sum <- c()
  ids <- unique(calcu$id)
  for(i in 1:length(ids)){
    
    profit[i] <-   calcu$Price[calcu$action =="exit_short" & calcu$id == ids[i]] - calcu$Price[calcu$action =="enter_short" & calcu$id == ids[i]] 
  }
  profit_sum <- sum(profit)
  dd <- data.frame(profit = profit_sum, n_trades = length(unique(calcu$id)),
                   enter_date = unique(calcu$Date)[1], exit_date = tail(unique(calcu$Date), 1))
} else {
  
  dd <- data.frame(profit = 0, n_trades = 0, enter_date = as.Date("2020-04-07"), exit_date =as.Date("2020-04-07"))
}

