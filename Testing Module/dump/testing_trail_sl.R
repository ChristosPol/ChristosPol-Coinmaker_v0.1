# Strategy 2 -------------------------------------------------------------------
# Crossing fast ema to slower with stoploss condition
fut

fast_EMA <- 9
slow_EMA <- 21
takeprofit <- 0.05
stoploss_trail <- 0.02
stoploss_ult <- 0.05
cross_EMA_stoploss_trail <- function(fast_EMA, slow_EMA,takeprofit, stoploss_trail,stoploss_ult) {
  
  # Train and test datasets$
  train_data[, c(paste0("EMA", "_", fast_EMA),
                 paste0("EMA", "_", slow_EMA),
                 "exit_value","exit_condition",
                 "crossover",
                 "crossover_Volume",
                 "action",
                 "tp",
                 "ult_sl",
                 "trail_sl",
                 "Units",
                 "Price",
                 "id") := list(NA, NA,NA,NA,NA, NA, NA, NA, NA, NA, NA, NA, NA) ]
  
  test_data[, c(paste0("EMA", "_", fast_EMA),
                paste0("EMA", "_", slow_EMA),
                "exit_value",
                "exit_condition",
                "crossover",
                "crossover_Volume",
                "action",
                "tp",
                "ult_sl",
                "trail_sl",
                "Units",
                "Price",
                "id") := list(NA, NA, NA,NA,NA,NA, NA, NA, NA ,NA, NA, NA, NA) ]
  
  # Loop to evaluate last line of the test dataset
  for (i in 1:nrow(test_data)) {
    
    # Bind last row
    fut <- rbind(train_data, test_data[i, ])
    
    # Create indicators
    fut[, c(paste0("EMA", "_", fast_EMA),
            paste0("EMA", "_", slow_EMA)) := list(EMA(close, n = fast_EMA),
                                                  EMA(close, n = slow_EMA)) ]
    
    
    fut$crossover[get(paste0("EMA", "_", fast_EMA), fut) > get(paste0("EMA", "_", slow_EMA), fut)] <- "faster_EMA_higher"
    fut$crossover[get(paste0("EMA", "_", fast_EMA), fut) <= get(paste0("EMA", "_", slow_EMA), fut)] <- "faster_EMA_lower"
    
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
    
    
    # BUY Condition
    if ((is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action"))
        & fut$crossover[nrow(fut)] == "faster_EMA_higher") {
      
      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)]*fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
      
      # Keep Condition  
    } else if (fut$action[nrow(fut) - 1] %in% c("buy", "keep") &
               fut$exit_condition[nrow(fut)] == FALSE ) {
      
      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      
      # Sell Condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") &
               fut$exit_condition[nrow(fut)] == TRUE) {
      
      
      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]
      
      # No Action Condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("sell", "no action") | (is.na(fut$action[nrow(fut) - 1])
                                                                         & fut$crossover[nrow(fut)] == "faster_EMA_lower")) {
      
      fut$action[nrow(fut)] <- "no action"
    }
    
    train_data <- fut
  }
  return(train_data)
}
