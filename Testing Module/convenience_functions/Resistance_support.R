candles <- simple_OHLC(interval = 60, pair = "XRPEUR")
candles$RSI <- RSI(candles$close, n =14)
SR_lines(roll = 100, data = candles, plot.it = T)

# Test, same 
real <- ceiling(nrow(candles) / 20)
realized_candles <- candles[1:real, ]

future_candles <- candles[(real + 1):nrow(candles), ]
roll <- 300
takeprofit <- 0.1
stoploss_trail <- 0.1
stoploss_ult <- 0.1
initial_budget <- 500

Pure_RSI_Volume_Trailing <- function(roll, takeprofit, stoploss_trail,stoploss_ult) {
  
  # Train and test datasets
  realized_candles[, c("SL",
                       "RL",
                       "exit_condition",
                       "crossover_support",
                       "crossover_resistance",
                       "action",
                       "Units",
                       "Price",
                       "tp",
                       "ult_sl",
                       "trail_sl",
                       "id") := list(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)]

  future_candles[, c("SL",
                     "RL",
                     "exit_condition",
                     "crossover_support",
                     "crossover_resistance",
                     "action",
                     "Units",
                     "Price",
                     "tp",
                     "ult_sl",
                     "trail_sl",
                     "id") := list(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)]
  # nrow(future_candles)
  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(future_candles)){
    
    fut <- rbind(realized_candles, future_candles[i, ])
    
    # Add SR lines -------------------------------------------------------------
    SR_vals <- SR_lines(roll = roll, data = fut, plot.it = FALSE)
  
    fut$SL[nrow(fut)] <- SR_vals$SL
    fut$RL[nrow(fut)] <- SR_vals$RL
    
    # Technical indicators -----------------------------------------------------
    
    # Support and resistance lines
    fut$crossover_support[nrow(fut)] <- ifelse(fut$close[nrow(fut)] <= fut$SL[nrow(fut)],
                                              "below_support", "above_support")
  
    fut$crossover_resistance[nrow(fut)] <- ifelse(fut$close[nrow(fut)] >= fut$RL[nrow(fut)],
                                               "above_resistance", "below_resistance")
    
    
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
         fut$crossover_support[nrow(fut)] == "below_support") {
      
      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
      
      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") & (
      fut$exit_condition[nrow(fut)] == TRUE | fut$crossover_resistance[nrow(fut)] == "above_resistance" )) {
      
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
    
    realized_candles <- fut
    print(i)
    # browser()
  }
  return(realized_candles)
}

View(realized_candles)
warnings()
unique(realized_candles$Price)
myresult <- realized_candles
# Close last position
if(myresult$action[nrow(myresult)] == "keep") {
  myresult$action[nrow(myresult)] <- "sell"
  myresult$Price[nrow(myresult)] <- myresult$close[nrow(myresult)] * myresult$Units[nrow(myresult)]
}

# Calculate profits
calculate_profits(myresult)
mytest <- myresult
idents <- unique(mytest$id)[!is.na(unique(mytest$id))]
par(mfrow = c(1, 1))

i <- 1
for (i in 1:length(idents)){
  
  h <- head(which(mytest$id == idents[i]),1) -200
  
  if(h < 0){
    h <- 1
  }
  t <- tail(which(mytest$id == idents[i]),1) + 200
  mytest <- myresult[h:t, ]
  ident <- idents[i]
  
  plot(1:nrow(mytest), mytest$close, type = "l")
  buyprice <- mytest$close[mytest$action =="buy" & mytest$id ==ident][!is.na(mytest$close[mytest$action =="buy" & mytest$id ==ident])]
  sellprice <- mytest$close[mytest$action =="sell" & mytest$id ==ident][!is.na(mytest$close[mytest$action =="sell" & mytest$id ==ident])]
  
  mtext(round((sellprice - buyprice)/buyprice, digits = 3), side = 3)
  
  points(which(mytest$action =="buy" & mytest$id ==ident), mytest$close[mytest$action =="buy" & mytest$id ==ident][!is.na(mytest$close[mytest$action =="buy" & mytest$id ==ident])], pch =19, col ="green")
  points(which(mytest$action =="sell"& mytest$id ==ident), mytest$close[mytest$action =="sell"& mytest$id ==ident][!is.na(mytest$close[mytest$action =="sell"& mytest$id ==ident])], pch =19, col ="red")
  
  abline(h = mytest$SL[mytest$action =="buy"& mytest$id ==ident][!is.na(mytest$SL[mytest$action =="buy"& mytest$id ==ident])], col = "green")
  abline(h = mytest$RL[mytest$action =="sell"& mytest$id ==ident][!is.na(mytest$RL[mytest$action =="sell"& mytest$id ==ident])], col = "red")
  
  # abline(h = 40)
  # 
  # plot(mytest$volume, type ="l")
  # lines(mytest$EMA_volume, col ="red")
  # points(which(mytest$action =="buy" & mytest$id ==ident), mytest$volume[mytest$action =="buy" & mytest$id ==ident][!is.na(mytest$volume[mytest$action =="buy" & mytest$id ==ident])], pch =19, col ="blue")
  # points(which(mytest$action =="sell"& mytest$id ==ident), mytest$volume[mytest$action =="sell"& mytest$id ==ident][!is.na(mytest$volume[mytest$action =="sell"& mytest$id ==ident])], pch =19, col ="black")
  # 
  
  mytest <- myresult
  print(i)
}
