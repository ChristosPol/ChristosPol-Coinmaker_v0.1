EMA_periods<- 10
takeprofit<- 0.08
stoploss <- 0.03
rsi_bound <- 40
periods_volume <- 20
rsi_period  <- 14

Volume_Reversal_RSI_NJ <- function(takeprofit, stoploss, rsi_bound, rsi_period, periods_volume, times_EMA_Vol, EMA_periods) {
  
  # Train and test datasets
  train_data[, c("exit_value",
                 "exit_condition",
                 "EMA_volume",
                 "EMA",
                 "RSI",
                 "crossover_volume",
                 "candle_pattern",
                 "Long_singal",
                 "action",
                 "Units",
                 "Price",
                 "id") := list(0,NA, NA,NA,NA,NA, NA, 0, NA, NA, NA, NA) ]
  
  test_data[, c("exit_value",
                "exit_condition",
                "EMA_volume",
                "EMA",
                "RSI",
                "crossover_volume",
                "candle_pattern",
                "Long_singal",
                "action",
                "Units",
                "Price",
                "id") := list(0, NA,NA,NA,NA,NA, NA, 0, NA, NA, NA, NA) ]
  
  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){
    
    fut <- rbind(train_data, test_data[i, ])
    
    
    # Technical indicators -----------------------------------------------------
    
    
    fut$EMA_volume <- EMA(fut$volume, periods_volume)
    fut$crossover_volume[nrow(fut)-1] <-  ifelse(fut$volume[nrow(fut)-1] >  fut$EMA_volume[nrow(fut)-1]*times_EMA_Vol, "Volume_Spike","No_Spike")
    # fut$crossover_volume[nrow(fut)-1] <- ifelse(fut$volume[nrow(fut)-1] >  max(fut$volume[(nrow(fut)-periods_volume): (nrow(fut)-2)]), "Volume_Spike","No_Spike")
    fut$RSI <- RSI(fut$close, n = rsi_period)
    
    fut$candle_pattern[nrow(fut)] <-  (fut$low[nrow(fut)-2] <= fut$low[nrow(fut)-1]) & (fut$low[nrow(fut)-2] <= fut$low[nrow(fut)]) & 
    (fut$close[nrow(fut)-2] >= fut$close[nrow(fut)-1]) & (fut$close[nrow(fut)-2] >= fut$open[nrow(fut)-2]) &
    (fut$close[nrow(fut)-2] - fut$open[nrow(fut)-2]) < (fut$high[nrow(fut)-2] - fut$low[nrow(fut)-2])
    
    
    fut$Long_singal[fut$candle_pattern[nrow(fut)] == TRUE & fut$crossover[nrow(fut)-1] =="Volume_Spike"] <- TRUE
    
    fut$EMA <- EMA(fut$close, n = EMA_periods)
    
    # Exit condition for stop loss 
    exit_value <- (fut$close[nrow(fut)] - tail(fut$close[!is.na(fut$action) &fut$action =="buy"], 1)) / tail(fut$close[!is.na(fut$action) &fut$action =="buy"], 1)
    
    if (length(exit_value) == 0) {
      exit_value <- 0
    }
    
    fut$exit_value[nrow(fut)] <- exit_value
    fut$exit_condition <- fut$exit_value > takeprofit | fut$exit_value <= -stoploss
    
    
    
    # Deciding upon action -----------------------------------------------------
    
    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         fut$Long_singal[nrow(fut)] == TRUE & fut$RSI[nrow(fut)] < rsi_bound) {
      
      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
      
      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") &
               fut$exit_condition[nrow(fut)] == TRUE ) {
      
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
    # print(i)
  }
  return(train_data)
}


calculate_profits(train_data)

mah <- subset(train_data,train_data$action %in% c("buy", "sell") )
mah <- mah[-nrow(mah),]
View(mah)
profitable_trades <- list()
ids_s <- unique(mah$id)
for(i in 1:length(unique(mah$id))){
  
  profitable_trades[[i]] <- mah$close[mah$id == ids_s[i] & mah$action =="sell"]-mah$close[mah$id == ids_s[i] & mah$action =="buy"]
}

table(unlist(profitable_trades) > 0)



rm(mytest)
mytest <- train_data
# bad_id <- names(which(table(mytest[mytest$action %in% c("buy", "sell"), ]$id) ==1))
# idents <- unique(mytest$id[mytest$id !=bad_id])[!is.na(unique(mytest$id[mytest$id !=bad_id]))]
idents <- unique(mytest$id)[!is.na(unique(mytest$id))]
par(mfrow = c(3, 1))

i <-1

for (i in 1:length(idents)){
  
  h <- head(which(mytest$id == idents[i]),1) -50 
  
  if(h < 0){
    h <- 1
  }
  t <- tail(which(mytest$id == idents[i]),1) + 50
  mytest <- train_data[h:t, ]
  ident <- idents[i]
  
  # plot(1:nrow(mytest), mytest$close, type = "l")
  plot_candlesticks(dta = mytest, Ns = nrow(mytest), asset = pair)
  
  lines(mytest$EMA, col = "black")
  
  buyprice <- mytest$close[mytest$action =="buy" & mytest$id ==ident][!is.na(mytest$close[mytest$action =="buy" & mytest$id ==ident])]
  sellprice <- mytest$close[mytest$action =="sell" & mytest$id ==ident][!is.na(mytest$close[mytest$action =="sell" & mytest$id ==ident])]
  
  mtext(round((sellprice - buyprice)/buyprice, digits = 3), side = 3)
  
  # plot_candlesticks(dta = mytest, Ns = nrow(mytest), a  sset = pair)
  
  points(which(mytest$action =="buy" & mytest$id ==ident), mytest$close[mytest$action =="buy" & mytest$id ==ident][!is.na(mytest$close[mytest$action =="buy" & mytest$id ==ident])], pch =19, col ="blue")
  points(which(mytest$action =="sell"& mytest$id ==ident), mytest$close[mytest$action =="sell"& mytest$id ==ident][!is.na(mytest$close[mytest$action =="sell"& mytest$id ==ident])], pch =19, col ="black")
  
  abline(h = mytest$close[mytest$action =="buy" & mytest$id ==ident][!is.na(mytest$close[mytest$action =="buy" & mytest$id ==ident])], col ="blue")
  abline(h = mytest$close[mytest$action =="sell" & mytest$id ==ident][!is.na(mytest$close[mytest$action =="sell" & mytest$id ==ident])], col ="black")
  
  # plot(mytest$volume, col ="red", type ="l")#, ylim = c(mn, mx))
  # lines(mytest$EMA_65, col ="blue")#, ylim = c(mn, mx))
  
  plot(mytest$volume, type ="l")
  # lines(mytest$sd, col ="green")
  points(which(mytest$action =="buy" & mytest$id ==ident), mytest$volume[mytest$action =="buy" & mytest$id ==ident][!is.na(mytest$volume[mytest$action =="buy" & mytest$id ==ident])], pch =19, col ="blue")
  points(which(mytest$action =="sell"& mytest$id ==ident), mytest$volume[mytest$action =="sell"& mytest$id ==ident][!is.na(mytest$volume[mytest$action =="sell"& mytest$id ==ident])], pch =19, col ="black")
  lines(mytest$EMA_volume, col ="grey")
  # lines(mytest$EMA_130, col ="blue")
  # plot(mytest$macd, type ="l")
  # lines(mytest$signal, col ="red")
  # abline(h = 0)
  # abline(h = 40)
  # points(which(mytest$action =="buy" & mytest$id ==ident), mytest$macd[mytest$action =="buy" & mytest$id ==ident][!is.na(mytest$macd[mytest$action =="buy" & mytest$id ==ident])], pch =19, col ="blue")
  # points(which(mytest$action =="sell"& mytest$id ==ident), mytest$macd[mytest$action =="sell"& mytest$id ==ident][!is.na(mytest$macd[mytest$action =="sell"& mytest$id ==ident])], pch =19, col ="black")
  # 
  # lines(mytest$Volume_SMA_5)
  # abline(v = which(mytest$action =="buy" & mytest$id ==ident))
  plot(mytest$RSI, type ="l")
  # abline(h = 30)
  abline(h = 60, col ="red")
  mytest <- train_data
  print(i)
}
  
