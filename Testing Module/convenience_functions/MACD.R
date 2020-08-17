# Badget 
initial_budget <- 500

# select period of data 
candles_recent <- candles
# 
# training data here is not the traditional training set, but more of a 
# placeholder initial dataset to calculate the indicators
train_n <- ceiling(nrow(candles_recent) / 2)
train_data <- candles_recent[1:train_n, ]

# Test, same 
test_data <- candles_recent[(train_n + 1):nrow(candles_recent), ]
test_data <- test_data[1:5000,]
takeprofit <-0.1
stoploss_trail <-0.02
stoploss_ult <-0.02
i <-1
par(mfrow=c(2,1))

bollinger_bands <- function(periods, times_sd, data){
  df <- data
  plot(df$close[-c(1:(periods-1))], type ="l", lwd =2)
  lines(SMA(df$close, n=periods)[-c(1:(periods-1))] + times_sd*rollapply(df$close, periods, sd), col ="red")
  lines(SMA(df$close, n=periods)[-c(1:(periods-1))] - times_sd*rollapply(df$close, periods, sd), col ="green")
}

bollinger_bands(periods = 60,  times_sd = 3, data = test_data)
unique(rollapplyr(data$close[-nrow(data)], roll, max, fill = NA))
# Strategy using volumes spikes and RSI oversold conditions
Pure_RSI_Volume_Trailing <- function(takeprofit, stoploss_trail, stoploss_ult) {
  
  # Train and test datasets
  train_data[, c("macd",
                 "exit_value",
                 "exit_condition",
                 "crossover_MACD",
                 "action",
                 "Units",
                 "Price",
                 "tp",
                 "ult_sl",
                 "trail_sl",
                 "id") := list(NA,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]
  
  test_data[, c("macd",
                "exit_value",
                "exit_condition",
                "crossover_MACD",
                "action",
                "Units",
                "Price",
                "tp",
                "ult_sl",
                "trail_sl",
                "id") := list(NA,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]
  
  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){
    
    fut <- rbind(train_data, test_data[i, ])
    
    # RSI and Volume
    
    fut$macd <- MACD(fut$close)[, 1]
    
    # MFI Crossing of upper or lower bounds
    fut$crossover_MACD[nrow(fut)] <- ifelse(fut$macd[nrow(fut)] > 0 ,
                                           "MACD_higher", "MACD_lower")
    
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
    if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){
      
      trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      if( trail_sl < tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)){
        trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
        
      } else {
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
    
    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl
    fut$trail_sl[nrow(fut)] <- trail_sl
    
    
    fut$exit_condition[nrow(fut)] <- fut$trail_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]
    
    
    # Deciding upon action -----------------------------------------------------
    
    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         fut$crossover_MACD[nrow(fut)] == "MACD_higher") {
      
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


myresult <- train_data


# Close last position
if(myresult$action[nrow(myresult)] == "keep") {
  myresult$action[nrow(myresult)] <- "sell"
  myresult$Price[nrow(myresult)] <- myresult$close[nrow(myresult)] * myresult$Units[nrow(myresult)]
}

# Calculate profits
calculate_profits(myresult)

# how many trades were succesful (to be functioned)
mah <- subset(myresult,myresult$action %in% c("buy","sell") )
profitable_trades <- list()
ids_s <- unique(mah$id)
for(i in 1:length(unique(mah$id))){
  
  profitable_trades[[i]] <- mah$close[mah$id == ids_s[i] & mah$action =="sell"]-mah$close[mah$id == ids_s[i] & mah$action =="buy"]
}
table(unlist(profitable_trades) > 0)

# Plot each trade's indicators and price action (to be functioned)
mytest <- myresult
idents <- unique(mytest$id)[!is.na(unique(mytest$id))]
par(mfrow = c(3, 1))

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
  
  plot(mytest$macd, col ="blue", type ="l")#, ylim = c(mn, mx))
  points(which(mytest$action =="buy" & mytest$id ==ident), mytest$macd[mytest$action =="buy" & mytest$id ==ident][!is.na(mytest$macd[mytest$action =="buy" & mytest$id ==ident])], pch =19, col ="blue")
  points(which(mytest$action =="sell"& mytest$id ==ident), mytest$macd[mytest$action =="sell"& mytest$id ==ident][!is.na(mytest$macd[mytest$action =="sell"& mytest$id ==ident])], pch =19, col ="black")
  abline(h = 0, lty ="dashed")
  
  plot(mytest$volume, type ="l")
  # lines(mytest$EMA_volume, col ="red")
  points(which(mytest$action =="buy" & mytest$id ==ident), mytest$volume[mytest$action =="buy" & mytest$id ==ident][!is.na(mytest$volume[mytest$action =="buy" & mytest$id ==ident])], pch =19, col ="blue")
  points(which(mytest$action =="sell"& mytest$id ==ident), mytest$volume[mytest$action =="sell"& mytest$id ==ident][!is.na(mytest$volume[mytest$action =="sell"& mytest$id ==ident])], pch =19, col ="black")
  
  
  mytest <- myresult
  print(i)
}
