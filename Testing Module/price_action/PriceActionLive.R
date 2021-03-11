rm(list = ls())
.rs.restartR()

# Source functions
path_source <- "/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1/Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)

# Choose any pair to pull
pair <- "XETHZEUR"
interval = 1


trades <- data.table(current_price = NA,price_action = NA, at = NA, action = "no_action", pos_perc = NA,
                     tp = 0.01, exit = NA, signal  =NA)
all_trades <- data.table(current_price = NA, price_action = NA, at = NA, action = "no_action", pos_perc = NA,
                         tp = 0.01, exit = NA, signal = NA)


repeat {
  OHLC <- simple_OHLC(interval, pair)
  OHLC[, diff := ((OHLC[, tail(close, 1)] - OHLC[, tail(close, 2)][1]) / OHLC[, tail(close, 1)])*100]
  signal <- OHLC[, tail(diff, 1)]
  
  if(signal < -1.20 & (all_trades[, tail(action, 1)] == "no_action"  | all_trades[, tail(action, 1)] == "sold")) {
    
    trades[, "price_action"] <- OHLC[, tail(close, 1)]
    trades[, "current_price"] <- OHLC[, tail(close, 1)]
    trades[, "at"] <- OHLC[, tail(Date_POSIXct, 1)]
    trades[, "action"] <- "long"
    trades[, "pos_perc"] <- ((all_trades[, tail(current_price, 1)]  - trades[, "price_action"] )/ all_trades[, tail(current_price, 1)] )*100
    
  } else if ( (tail(all_trades[, "pos_perc"], 1) > 1.20 | tail(all_trades[, "pos_perc"], 1) < -3) &  all_trades[, tail(action, 1)] %in% c("long","keep") ){
    trades[, "action"] <- "sold"
    trades[, "price_action"] <- all_trades[, tail(price_action, 1)]
    trades[, "current_price"] <- OHLC[, tail(close, 1)]
    trades[, "at"] <- OHLC[, tail(Date_POSIXct, 1)]
    trades[, "pos_perc"] <- ((all_trades[, tail(current_price, 1)]  - trades[, "price_action"] )/ all_trades[, tail(current_price, 1)])*100
  
  } else if (all_trades[, tail(action, 1)] %in% c("long", "keep") & ( tail(all_trades[, "pos_perc"],1) < 1.20)  ) {
    
    trades[, "action"] <- "keep"
    trades[, "current_price"] <- OHLC[, tail(close, 1)]
    trades[, "at"] <- OHLC[, tail(Date_POSIXct, 1)]
    trades[, "price_action"] <- all_trades[, tail(price_action, 1)]
    
    trades[, "pos_perc"] <- ((all_trades[, tail(current_price, 1)]  - trades[, "price_action"] )/ all_trades[, tail(current_price, 1)] )*100
    
  } else {
    
    trades[, "action"] <- "no_action"
    trades[, "current_price"] <- OHLC[, tail(close, 1)]
    trades[, "at"] <- OHLC[, tail(Date_POSIXct, 1)]
    trades[, "pos_perc"] <- NA
  }
  
  all_trades <- rbind(all_trades, trades)
  all_trades$signal[nrow(all_trades)] <-  signal
  # x <- ((copied1[, tail(close, 1)]- copied1[, tail(close, 2)][1]) / copied1[, tail(close, 1)])*100
  print(all_trades)
  
  plot_candlesticks(dta = OHLC, Ns = 50, asset = pair)
  
  plot_df <- all_trades[!is.na(all_trades$at)]
  
  
  if("long" %in% plot_df$action){
    
    
    df_points_buy <- data.frame(x = which(na.omit(unique(OHLC$Date_POSIXct)) %in% plot_df$at[plot_df$action == "long"]),
                                y = plot_df$price_action[plot_df$action == "long"])
  }
  if("sold" %in% plot_df$action){
    df_points_sell <- data.frame(x = which(unique(OHLC$Date_POSIXct) %in% plot_df$at[plot_df$action == "sold"]),
                                 y = plot_df$current_price[plot_df$action == "sold"])
  }
  if(exists("df_points_buy")){
    points(df_points_buy$x, df_points_buy$y, col = "blue", pch = 19)
  }
  if(exists("df_points_sell")){
    points(df_points_sell$x, df_points_sell$y, col = "red", pch = 19)
  }
  
  Sys.sleep(3)
}
