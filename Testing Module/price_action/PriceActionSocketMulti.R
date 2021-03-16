rm(list = ls())
.rs.restartR()

# Source functions
path_source <- "/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1/Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)
path_values <- "/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1/Testing Module/price_action/current_values/"
path_ohlc <- "/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1/Testing Module/price_action/ohlc/"

# Choose pair and interval
pair <- c("ETH/EUR", "ADA/EUR")
interval = 60

# Initiate websocket stream --- In screen
# source("Websocket.R")

trades <- data.table(current_price = as.numeric(c(NA, NA)), price_action = as.numeric(c(NA, NA)), at = c(Sys.time(),Sys.time()),
                     action = c("no_action", "no_action"), pos_perc = as.numeric(c(NA, NA)),
                     exit = c(NA, NA), signal = c(NA, NA), pair  = pair)
all_trades <- data.table(current_price = as.numeric(c(NA, NA)), price_action = as.numeric(c(NA, NA)), at = c(Sys.time(),Sys.time()),
                         action = c("no_action", "no_action"), pos_perc = as.numeric(c(NA, NA)),
                         exit = c(NA, NA), signal = c(NA, NA), pair  = pair)

repeat{
  
  tryCatch({
    
    # signals <- data.frame(pair = NA, signal = NA)
    for(i in 1:length(pair)){
      
      last_bar <- read.csv(paste0(path_ohlc, "OHLC_last_bar_", gsub("/", "-", pair[i]), ".csv"))
      last_close <- last_bar$close
      val <- read.csv(paste0(path_values, gsub("/", "", pair[i]) ,"_val.csv"))
      val <- val$x
      signal <- ((val - last_close) / val) * 100
      # signals[i, "pair"] <- pair[i]
      # signals[i, "signal"] <- signal
    
      if(signal < -1.5 & (all_trades[pair == pair[i], tail(action, 1)] == "no_action"  | all_trades[pair == pair[i], tail(action, 1)] == "sold")) {
        
        trades[pair == pair[i], "price_action"] <- val
        trades[pair == pair[i], "current_price"] <- val
        trades[pair == pair[i], "at"] <- Sys.time()
        trades[pair == pair[i], "action"] <- "long"
        trades[pair == pair[i], "pos_perc"] <- 0
        
      } else if ((tail(all_trades[pair == pair[i], "pos_perc"], 1) > 1 | tail(all_trades[pair == pair[i], "pos_perc"], 1) < -1.5) &  all_trades[pair == pair[i], tail(action, 1)] %in% c("long","keep") ){
        
        trades[pair == pair[i], action := "sold"]
        trades[pair == pair[i], price_action := all_trades[pair == pair[i], tail(price_action, 1)]]
        trades[pair == pair[i], current_price := val]
        trades[pair == pair[i], at := Sys.time()]
        trades[pair == pair[i], pos_perc := ((all_trades[pair == pair[i], tail(current_price, 1)]  - trades[pair == pair[i], "price_action"] )/ all_trades[pair == pair[i], tail(current_price, 1)])*100] 
        
      } else if (all_trades[pair == pair[i], tail(action, 1)] %in% c("long", "keep") & ( tail(all_trades[pair == pair[i], "pos_perc"],1) < 1)  ) {
        
        trades[pair == pair[i], action := "keep"]
        trades[pair == pair[i], current_price := val]
        trades[pair == pair[i], at := Sys.time()]
        trades[pair == pair[i], price_action := all_trades[pair == pair[i], tail(price_action, 1)]]
        trades[pair == pair[i], pos_perc := ((val  - trades[pair == pair[i], "price_action"] )/ val )*100]
      
      } else {
        
        trades[pair == pair[i], action := "no_action"]
        trades[pair == pair[i], current_price := val]
        trades[pair == pair[i], at := Sys.time()] 
        trades[pair == pair[i], pos_perc := NA]
      }
      
    
    all_trades <- rbind(all_trades, trades[pair == pair[i]])
    all_trades$signal[all_trades$pair == pair[i]][nrow(all_trades[all_trades$pair == pair[i]])] <- signal
    
    }
    
  }, error = function(e) { return(NA) })
  print(all_trades)
  Sys.sleep(1)
}
# Close websocket connection
ws2$close()

