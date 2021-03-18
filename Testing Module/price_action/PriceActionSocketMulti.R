rm(list = ls())
.rs.restartR()

# Source functions
path_source <- "/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1/Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)
path_values <- "/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1/Testing Module/price_action/current_values/"
path_ohlc <- "/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1/Testing Module/price_action/ohlc/"

# Choose pair and interval
pair <- c("ETH/EUR", "ADA/EUR", "BTC/EUR", "DOT/EUR", "XRP/EUR", "LINK/EUR", "LTC/EUR")
vol <- c(0.018, 25, 0.0005, 0.92, 71, 1, 0.16)
interval = 60

# API info
api_info <- read.table(paste("/media/chris/DATA/Documents/Bot_Trading", "API_Keys.txt", sep = "/"), sep = ";", header = T)
API_Key <- as.character(api_info$API_Key)
API_Sign <- as.character(api_info$API_Sign)


# Initiate websocket stream --- In screen
# source("Websocket.R")

trades <- data.table(current_price = as.numeric(rep(NA, length(pair))), price_action = as.numeric(rep(NA, length(pair))), at = rep(Sys.time(), length(pair)),
                     action = rep("no_action", length(pair)), pos_perc = as.numeric(rep(NA, length(pair))),
                     exit = rep(NA, length(pair)), signal = rep(NA, length(pair)), pair  = pair)
all_trades <- data.table(current_price = as.numeric(rep(NA, length(pair))), price_action = as.numeric(rep(NA, length(pair))), at = rep(Sys.time(), length(pair)),
                         action = rep("no_action", length(pair)), pos_perc = as.numeric(rep(NA, length(pair))),
                         exit = rep(NA, length(pair)), signal = rep(NA, length(pair)), pair  = pair)

repeat{
  
  tryCatch({
    
    # signals <- data.frame(pair = NA, signal = NA)
    for(i in 1:length(pair)){
      
      last_bar <- read.csv(paste0(path_ohlc, "OHLC_last_bar_", gsub("/", "-", pair[i]), ".csv"))
      last_close <- last_bar$close
      val <- read.csv(paste0(path_values, gsub("/", "", pair[i]) ,"_val.csv"))
      val <- val$x
      signal <- ((val - last_close) / val) * 100

    
      if(signal < -3 & (all_trades[pair == pair[i], tail(action, 1)] == "no_action"  | all_trades[pair == pair[i], tail(action, 1)] == "sold")) {
        
        # Give API Order to buy at market
        buy_it <- add_market_order(url = "https://api.kraken.com/0/private/AddOrder",
                                   key = API_Key, secret = API_Sign, pair = pair[i], type = "buy",
                                   ordertype = "market", volume = vol[i])
        
        trades[pair == pair[i], "price_action"] <- val
        trades[pair == pair[i], "current_price"] <- val
        trades[pair == pair[i], "at"] <- Sys.time()
        trades[pair == pair[i], "action"] <- "long"
        trades[pair == pair[i], "pos_perc"] <- 0
        
      } else if ((tail(all_trades[pair == pair[i], "pos_perc"], 1) > 2 | tail(all_trades[pair == pair[i], "pos_perc"], 1) < -3) &  all_trades[pair == pair[i], tail(action, 1)] %in% c("long","keep") ){
        
        # # Give API Order to buy at market
        sell_it <- add_market_order(url = "https://api.kraken.com/0/private/AddOrder",
                                    key = API_Key, secret = API_Sign, pair = pair[i], type = "sell",
                                    ordertype = "market", volume = vol[i])
        
        trades[pair == pair[i], action := "sold"]
        trades[pair == pair[i], price_action := all_trades[pair == pair[i], tail(price_action, 1)]]
        trades[pair == pair[i], current_price := val]
        trades[pair == pair[i], at := Sys.time()]
        trades[pair == pair[i], pos_perc := ((all_trades[pair == pair[i], tail(current_price, 1)]  - trades[pair == pair[i], "price_action"] )/ all_trades[pair == pair[i], tail(current_price, 1)])*100] 
        
      } else if (all_trades[pair == pair[i], tail(action, 1)] %in% c("long", "keep") & ( tail(all_trades[pair == pair[i], "pos_perc"],1) < 2)  ) {
        
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
    
    mytrades <- all_trades[action %in% c("long", "sold")]
    if(nrow(mytrades) > 1){
      write.csv(mytrades, "trades.csv")  
    }
    
  }, error = function(e) { return(NA) })
  print(all_trades)
  Sys.sleep(2)
  
}
# Close websocket connection
ws2$close()
