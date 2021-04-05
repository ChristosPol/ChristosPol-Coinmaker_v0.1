rm(list = ls())
.rs.restartR()

# Source functions
path_source <- "/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1/Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)
path <- "/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1/Testing Module/price_action/"

# Choose pair and interval
pair <- "ETH/EUR"
interval = 60

# Initiate websocket stream --- In screen
# source("Websocket.R")

trades <- data.table(current_price = NA,price_action = NA, at = Sys.time(), action = "no_action", pos_perc = NA,
                     tp = 0.01, exit = NA, signal = NA)
all_trades <- data.table(current_price = NA, price_action = NA, at = Sys.time(), action = "no_action", pos_perc = NA,
                         tp = 0.01, exit = NA, signal = NA)

repeat{

  tryCatch({
    # print("reading last bar...")
    last_bar <- read.csv(paste0(path, "OHLC_last_bar.csv"))
    # print("reading last bar susccesful")
    last_close <- last_bar$close
    
    # print("reading val...")
    val <- read.csv(paste0(path, "val.csv"))
    # print("reading val susccesful")
    val <- val$x
    signal <- ((val - last_close) / val)*100
    
    if(signal < -1.5 & (all_trades[, tail(action, 1)] == "no_action"  | all_trades[, tail(action, 1)] == "sold")) {
      
      trades[, "price_action"] <- val
      trades[, "current_price"] <- val
      trades[, "at"] <- Sys.time()
      trades[, "action"] <- "long"
      trades[, "pos_perc"] <- 0
      
    } else if ( (tail(all_trades[, "pos_perc"], 1) > 1 | tail(all_trades[, "pos_perc"], 1) < -1.5) &  all_trades[, tail(action, 1)] %in% c("long","keep") ){
      trades[, "action"] <- "sold"
      trades[, "price_action"] <- all_trades[, tail(price_action, 1)]
      trades[, "current_price"] <- val
      trades[, "at"] <- Sys.time()
      trades[, "pos_perc"] <- ((all_trades[, tail(current_price, 1)]  - trades[, "price_action"] )/ all_trades[, tail(current_price, 1)])*100
      
    } else if (all_trades[, tail(action, 1)] %in% c("long", "keep") & ( tail(all_trades[, "pos_perc"],1) < 1)  ) {
      
      trades[, "action"] <- "keep"
      trades[, "current_price"] <- val
      trades[, "at"] <- Sys.time()
      trades[, "price_action"] <- all_trades[, tail(price_action, 1)]
      
      # trades[, "pos_perc"] <- ((all_trades[, tail(current_price, 1)]  - trades[, "price_action"] )/ all_trades[, tail(current_price, 1)] )*100
      trades[, "pos_perc"] <- ((val  - trades[, "price_action"] )/ val )*100
    } else {
      
      trades[, "action"] <- "no_action"
      trades[, "current_price"] <- val
      trades[, "at"] <- Sys.time()
      trades[, "pos_perc"] <- NA
    }
    
    all_trades <- rbind(all_trades, trades)
    all_trades$signal[nrow(all_trades)] <-  signal

  }, error = function(e) { return(NA) })
    print(all_trades)
  Sys.sleep(1)
}
# Close websocket connection
ws2$close()

