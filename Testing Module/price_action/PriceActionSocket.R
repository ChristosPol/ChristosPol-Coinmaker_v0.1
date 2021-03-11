rm(list = ls())
.rs.restartR()

# Source functions
path_source <- "/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1/Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)

# Choose pair and interval
pair <- "ETH/EUR"
interval = 60

# Initiate websocket stream
source("Websocket.R")

trades <- data.table(current_price = NA,price_action = NA, at = NA, action = "no_action", pos_perc = NA,
                     tp = 0.01, exit = NA, signal  =NA)
all_trades <- data.table(current_price = NA, price_action = NA, at = NA, action = "no_action", pos_perc = NA,
                         tp = 0.01, exit = NA, signal = NA)

OHLC <- simple_OHLC(interval, pair)

previous_bar <- OHLC[nrow(OHLC)-1, ] 

signal <- ((as.numeric(as.character(val))-previous_bar$close) / as.numeric(as.character(val)))*100

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

# Close websocket connection
ws2$close()

