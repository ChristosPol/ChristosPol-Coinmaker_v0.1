# Connect through the websocket of kraken to get live stream of prices
screen -S websocket R

library(websocket)
library(RJSONIO)
library(utils)

# Choose pair and interval
pair <- c("ETH/EUR", "ADA/EUR", "BTC/EUR", "DOT/EUR", "XRP/EUR", "LINK/EUR", "LTC/EUR")
interval = 60

# Path to write values
path <- "/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1/Testing Module/price_action/current_values/"

# Handler function
poll_until_connected <- function(ws, timeout = 5) {
  connected <- FALSE
  end <- Sys.time() + timeout
  while (!connected && Sys.time() < end) {
    # Need to run the event loop for websocket to complete connection.
    later::run_now(0.1)
    
    ready_state <- ws$readyState()
    if (ready_state == 0L) {
      # 0 means we're still trying to connect.
      # For debugging, indicate how many times we've done this.
      cat(".")         
    } else if (ready_state == 1L) {
      connected <- TRUE
    } else {
      break
    }
  }
  
  if (!connected) {
    stop("Unable to establish websocket connection.")
  }
}

# Create json to send
x <- list( event = 'subscribe', pair  = as.array(pair),
           subscription = list(name="ohlc", interval = interval) )
json <- toJSON(x, pretty = T )

# Connect to websocket
ws2 <- websocket::WebSocket$new("wss://ws.kraken.com/", autoConnect = FALSE) 

# Action on message
ws2$onMessage(function(event) {
  
  input_message <- jsonlite::fromJSON(event$data)
  # print(input_message)
  if (input_message[[1]][1] == 571) {
    write.csv(input_message[[2]][6], paste0(path ,"ETHEUR_","val.csv"))
    print( paste0(input_message[[2]][6], " _ETHEUR"))
  } else if (input_message[[1]][1] == 1483){
    write.csv(input_message[[2]][6], paste0(path ,"ADAEUR_","val.csv"))
    print( paste0(input_message[[2]][6], " _ADAEUR"))
  } else if (input_message[[1]][1] == 347){
    write.csv(input_message[[2]][6], paste0(path ,"BTCEUR_","val.csv"))
    print( paste0(input_message[[2]][6], " _BTCEUR"))
  } else if (input_message[[1]][1] == 2955){
    write.csv(input_message[[2]][6], paste0(path ,"DOTEUR_","val.csv"))
    print( paste0(input_message[[2]][6], " _DOTEUR"))
  } else if (input_message[[1]][1] == 923){
    write.csv(input_message[[2]][6], paste0(path ,"XRPEUR_","val.csv"))
    print( paste0(input_message[[2]][6], " _XRPEUR"))
  } else if (input_message[[1]][1] == 1995){
    write.csv(input_message[[2]][6], paste0(path ,"LINKEUR_","val.csv"))
    print( paste0(input_message[[2]][6], " _LINKEUR"))
  } else if (input_message[[1]][1] == 443){
    write.csv(input_message[[2]][6], paste0(path ,"LTCEUR_","val.csv"))
    print(paste0(input_message[[2]][6], " _LTCEUR"))
  }
})

# send the call
ws2$connect()
poll_until_connected(ws2)
ws2$send(json)
# ws2$close()


