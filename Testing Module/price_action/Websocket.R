library(websocket)
library(RJSONIO)
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

quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 

x <- list( event = 'subscribe', pair  = as.array(pair),
           subscription = list(name="ohlc", interval = interval) )
json <- toJSON(x, pretty = T )



ws2 <- websocket::WebSocket$new("wss://ws.kraken.com/", autoConnect = FALSE) 

ws2$onMessage(function(event) {
  
  input_message <- jsonlite::fromJSON(event$data)
  
  if(length(input_message) > 2) {
    
    val <<- input_message[[2]][6]
  }

})

ws2$connect()
poll_until_connected(ws2)
ws2$send(json)
# ws2$close()


