candles
roll <- 100
# candles <- simple_OHLC(interval = 30, pair = "REPEUR")
# candles$RSI <- RSI(candles$close, n =14)
real <- ceiling(nrow(candles) / 10)
realized_candles <- candles[1:real, ]



SL_lines <- function(roll, data){
  
  last_close <- data$close[nrow(data)]
  resistance <- unique(rollapplyr(data$close[-nrow(data)], roll, max, fill = NA))
  
  if (length(which(resistance < last_close)) == 0) {
    resistance_act <- min(resistance, na.rm = T)
  } else {
    resistance_act <- min(resistance[-which(resistance < last_close)], na.rm = T)
  }
  
  support <- unique(rollapplyr(data$close[-nrow(data)], roll, min, fill = NA))
  
  if (length(which(support > last_close)) == 0) {
    support_act <- max(support, na.rm =T)
  } else {
    support_act <- max(support[-which(support > last_close)], na.rm = T)
  }
  
  plot_candlesticks(dta = data, Ns = nrow(data), asset = pair)
  abline(h = resistance_act, col ="lightblue")
  abline(h = support_act, col ="black")
  return(list(SL = support_act, RL = resistance_act))
}
SL_lines(roll = 500, data=candles)

# Test, same 
future_candles <- candles[(real + 1):nrow(candles), ]


candles$RSI

