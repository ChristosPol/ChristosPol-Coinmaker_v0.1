candles
roll <- 100
candles <- simple_OHLC(interval = 240, pair = "DASHEUR")
candles$RSI <- RSI(candles$close, n =14)
plot_candlesticks(dta = candles, Ns = nrow(candles), asset = pair)

last_close <- candles$close[nrow(candles)]
resistance <- unique(rollapplyr(candles$close, roll, max, fill = NA))

if (length(which(resistance < last_close)) == 0) {
  resistance_act <- resistance
} else {
  resistance_act <- resistance[-which(resistance < last_close)]
}

support <- unique(rollapplyr(candles$close, roll, min, fill = NA))

if (length(which(support > last_close)) == 0) {
  support_act <- support
} else {
  support_act <- support[-which(support > last_close)]
}


abline(h = resistance_act, col ="lightblue")
abline(h = support_act, col ="grey")
candles$RSI
