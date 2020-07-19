df <- simple_OHLC(interval = 60, pair = "ETHEUR")
plot(df$close, type ="l")
plot_candlesticks(dta = df, Ns = nrow(df), asset = "ETHEUR")


