api_info <- read.table(paste("/media/chris/DATA/Documents/Bot_Trading", "API_Keys.txt", sep = "/"), sep = ";", header = T)

API_Key <- as.character(api_info$API_Key)
API_Sign <- as.character(api_info$API_Sign)
avail_pairs <- myfun("https://api.kraken.com/0/public/AssetPairs", secret = API_Sign, key = API_Key)
all_pairs <- names(avail_pairs[[2]])

# Get only EUR related crypto pairs
EUR_pairs <- grep("EUR", all_pairs, value = T)

# Remove Forex pairs
to_remove <- grep(paste(c("USD",
                          ".d",
                          "AUD",
                          "CAD",
                          "JPY",
                          "CHF",
                          "GBP"), collapse ="|"), EUR_pairs, value = T)
EUR_pairs <- EUR_pairs[!EUR_pairs %in% to_remove]

# Dynamic support and resistance
# Get OHLC data and determine trends

i <- 2
par(mfrow =c(2,2))
par(bg = 'grey')
value_price <- list()

for (i in 1:length(EUR_pairs)){
  msg <- tryCatch({
  df <- simple_OHLC(interval = 5, pair = EUR_pairs[i])
  df$sharpe <- mean(diff(df$close)) / sd(df$close)
  df$SMA_N <- SMA(df$close, n = 30)
  df$rsi <- RSI(df$close, n = 14)
  value_price[[i]] <- (df$close[nrow(df)] - df$SMA_200[nrow(df)])/df$close[nrow(df)] 
  
  SR_lines(data = df, roll = 200, n_sort = 5, pair = EUR_pairs[i], Ns = nrow(df))
  abline(h = df$close[nrow(df)], lty = "dashed", col = "blue")
  
  bollinger_bands(periods = 20,times_sd = 2.5, data = df)
  df$macd <- MACD(df$close)[, 1]
  df$signal <- MACD(df$close)[, 2]
  plot(df$macd, type = "l")
  abline(h = 0 , col ="red", lty = "dashed")
  lines(df$signal)
  plot(df$rsi, type = "l")
  abline(h = 30, lty ="dashed")
  abline(h = 70, lty ="dashed")
  }, error = function(e){
  })
  print(i/length(EUR_pairs))
  print(paste0("Sharpe Ratio for: ",  EUR_pairs[i]," ", round(unique(df$sharpe), 4))) 
  Sys.sleep(2)
  
}
names(value_price) <- EUR_pairs

sleep <-1
par(mfrow=c(1,1))
repeat{
  Sys.sleep(sleep)

    df <- simple_OHLC(interval = 1, pair = "BTCEUR")
    df$SMA_N <- SMA(df$close, n = 30)
    df$rsi <- RSI(df$close, n = 14)
    value_price[[i]] <- (df$close[nrow(df)] - df$SMA_200[nrow(df)])/df$close[nrow(df)] 
    
    SR_lines(data = df, roll = nrow(df), n_sort = 10, pair = EUR_pairs[i], Ns = 50)
    abline(h = df$close[nrow(df)], lty = "dashed", col = "blue")
    
    bollinger_bands(periods = 20,times_sd = 2.5, data = tail(df,50))
    df$macd <- MACD(df$close)[, 1]
    df$signal <- MACD(df$close)[, 2]
    plot(tail(df$macd, 50), type = "l")
    abline(h = 0 , col ="red", lty = "dashed")
    # lines(df$signal)
    plot(tail(df$rsi, 50), type = "l")
    abline(h = 30, lty ="dashed")
    abline(h = 70, lty ="dashed")
    flush.console()
}

n=1000
df=data.frame(time=1:n,y=runif(n))
window=100
for(i in 1:(n-window)) {
  flush.console()
  plot(df$time,df$y,type='l',xlim=c(i,i+window))
  Sys.sleep(.09)
}

