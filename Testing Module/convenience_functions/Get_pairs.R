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
# result = tryCatch({
#   expr
# }, warning = function(w) {
#   warning-handler-code
# }, error = function(e) {
#   error-handler-code
# }, finally = {
#   cleanup-code
# }
# Get OHLC data and determine trends
i <- 6
value_price <- list()

for (i in 1:length(EUR_pairs)){
  msg <- tryCatch({
  df <- simple_OHLC(interval = 1440, pair = EUR_pairs[i])
  df$SMA_200 <- SMA(df$close, n =100)
  
  value_price[[i]] <- (df$close[nrow(df)] - df$SMA_200[nrow(df)])/df$close[nrow(df)] 
  }, error = function(e){
  })
  # df$day <- substr(df$Date_POSIXct, 1,10 )
  # last_day_close <- df$close[df$day %in% tail(unique(df$day), 2)] 
  # (tail(last_day_close, 1) - last_day_close[1]) / last_day_close[1]
  # 
  # 
  # tt<- EMA(df$close, n = 200)
  print(i/length(EUR_pairs))
  Sys.sleep(2)
  
}
names(value_price) <- EUR_pairs

# pair_compared_SMA <- sort(unlist(value_price))
sort(unlist(value_price))

trade_pairs <- pair_compared_SMA[pair_compared_SMA<0][1:5]

initial_budget <- 500
divided_budget <- initial_budget/length(trade_pairs)

# Need to give market orders for these 5 coins
for (i in 1:length(trade_pairs)) {
  
  last_closed_price <- myfun(paste0("https://api.kraken.com/0/public/Ticker?pair=", names(trade_pairs[1])), secret = API_Sign, key = API_Key)
  last_closed_price <- as.numeric(last_closed_price$result$OXTEUR$c[1])
  buy_it <- add_market_order(url = "https://api.kraken.com/0/private/AddOrder",
                           key = API_Key, secret = API_Sign, pair = names(trade_pairs[1]), type = "buy",
                           ordertype = "market", volume = divided_budget/last_closed_price)
}

# Review your market orders


myfun("https://api.kraken.com/0/private/ClosedOrders", secret = API_Sign, key = API_Key)
