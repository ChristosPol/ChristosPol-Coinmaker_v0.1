Sys.sleep(15)
rm(list = ls())
# screen -S Live_Trading R

source(paste("/media/chris/DATA/Documents/Bot_Trading/Coinmaker", "10 Utils.R", sep = "/"))

# Load optimized parameters from location
params <- read.table("/media/chris/DATA/Documents/Bot_Trading/Coinmaker/Parameter optimization/Parameters/XETHZEUR/best params.csv",
                     header = TRUE,
                     sep = ",", stringsAsFactors = FALSE)

# Set parameters
fast_EMA = params$fast_EMA[nrow(params)]
slow_EMA = params$slow_EMA[nrow(params)]
takeprofit = params$takeprofit[nrow(params)]
stoploss_trail = params$stoploss_trail[nrow(params)]
stoploss_ult = params$stoploss_ult[nrow(params)]
options(digits = 5)
interval <- 15

# API info
api_info <- read.table(paste("/media/chris/DATA/Documents/Bot_Trading", "API_Keys.txt", sep = "/"), sep = ";", header = T)
API_Key <- as.character(api_info$API_Key)
API_Sign <- as.character(api_info$API_Sign)

# Get private balance url
url <- "https://api.kraken.com/0/private/Balance"

# Choose pair
pair <- "ETHEUR"


# 1. Get the OHLC - Repeat this call every x interval
what <- tryCatch(
  {
    url <- paste0('https://api.kraken.com/0/public/OHLC?pair=',pair,'&interval=', interval)
    dat <- jsonlite::fromJSON(url)
  },
  error = function(e){})

if(is.null(dat$result[1])) next # error, skip
if(nrow(as.data.frame(dat$result[1])) == 0) break # last batch empty

df <- as.data.table(dat$result[[1]])
colnames(df) <- c("time", "open", "high", "low", "close",
                  "vwap", "volume", "count")
df[, Date_POSIXct := anytime(as.numeric(as.character(time)))]
df$Date_POSIXct <- as.character(df$Date_POSIXct)

# Remove last row as its the candle that still ongoing
df <- df[-nrow(df), ]

# as numeric
df$open <- as.numeric(df$open)
df$high <- as.numeric(df$high)
df$low <- as.numeric(df$low)
df$close <- as.numeric(df$close)
df$volume <- as.numeric(df$volume)
df$servertime <- 0
df$systemtime <- 0
servertime <- myfun("https://api.kraken.com/0/public/Time", API_Key, API_Sign)
df$servertime[nrow(df)] <- as.character(servertime$result$rfc1123)
df$systemtime[nrow(df)] <- as.character(Sys.time())

# 2. Add Indicators
df[,c("fast_EMA",
      "slow_EMA",
      "exit_value",
      "exit_condition",
      "crossover",
      "action",
      "tp",
      "ult_sl",
      "trail_sl",
      "Units",
      "Price",
      "id") := list(NA, NA,NA,NA,NA, NA,  NA, NA, NA, NA, NA, NA)   ]

df$action <- as.character(df$action)
# RSI and Volume
df$fast_EMA <- EMA(df$close, n = fast_EMA)
df$slow_EMA <- EMA(df$close, n = slow_EMA)

# Technical indicators -----------------------------------------------------

# RSI Crossing of upper or lower bounds
df$crossover[nrow(df)] <- ifelse(df$fast_EMA[nrow(df)] > df$slow_EMA[nrow(df)] ,
                                     "faster_EMA_higher", "faster_EMA_lower")

lastrow <- df[nrow(df), ]


if(file.exists("/media/chris/DATA/Documents/Bot_Trading/Coinmaker/Trading Module/Trading_Table/da.csv")){
  tmp <- read.table("/media/chris/DATA/Documents/Bot_Trading/Coinmaker/Trading Module/Trading_Table/da.csv",
                    header = FALSE,
                    sep = ",", stringsAsFactors = FALSE)
  colnames(tmp) <- colnames(lastrow)
  da <- rbind(tmp, lastrow)
} else {
  
  da <- lastrow
}

# Exit condition for takeprofit  - Fixed
tp <- tail(da$close[da$action == "buy"][!is.na(da$close[da$action == "buy"])], 1) + takeprofit * tail(da$close[da$action == "buy"][!is.na(da$close[da$action == "buy"])], 1)

if (length(tp) == 0) {
  tp <- 0
}

# Ultimate stop loss
ult_sl <- tail(da$close[da$action == "buy"][!is.na(da$close[da$action == "buy"])], 1) - stoploss_trail * tail(da$close[da$action == "buy"][!is.na(da$close[da$action == "buy"])], 1)

if (length(ult_sl) == 0) {
  ult_sl <- 0
}


# Trailing stop loss

if(nrow(da) > 1 ){
  
  
  # Trailing stop loss
  # browser()
  if (da$action[nrow(da)-1] %in% c("buy", "keep") & ( da$close[nrow(da)] > da$close[nrow(da)-1] )  ){
    
    trail_sl <- da$close[nrow(da)] - stoploss_trail * da$close[nrow(da)]
    if( trail_sl < tail(da$trail_sl[!is.na(da$trail_sl)], 1)){
      trail_sl <- tail(da$trail_sl[!is.na(da$trail_sl)], 1)
      
    }else {
      trail_sl <- da$close[nrow(da)] - stoploss_trail * da$close[nrow(da)]
      
    }
    
    
  } else if (da$action[nrow(da)-1] %in% c("buy", "keep") & ( da$close[nrow(da)] <= da$close[nrow(da)-1] ) ){
    
    trail_sl <- tail(da$trail_sl[!is.na(da$trail_sl)], 1)
  } else {
    
    trail_sl <-0
  }
  
  
  if(length(trail_sl) == 0 ){
    
    trail_sl <- 0
  }
  # browser() 
}else{
  
  trail_sl <-0
}


if(length(trail_sl) == 0 ){
  
  trail_sl <- 0
}

# if (!exists("trail_sl")) {
#   trail_sl <- 0
# }



da$tp[nrow(da)] <- tp
da$ult_sl[nrow(da)] <- ult_sl
da$trail_sl[nrow(da)] <- trail_sl


da$exit_condition[nrow(da)] <- da$trail_sl[nrow(da)] > da$close[nrow(da)] | da$ult_sl[nrow(da)] > da$close[nrow(da)] | da$tp[nrow(da)] < da$close[nrow(da)]

if (nrow(da) > 1) {
  
  # BUY Condition ------------------------------------------------------------
  if ((is.na(da$action[nrow(da) - 1]) | da$action[nrow(da) - 1] %in% c("sell", "no action")) &
      da$crossover[nrow(fut)] == "faster_EMA_higher" ){  
    
    # get initial balance in EUR
    init_balance <- get_balance(url = "https://api.kraken.com/0/private/Balance",
                                key = API_Key, secret = API_Sign)
    initial_budget <- as.numeric(init_balance$result$ZEUR)
    initial_budget <- initial_budget - 10
    # initial_budget <- 500
    # Give API Order to buy at market
    buy_it <- add_market_order(url = "https://api.kraken.com/0/private/AddOrder",
                               key = API_Key, secret = API_Sign, pair = pair, type = "buy",
                               ordertype = "market", volume = initial_budget / da$close[nrow(da)])
    
    # print(buy_it)
    da$action[nrow(da)] <- "buy"
    da$Units[nrow(da)] <- initial_budget / da$close[nrow(da)]
    da$Price[nrow(da)] <- da$Units[nrow(da)]*da$close[nrow(da)]
    da$id[nrow(da)] <- buy_it$result$txid
    
    
    # KEEP condition
  } else if (  da$action[nrow(da) - 1] %in% c("buy", "keep")   & 
               da$exit_condition[nrow(da)] == FALSE ) {
    
    da$action[nrow(da)] <- "keep"
    da$Units[nrow(da)] <- da$Units[nrow(da) -1 ]
    da$id[nrow(da)] <- da$id[nrow(da)-1]
    
    # SELL condition 
  } else if (da$action[nrow(da) - 1] %in% c("keep", "buy") & (
    da$exit_condition[nrow(da)] == TRUE  ) ) {
    
    
    crypto_hold <- get_balance(url = "https://api.kraken.com/0/private/Balance",
                               key = API_Key, secret = API_Sign)
    crypto_hold_eth <- as.numeric(crypto_hold$result$LSK)
    
    # # Give API Order to buy at market
    sell_it <- add_market_order(url = "https://api.kraken.com/0/private/AddOrder",
                                key = API_Key, secret = API_Sign, pair = pair, type = "sell",
                                ordertype = "market", volume = crypto_hold_eth)
    
    # print(sell_it)
    da$action[nrow(da)] <- "sell"
    da$Units[nrow(da)] <- da$Units[nrow(da) -1]
    da$Price[nrow(da)] <- da$close[nrow(da)]* da$Units[nrow(da)]
    da$id[nrow(da)] <- sell_it$result$txid
    
  } else {
    
    da$action[nrow(da)] <- "no action"
    
  }
  
  
} else {
  
  da$action[nrow(da)] <- "no action"
  
}
print(da)
write.table(da[nrow(da), ], file = "/media/chris/DATA/Documents/Bot_Trading/Coinmaker/Trading Module/Trading_Table/da.csv",
            row.names = FALSE,
            col.names = FALSE,
            append = TRUE,
            sep = ",")

