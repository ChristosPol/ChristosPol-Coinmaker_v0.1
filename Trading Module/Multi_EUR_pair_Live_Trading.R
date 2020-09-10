rm(list = ls())
source(paste("/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1", "10 Utils.R", sep = "/"))
setDTthreads(1)

api_info <- read.table(paste("/media/chris/DATA/Documents/Bot_Trading", "API_Keys.txt", sep = "/"), sep = ";", header = T)

API_Key <- as.character(api_info$API_Key)
API_Sign <- as.character(api_info$API_Sign)
avail_pairs <- myfun("https://api.kraken.com/0/public/AssetPairs", secret = API_Sign, key = API_Key)
all_pairs <- names(avail_pairs[[2]])

# Parameters
spar <- 0.95
takeprofit <- 0.015 
stoploss_trail <- 1
stoploss_ult <- 1

trading_table_path <- "/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1/Trading Module/Trading_Table/"

# Get only EUR related crypto pairs
EUR_pairs <- grep("EUR", all_pairs, value = T)

# Remove Forex pairs
to_remove <- grep(paste(c("USD",
                          ".d",
                          "AUD",
                          "CAD",
                          "JPY",
                          "CHF",
                          "GBP",
                          "REP",
                          "PAX",
                          "DAI",
                          "BAT"), collapse ="|"), EUR_pairs, value = T)
EUR_pairs <- EUR_pairs[!EUR_pairs %in% to_remove]
i <- 1

for (i in 1:length(EUR_pairs)){
  
  df <- simple_OHLC(interval = 60, pair = EUR_pairs[i])
  
  # Remove last row as its the candle that still ongoing
  df <- df[-nrow(df), ]
  
  df$servertime <- 0
  df$systemtime <- 0
  servertime <- myfun("https://api.kraken.com/0/public/Time", API_Key, API_Sign)
  df$servertime[nrow(df)] <- as.character(servertime$result$rfc1123)
  df$systemtime[nrow(df)] <- as.character(Sys.time())
  
  # 2. Add Indicators
  df[, c("x",
         "spline",
         "deriv",
         "sign_derivs",
         "change_sign",
         "exit_condition",
         "action",
         "Units",
         "Price",
         "tp",
         "ult_sl",
         "trail_sl",
         "id") := list(NA,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]
  
  df$action <- as.character(df$action)
  
  df$x <- 1:nrow(df)
  
  # Calculate spline - derivative
  smoothingSpline = smooth.spline(df[, close] ~ as.numeric(rownames(df)), spar = spar)
  df[, spline := predict(smoothingSpline)$y]
  df[, deriv := predict(smoothingSpline, deriv = 1)$y]
  
  # Sign of deriv - [-2 for desc, 2 for asc] 
  df[, sign_derivs := c(sign(deriv))]
  df[, change_sign := c(0, diff(sign(deriv)))]
  
  lastrow <- df[nrow(df), ]
  
  if(file.exists(paste0(trading_table_path, EUR_pairs[i], "_table.csv") )){
    tmp <- read.table(paste0(trading_table_path, EUR_pairs[i], "_table.csv"),
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
  ult_sl <- tail(da$close[da$action == "buy"][!is.na(da$close[da$action == "buy"])], 1) - stoploss_ult * tail(da$close[da$action == "buy"][!is.na(da$close[da$action == "buy"])], 1)
  
  if (length(ult_sl) == 0) {
    ult_sl <- 0
  }

  if(nrow(da) > 1 ){
    # Trailing stop loss
    # browser()
    if (da$action[nrow(da)-1] %in% c("buy", "keep") & ( da$close[nrow(da)] > da$close[nrow(da)-1] )  ){
      trail_sl <- da$close[nrow(da)] - stoploss_trail * da$close[nrow(da)]
      if( trail_sl < tail(da$trail_sl[!is.na(da$trail_sl)], 1)){
        trail_sl <- tail(da$trail_sl[!is.na(da$trail_sl)], 1)
      } else {
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
    
  } else {
    trail_sl <-0
  }
  
  if(length(trail_sl) == 0 ){
    trail_sl <- 0
  }

  da$tp[nrow(da)] <- tp
  da$ult_sl[nrow(da)] <- ult_sl
  da$trail_sl[nrow(da)] <- trail_sl
  da$exit_condition[nrow(da)] <- da$trail_sl[nrow(da)] > da$close[nrow(da)] | da$ult_sl[nrow(da)] > da$close[nrow(da)] | da$tp[nrow(da)] < da$close[nrow(da)]
  
  if (nrow(da) > 1) {
    
    # BUY Condition ------------------------------------------------------------
    if ((is.na(da$action[nrow(da) - 1]) | da$action[nrow(da) - 1] %in% c("sell", "no action")) &
        da$deriv[nrow(da)] > 0){  
      
      # get initial balance in EUR
      # init_balance <- get_balance(url = "https://api.kraken.com/0/private/Balance",
      #                             key = API_Key, secret = API_Sign)
      # initial_budget <- as.numeric(init_balance$result$ZEUR)
      # initial_budget <- initial_budget - 10
      initial_budget <- 500
      # Give API Order to buy at market
      # buy_it <- add_market_order(url = "https://api.kraken.com/0/private/AddOrder",
      #                            key = API_Key, secret = API_Sign, pair = pair, type = "buy",
      #                            ordertype = "market", volume = initial_budget / da$close[nrow(da)])
      
      # print(buy_it)
      da$action[nrow(da)] <- "buy"
      da$Units[nrow(da)] <- initial_budget / da$close[nrow(da)]
      da$Price[nrow(da)] <- da$Units[nrow(da)]*da$close[nrow(da)]
      # da$id[nrow(da)] <- buy_it$result$txid
      da$id[nrow(da)] <- round(runif(1, 10000, 5000000))
      
      # KEEP condition
    } else if (  da$action[nrow(da) - 1] %in% c("buy", "keep")   & 
                 da$exit_condition[nrow(da)] == FALSE ) {
      
      da$action[nrow(da)] <- "keep"
      da$Units[nrow(da)] <- da$Units[nrow(da) -1 ]
      da$id[nrow(da)] <- da$id[nrow(da)-1]
      
      # SELL condition 
    } else if (da$action[nrow(da) - 1] %in% c("keep", "buy") & (
      da$exit_condition[nrow(da)] == TRUE | da$deriv[nrow(da)] < 0 ) ) {
      
      
      # crypto_hold <- get_balance(url = "https://api.kraken.com/0/private/Balance",
      #                            key = API_Key, secret = API_Sign)
      # crypto_hold_eth <- as.numeric(crypto_hold$result$XETH)
      
      # # Give API Order to buy at market
      # sell_it <- add_market_order(url = "https://api.kraken.com/0/private/AddOrder",
      #                             key = API_Key, secret = API_Sign, pair = pair, type = "sell",
      #                             ordertype = "market", volume = crypto_hold_eth)
      
      # print(sell_it)
      da$action[nrow(da)] <- "sell"
      da$Units[nrow(da)] <- da$Units[nrow(da) -1]
      da$Price[nrow(da)] <- da$close[nrow(da)]* da$Units[nrow(da)]
      # da$id[nrow(da)] <- sell_it$result$txid
      da$id[nrow(da)] <- round(runif(1, 10000, 5000000))
      
    } else {
      
      da$action[nrow(da)] <- "no action"
      
    }
    
    
  } else {
    
    da$action[nrow(da)] <- "no action"
    
  }

  write.table(da[nrow(da), ], file = paste0(trading_table_path, EUR_pairs[i], "_table.csv"),
              row.names = FALSE,
              col.names = FALSE,
              append = TRUE,
              sep = ",")
  Sys.sleep(5)
}
  