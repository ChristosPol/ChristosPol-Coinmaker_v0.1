
# Packages --------------------------------------------------------------------

suppressMessages(library(xts))
suppressMessages(library(Rbitcoin))
suppressMessages(library(httr))
suppressMessages(library(anytime))
suppressMessages(library(data.table))
suppressMessages(library(dplyr))
suppressMessages(library(lubridate))
suppressMessages(library(ggplot2))
suppressMessages(library(TTR))
suppressMessages(library(openssl))
suppressMessages(library(digest))
suppressMessages(library(zoo))
suppressMessages(library(foreach))
suppressMessages(library(doParallel))
suppressMessages(library(bit64))
suppressMessages(library(nanotime))
suppressMessages(library(gganimate))
suppressMessages(library(gapminder))
suppressMessages(library(gifski))
suppressMessages(library(gridExtra))
# Functions --------------------------------------------------------------------

# Download historical trade data for selected pair using initial id ------------
hist_trades_pair <- function(sleep, hist_id, pair){
  repeat {
    Sys.sleep(sleep)
    what <- tryCatch(
      {
        url <- paste0('https://api.kraken.com/0/public/Trades?pair=',pair,'&since='
                      ,hist_id)
        dat <- jsonlite::fromJSON(url)
      },
      error = function(e){})
    if(is.null(dat$result[1])) next # error, skip
    if(nrow(as.data.frame(dat$result[1])) == 0) break # last batch empty
    temp <- cbind(data.frame(dat$result[1]), dat$result$last)
    hist_id <- dat$result$last
    file <- paste0(paste(pair_data_results, pair, sep = "/"), ".csv")
    write.table(temp, file, sep = ",", row.names = FALSE,
                col.names = FALSE,
                append = TRUE)
    print(Sys.time())
    print(unique(as.character(temp$`dat$result$last`)))
  }
}

# Plot candlestick data --------------------------------------------------------
plot_candlesticks <- function(dta, Ns, asset){
  
  dta <- tail(dta, Ns)
  mn <- min(dta$low)
  mx <- max(dta$high)
  
  xs <- c(1:nrow(dta))
  color_list <- ifelse(dta$close >= dta$open, "green", "red")
  
  plot(dta$high, main = asset, xaxt = "n", xlab = "", ylab = "price", ylim = c(mn, mx), type = "n")
  par(new = T)
  plot(dta$low, main = "", axes = F, xlab = "", ylab = "", ylim = c(mn, mx), type = "n")
  segments(x0 = xs, y0 = dta$open, x1 = xs, y1 = dta$close, col = color_list, lwd = 4)
  segments(x0 = xs, y0= dta$low, x1 = xs, y1 = dta$high, col = color_list, lwd = 1)
  axis(1, at = seq(1, max(xs), by =100), labels = substr(dta$Date_POSIXct[seq(1, max(xs), by =100)],1,10), las = 2)
}

# # Plot chart with SR lines and return values 

SR_lines <- function(data, roll, n_sort, pair, Ns, plot.it = FALSE){
  
  last_close <- data$high[nrow(data)]
  
  last_prices <- tail(data$high[-nrow(data)], roll)
  last_volumes <- tail(data$volume[-nrow(data)], roll)
  mydf <- data.frame(last_prices, last_volumes)
  mydf <- arrange(mydf, mydf$last_prices)
  
  sup_df <- head(mydf, n_sort)
  sup_w_mean <- sum(sup_df$last_prices *sup_df$last_volumes)/sum(sup_df$last_volumes)
  
  rs_df <- tail(mydf, n_sort)
  rs_w_mean <- sum(rs_df$last_prices *rs_df$last_volumes)/sum(rs_df$last_volumes)
  if(plot.it == TRUE){
    
  
    plot_candlesticks(dta = data, Ns = Ns, asset = pair)
    abline(h = rs_w_mean, col = "black", lty = "dashed")
    abline(h = sup_w_mean, col = "black", lty = "dashed")
  }
  return(list(SL = sup_w_mean, RL = rs_w_mean))
  
}

# Plots boolinger bands
bollinger_bands <- function(periods, times_sd, data){
  df <- data
  plot(df$close[-c(1:(periods-1))], type ="l", lwd =2)
  lines(SMA(df$close, n=periods)[-c(1:(periods-1))] + times_sd*rollapply(df$close, periods, sd), col ="red")
  lines(SMA(df$close, n=periods)[-c(1:(periods-1))] - times_sd*rollapply(df$close, periods, sd), col ="green")
}

# how many trades were succesful (to be functioned)
win_ratio <- function(dataset){
  df <- dataset
  mah <- subset(df, df$action %in% c("buy","sell") )
  profitable_trades <- list()
  ids_s <- unique(mah$id)
  for(i in 1:length(unique(mah$id))){
    
    profitable_trades[[i]] <- mah$close[mah$id == ids_s[i] & mah$action =="sell"]-mah$close[mah$id == ids_s[i] & mah$action =="buy"]
  }
  res <- table(unlist(profitable_trades) > 0)[names(table(unlist(profitable_trades) > 0)) ==T]/sum(table(unlist(profitable_trades) > 0))
  return(res)
}

# SR_lines <- function(roll, data, plot.it, pair){
#   
#   last_close <- data$close[nrow(data)]
#   resistance <- unique(rollapplyr(data$close[-nrow(data)], roll, max, fill = NA))
#   resistance <- resistance[!is.na(resistance)]
#   
#   if (length(which(resistance < last_close)) == 0) {
#     resistance_act <- min(resistance, na.rm = T)
#   
#   } else if(all(last_close > resistance)){
#     resistance_act <- max(resistance, na.rm =T)
#   
#   } else {
#     resistance_act <- min(resistance[-which(resistance < last_close)], na.rm = T)
#   }
#   
#   support <- unique(rollapplyr(data$close[-nrow(data)], roll, min, fill = NA))
#   support <- support[!is.na(support)]
#   
#   if (length(which(support > last_close)) == 0) {
#     support_act <- max(support, na.rm = T)
#     
#   } else if(all(last_close < support)){
#     support_act <- min(support, na.rm =T)
#     
#   } else {
#     support_act <- max(support[-which(support > last_close)], na.rm = T)
#   }
#   if(plot.it == TRUE){
#     plot_candlesticks(dta = data, Ns = nrow(data), asset = pair)
#     abline(h = resistance_act, col = "lightblue")
#     abline(h = support_act, col = "black")
#   }
#   return(list(SL = support_act, RL = resistance_act))
# }

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#                                   Add standard order
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Inputs -----------------------------------------------------------------------
# pair = asset pair
# type = type of order (buy/sell)
# ordertype = order type:
#   market
# limit (price = limit price)
# stop-loss (price = stop loss price)
# take-profit (price = take profit price)
# stop-loss-profit (price = stop loss price, price2 = take profit price)
# stop-loss-profit-limit (price = stop loss price, price2 = take profit price)
# stop-loss-limit (price = stop loss trigger price, price2 = triggered limit price)
# take-profit-limit (price = take profit trigger price, price2 = triggered limit price)
# trailing-stop (price = trailing stop offset)
# trailing-stop-limit (price = trailing stop offset, price2 = triggered limit offset)
# stop-loss-and-limit (price = stop loss price, price2 = limit price)
# settle-position
# price = price (optional.  dependent upon ordertype)
# price2 = secondary price (optional.  dependent upon ordertype)
# volume = order volume in lots
# leverage = amount of leverage desired (optional.  default = none)
# oflags = comma delimited list of order flags (optional):
#   viqc = volume in quote currency (not available for leveraged orders)
# fcib = prefer fee in base currency
# fciq = prefer fee in quote currency
# nompp = no market price protection
# post = post only order (available when ordertype = limit)
# starttm = scheduled start time (optional):
#   0 = now (default)
# +<n> = schedule start time <n> seconds from now
# <n> = unix timestamp of start time
# expiretm = expiration time (optional):
#   0 = no expiration (default)
# +<n> = expire <n> seconds from now
# <n> = unix timestamp of expiration time
# userref = user reference id.  32-bit signed number.  (optional)
# validate = validate inputs only.  do not submit order (optional)
# optional closing order to add to system when order gets filled:
#   close[ordertype] = order type
# close[price] = price
# close[price2] = secondary price

# Values -----------------------------------------------------------------------
# descr = order description info
# order = order description
# close = conditional close order description (if conditional close set)
# txid = array of transaction ids for order (if order was added successfully)
# url      <- "https://api.kraken.com/0/private/AddOrder"
# type <- "sell"
# ordertype <- "market"
# volume <- 0.1

add_market_order <- function(url, key, secret, pair, type, ordertype, volume) {
  
  nonce <- as.character(as.numeric(Sys.time()) * 1000000)
  post_data <- paste0("nonce=", nonce, "&pair=", pair, "&type=", type, "&ordertype=", ordertype,
                      "&volume=", volume)
  method_path <- gsub("^.*?kraken.com", "", url)
  sign <- hmac(key =  RCurl::base64Decode(secret, mode = "raw"), 
               object = c(charToRaw(method_path), digest(object = paste0(nonce, 
                                                                         post_data), algo = "sha256", serialize = FALSE, 
                                                         raw = TRUE)), algo = "sha512", raw = TRUE)
  httpheader <- c(`API-Key` = key, `API-Sign` =  RCurl::base64Encode(sign))
  
  curl <- RCurl::getCurlHandle(useragent = paste("Rbitcoin", packageVersion("Rbitcoin")))
  query_result_json <- rawToChar(RCurl::getURLContent(curl = curl, 
                                                      url = url, binary = TRUE, postfields = post_data, 
                                                      httpheader = httpheader))
  query_result <- jsonlite::fromJSON(query_result_json)
  
  return(query_result)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#                                   Trade Balance
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Inputs -----------------------------------------------------------------------
# aclass = asset class (optional):  currency (default, always currency)
# asset = base asset used to determine balance (default = ZUSD)

# Values -----------------------------------------------------------------------
# eb = equivalent balance (combined bbalancealance of all currencies)
# tb = trade balance (combined  of all equity currencies)
# m = margin amount of open positions
# n = unrealized net profit/loss of open positions
# c = cost basis of open positions
# v = current floating valuation of open positions
# e = equity = trade balance + unrealized net profit/loss
# mf = free margin = equity - initial margin (maximum margin available to open new positions)
# ml = margin level = (equity / initial margin) * 100
# url      <- "https://api.kraken.com/0/private/Balance"

get_balance <- function (url, key, secret) {
  
  nonce <- as.character(as.numeric(Sys.time()) * 1000000)
  post_data <- paste0("nonce=", nonce)
  method_path <- gsub("^.*?kraken.com", "", url)
  sign <- hmac(key =  RCurl::base64Decode(secret, mode = "raw"), 
               object = c(charToRaw(method_path), digest(object = paste0(nonce, 
                                                                         post_data), algo = "sha256", serialize = FALSE, 
                                                         raw = TRUE)), algo = "sha512", raw = TRUE)
  httpheader <- c(`API-Key` = key, `API-Sign` =  RCurl::base64Encode(sign))
  
  curl <- RCurl::getCurlHandle(useragent = paste("Rbitcoin", packageVersion("Rbitcoin")))
  query_result_json <- rawToChar(RCurl::getURLContent(curl = curl, 
                                                      url = url, binary = TRUE, postfields = post_data, 
                                                      httpheader = httpheader))
  query_result <- jsonlite::fromJSON(query_result_json)
  
  return(query_result)
}

# Splines trend inversion
Splines_Tangent <- function(takeprofit, stoploss_trail,stoploss_ult, spar,plot.it) {
  
  # Train and test datasets
  train_data[, c("x",
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
  
  test_data[, c("x",
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
  
  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){
    
    fut <- rbind(train_data, test_data[i, ])
    fut$x <- 1:nrow(fut)
    # Calculate spline - derivative
    smoothingSpline = smooth.spline(fut[, close] ~ as.numeric(rownames(fut)) , spar = spar)
    fut[, spline := predict(smoothingSpline)$y]
    fut[, deriv := predict(smoothingSpline, deriv = 1)$y]
    
    # Sign of deriv - [-2 for desc, 2 for asc] 
    fut[, sign_derivs := c(sign(deriv))]
    fut[, change_sign := c(0, diff(sign(deriv)))]
    
    if(plot.it == TRUE){
      
      fut <- tail(fut, 200)
      df_points_buy <- data.frame(x = na.omit(fut$x[fut$action =="buy"]),
                              y = na.omit(fut$close[fut$action =="buy"]))
      df_points_sell <- data.frame(x = na.omit(fut$x[fut$action =="sell"]),
                                  y = na.omit(fut$close[fut$action == "sell"]))
      
      par(mfrow = c(2, 1))
      plot(fut$close, type ="l", main = paste0("profits = ", tail(na.omit(fut$Price), 1)))
      lines(fut$spline, col ="red")
      points(df_points_buy$x, df_points_buy$y, col ="green", pch = 19)
      points(df_points_sell$x, df_points_sell$y, col ="red", pch = 19)
  
      plot(fut$deriv, type ="l", main = paste0("sign: ",
                                               " sign deriv: ", fut$sign_derivs[nrow(fut)], " deriv ", fut$deriv[nrow(fut)]))
      abline(h = 0, col = "red", lty = 5, lwd = 2)
      # plot(fut$volume, type ="l")
      # plot(fut$ratio_volume, type ="l")
      # abline(h =0.50, col ="red")
    }
    
    # Exit condition for takeprofit  - Fixed
    tp <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) + takeprofit * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)
    
    if (length(tp) == 0) {
      tp <- 0
    }
    
    # Ultimate stop loss
    ult_sl <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) - stoploss_ult * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)
    
    if (length(ult_sl) == 0) {
      ult_sl <- 0
    }
    
    
    # Trailing stop loss
    if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){
      
      trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      if( trail_sl < tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)){
        trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
        
      } else {
        trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      }
      
    } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1] ) ){
      trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
      
    } else {
      trail_sl <-0
    }
    
    if(length(trail_sl) == 0 ){
      
      trail_sl <- 0
    }
    
    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl
    fut$trail_sl[nrow(fut)] <- trail_sl
    
    
    fut$exit_condition[nrow(fut)] <- fut$trail_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]
    
    
    # Deciding upon action -----------------------------------------------------
    
    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         fut$deriv[nrow(fut)] > 0) {
      
      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
      
      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") & (
      fut$exit_condition[nrow(fut)] == TRUE | fut$deriv[nrow(fut)] < 0 )) {
      
      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]
      
      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   &
                fut$exit_condition[nrow(fut)] == FALSE  ) {
      
      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      
    } else {
      
      fut$action[nrow(fut)] <- "no action"
      
    }
    
    train_data <- fut
    # print(i)
    if(plot.it == TRUE){
      Sys.sleep(0.1)
      # flush.console()
    }
  }
  return(train_data)
}


# Strategy using volumes spikes and RSI oversold conditions
Pure_RSI_Volume_Trailing <- function(RSI_Period, RSI_below, EMA_volume, takeprofit, stoploss_trail,stoploss_ult, times_vol) {
  
  # Train and test datasets
  train_data[, c("SMA",
                 "RSI",
                 "EMA_volume",
                 "exit_value",
                 "exit_condition",
                 "crossover_volume",
                 "crossover_RSI",
                 "Slope",
                 "action",
                 "Units",
                 "Price",
                 "tp",
                 "ult_sl",
                 "trail_sl",
                 "id") := list(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]
  
  test_data[, c("SMA",
                "RSI",
                "EMA_volume",
                "exit_value",
                "exit_condition",
                "crossover_volume",
                "crossover_RSI",
                "Slope",
                "action",
                "Units",
                "Price",
                "tp",
                "ult_sl",
                "trail_sl",
                "id") := list(NA, NA,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]
  
  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){
    
    fut <- rbind(train_data, test_data[i, ])
    
    # RSI and Volume
    fut$RSI <- RSI(fut$close, n = RSI_Period)
    fut$EMA_volume <- EMA(fut$volume, n = EMA_volume)

    # Volume Crossing of volume over the SMA(volume, n_periods)
    fut$crossover_volume[nrow(fut)] <- ifelse(fut$volume[nrow(fut)] > fut$EMA_volume[nrow(fut)] * times_vol ,
                                              "volume_higher", "volume_lower")
    # RSI Crossing of upper or lower bounds
    fut$crossover_RSI[nrow(fut)] <- ifelse(fut$RSI[nrow(fut)] < RSI_below ,
                                           "RSI_lower", "RSI_higher")
    
    # Exit condition for takeprofit  - Fixed
    tp <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) + takeprofit * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)
    
    if (length(tp) == 0) {
      tp <- 0
    }
    
    # Ultimate stop loss
    ult_sl <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) - stoploss_ult * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)
    
    if (length(ult_sl) == 0) {
      ult_sl <- 0
    }
    
    
    # Trailing stop loss
    if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){
      
      trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      if( trail_sl < tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)){
        trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
        
      } else {
        trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
        }
      
    } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1] ) ){
      trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
    
    } else {
      trail_sl <-0
    }
    
    if(length(trail_sl) == 0 ){
      
      trail_sl <- 0
    }
    
    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl
    fut$trail_sl[nrow(fut)] <- trail_sl
    
    
    fut$exit_condition[nrow(fut)] <- fut$trail_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]
    
    
    # Deciding upon action -----------------------------------------------------
    
    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         fut$crossover_volume[nrow(fut)] == "volume_higher" &  fut$crossover_RSI[nrow(fut)] == "RSI_lower") {
      
      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
      
      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") & (
      fut$exit_condition[nrow(fut)] == TRUE )) {
      
      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]
      
      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   & 
                fut$exit_condition[nrow(fut)] == FALSE  ) {
      
      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      
    } else {
      
      fut$action[nrow(fut)] <- "no action"
      
    }
    
    train_data <- fut
    print(i)
  }
  return(train_data)
}
# Dynamic sr lines
Dynamic_SR_Lines <- function(roll,
                             n_sort,
                             takeprofit,
                             stoploss_trail,
                             stoploss_ult,
                             RSI_Period,
                             RSI_below) {
  
  # Train and test datasets
  train_data[, c("RSI",
                 "crossover_RSI",
                 "SL",
                 "RL",
                 "exit_value",
                 "exit_condition",
                 "crossover",
                 "action",
                 "Units",
                 "Price",
                 "tp",
                 "ult_sl",
                 "trail_sl",
                 "id") := list(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]
  
  test_data[, c("RSI",
                "crossover_RSI",
                "SL",
                "RL",
                "exit_value",
                "exit_condition",
                "crossover",
                "action",
                "Units",
                "Price",
                "tp",
                "ult_sl",
                "trail_sl",
                "id") := list(NA, NA, NA, NA,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]
  
  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){
    
    fut <- rbind(train_data, test_data[i, ])
    
    lines_sr <- SR_lines(data = fut, roll = roll, n_sort = n_sort, pair = "BTCEUR", Ns = nrow(fut))
    fut$RL[nrow(fut)] <- lines_sr$RL 
    fut$SL[nrow(fut)] <- lines_sr$SL
    
    if(fut$close[nrow(fut)] < lines_sr$SL){
      
      fut$crossover[nrow(fut)] <- "Below"
    }else if (fut$close[nrow(fut)] > lines_sr$RL){
      fut$crossover[nrow(fut)] <- "Above"
    }else{
      fut$crossover[nrow(fut)] <- "Between"
    }
    
    # RSI calculation
    fut$RSI <- RSI(fut$close, n = RSI_Period)
    
    # RSI Crossing of upper or lower bounds
    fut$crossover_RSI[nrow(fut)] <- ifelse(fut$RSI[nrow(fut)] < RSI_below ,
                                           "RSI_lower", "RSI_higher")
    

    
    # Exit condition for takeprofit  - Fixed
    tp <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) + takeprofit * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)
    
    if (length(tp) == 0) {
      tp <- 0
    }
    
    # Ultimate stop loss
    ult_sl <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) - stoploss_ult * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)
    
    if (length(ult_sl) == 0) {
      ult_sl <- 0
    }
    
    
    # Trailing stop loss
    if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){
      
      trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      if( trail_sl < tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)){
        trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
        
      } else {
        trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      }
      
    } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1] ) ){
      trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
      
    } else {
      trail_sl <-0
    }
    
    if(length(trail_sl) == 0 ){
      
      trail_sl <- 0
    }
    
    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl
    fut$trail_sl[nrow(fut)] <- trail_sl
    
    
    fut$exit_condition[nrow(fut)] <- fut$trail_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]
    
    
    # Deciding upon action -----------------------------------------------------
    
    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         (fut$crossover[nrow(fut)] == "Below" &  fut$crossover_RSI[nrow(fut)] == "RSI_lower")) {
      
      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
      
      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") & (
      # fut$exit_condition[nrow(fut)] == TRUE)) {
      fut$exit_condition[nrow(fut)] == TRUE | fut$crossover[nrow(fut)] == "Above")) {
      
      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]
      
      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   & 
                fut$exit_condition[nrow(fut)] == FALSE  ) {
      
      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      
    } else {
      
      fut$action[nrow(fut)] <- "no action"
      
    }
    
    train_data <- fut
    print(i)
  }
  return(train_data)
}


# Indicator trading ------------------------------------------------------------
# MACD crossover going long
MACD_long <- function(EMA_Trend, nfast, nslow, nsig, stoploss, takeprofit) {
  
  # Train and test datasets
  train_data[, c("macd",
                 "signal",
                 "hist",
                 paste0("EMA_Trend", "_", EMA_Trend),
                 "Uptrend",
                 "hist_positive",
                 "exit_value",
                 "exit_condition",
                 "crossover",
                 "action",
                 "Units",
                 "Price",
                 "id") := list(0, NA, 0 ,0, NA, NA, 0, NA, NA, NA, NA, NA, NA) ]
  
  test_data[,  c("macd",
                 "signal",
                 "hist",
                 paste0("EMA_Trend", "_", EMA_Trend),
                 "Uptrend",
                 "hist_positive",
                 "exit_value",
                 "exit_condition",
                 "crossover",
                 "action",
                 "Units",
                 "Price",
                 "id") := list(0, NA, 0, 0, NA, NA, 0, NA, NA, NA, NA, NA, NA) ]
  
  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){
    
    fut <- rbind(train_data, test_data[i, ])
    
    
    # Technical indicators -----------------------------------------------------
    macd_indicator <- as.data.frame(MACD(fut$close, nFast = nfast, nSlow = nslow, nSig = nsig))
    macd_indicator$hist <-macd_indicator$macd - macd_indicator$signal 
    
    fut$macd <- macd_indicator$macd
    fut$signal <- macd_indicator$signal
    fut$hist <- macd_indicator$hist
    
    fut[, c(paste0("EMA_Trend", "_", EMA_Trend)) := list(EMA(close, n = EMA_Trend)) ]
    
    # Long  signal
    fut$crossover[fut$macd > fut$signal] <- "macd_higher"
    fut$crossover[fut$macd <=  fut$signal] <- "macd_lower"  
    
    fut$hist_positive[fut$macd & fut$signal < 0] <- "hist_negative"
    fut$hist_positive[fut$macd & fut$signal >= 0] <- "hist_positive"
    
    # Uptrend signal
    fut$Uptrend[fut$close > get(paste0("EMA_Trend", "_", EMA_Trend), fut)] <- TRUE
    fut$Uptrend[fut$close <= get(paste0("EMA_Trend", "_", EMA_Trend), fut)] <- FALSE
    
    # Exit condition for stop loss 
    exit_value <- (fut$close[nrow(fut)] - tail(fut$close[!is.na(fut$action) &fut$action =="buy"], 1)) / tail(fut$close[!is.na(fut$action) &fut$action =="buy"], 1)
    
    if (length(exit_value) == 0) {
      exit_value <- 0
    }
    
    fut$exit_value[nrow(fut)] <- exit_value
    fut$exit_condition <- fut$exit_value > takeprofit | fut$exit_value <= -stoploss

    # Deciding upon action -----------------------------------------------------
    
    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         fut$crossover[nrow(fut)] == "macd_higher" &  fut$Uptrend[nrow(fut)] == TRUE & fut$hist_positive[nrow(fut)] == "hist_negative"  ) {
      
      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
      
      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") &
               (fut$exit_condition[nrow(fut)] == TRUE | fut$crossover[nrow(fut)] =="macd_lower") ) {
      
      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]
      
      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   & 
                (fut$exit_condition[nrow(fut)] == FALSE | fut$crossover[nrow(fut)] =="macd_higher" )  ) {
      
      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      
    } else {
      
      fut$action[nrow(fut)] <- "no action"
      
    }
    
    train_data <- fut
    # print(i)
  }
  return(train_data)
}




# Strategy  --------------------------------------------------------------------

Volume_candle_patterns <- function(takeprofit, stoploss, SMA_Volume) {
  
  # Train and test datasets
  train_data[, c(paste0("Volume_SMA", "_", SMA_Volume),
                 "exit_value",
                 "pattern",
                 "candle_type",
                 "exit_condition",
                 "crossover",
                 "action",
                 "Units",
                 "Price",
                 "id") := list(0, NA,0 ,0,NA, NA, NA, NA, NA, NA) ]
  
  test_data[,  c(paste0("Volume_SMA", "_", SMA_Volume),
                 "exit_value",
                 "pattern",
                 "candle_type",
                 "exit_condition",
                 "crossover",
                 "action",
                 "Units",
                 "Price",
                 "id") := list(0, NA,0,0, NA, NA, NA, NA, NA, NA) ]
  
  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){
    
    fut <- rbind(train_data, test_data[i, ])
    
    # Candle type
    fut$candle_type[fut$open > fut$close] <- "bearish"
    fut$candle_type[fut$open < fut$close] <- "bullish"
    fut$candle_type[fut$open == fut$close] <- "neutral"
    # Technical indicators -----------------------------------------------------
    
    fut[, c(paste0("Volume_SMA", "_", SMA_Volume)) := list(SMA(volume, n = SMA_Volume)) ]
    
    # Volume Crossing of volume over the mean(volume, n_periods) + times * sd(volume, n_periods)
    fut$crossover[fut$volume > get(paste0("Volume_SMA", "_", SMA_Volume), fut)  ] <- "volume_higher"
    fut$crossover[fut$volume <= get(paste0("Volume_SMA", "_", SMA_Volume), fut)  ] <- "volume_lower"  
    
    fut$pattern[nrow(fut)] <- fut$candle_type[nrow(fut)] == "bullish" & fut$candle_type[nrow(fut) -1] == "bearish"
    
    
    # Exit condition for stop loss 
    exit_value <- (fut$close[nrow(fut)] - tail(fut$close[!is.na(fut$action) &fut$action %in% c("buy", "keep")], 1)) / tail(fut$close[!is.na(fut$action) &fut$action %in% c("buy", "keep")], 1)
    
    if (length(exit_value) == 0) {
      exit_value <- 0
    }
    
    fut$exit_value[nrow(fut)] <- exit_value
    fut$exit_condition <-  fut$exit_value <= -stoploss
    
    
    
    # Deciding upon action -----------------------------------------------------
    
    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         fut$crossover[nrow(fut)] == "volume_higher" & fut$pattern[nrow(fut)] == TRUE ) {
      
      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
      
      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") &
               fut$exit_condition[nrow(fut)] == TRUE ) {
      
      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]
      
      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   & 
                fut$exit_condition[nrow(fut)] == FALSE  ) {
      
      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      
    } else {
      
      fut$action[nrow(fut)] <- "no action"
      
    }
    
    train_data <- fut
    # print(i)
  }
  return(train_data)
}

################################################################################

# ------------------------------------------------------------------------------
Volume_RSI_TP_SL <- function(RSI_Period, RSI_below, RSI_above, SMA_volume, takeprofit, stoploss, candle_action_long) {
  
  # Train and test datasets
  train_data[, c(paste0("RSI", "_", RSI_Period),
                 paste0("SMA_volume", "_", SMA_volume),
                 "candle_type",
                 "exit_value",
                 "exit_condition",
                 "crossover",
                 "crossover_RSI",
                 "action",
                 "Units",
                 "Price",
                 "id") := list(0, 0,NA, NA, NA, NA, NA, NA, NA, NA, NA) ]
  
  test_data[, c(paste0("RSI", "_", RSI_Period),
                paste0("SMA_volume", "_", SMA_volume),
                "exit_value",
                "candle_type",
                "exit_condition",
                "crossover",
                "crossover_RSI",
                "action",
                "Units",
                "Price",
                "id") := list(0, 0, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]
  
  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){
    
    fut <- rbind(train_data, test_data[i, ])
    
    # Mean and sd of volume in train set
    fut[, c(paste0("RSI", "_", RSI_Period),
            paste0("SMA_volume", "_", SMA_volume)) := list(RSI(close, n = RSI_Period),
                                                           EMA(close, n = SMA_volume)) ]
    
    fut$candle_type[fut$open > fut$close] <- "bearish"
    fut$candle_type[fut$open < fut$close] <- "bullish"
    fut$candle_type[fut$open == fut$close] <- "neutral"
    # Technical indicators -----------------------------------------------------
    
    
    # Volume Crossing of volume over the SMA(volume, n_periods)
    fut$crossover[fut$volume > get(paste0("SMA_volume", "_", SMA_volume), fut) ] <- "volume_higher"
    fut$crossover[fut$volume <= get(paste0("SMA_volume", "_", SMA_volume), fut)  ] <- "volume_lower"
    
    # RSI Crossing of upper or lower bounds
    fut$crossover_RSI[get(paste0("RSI", "_", RSI_Period), fut) < RSI_below ] <- "rsi_below"
    fut$crossover_RSI[get(paste0("RSI", "_", RSI_Period), fut) > RSI_above ] <- "rsi_above"
    fut$crossover_RSI[is.na(fut$crossover_RSI)] <- "rsi_between"
    
    
    # Exit condition for stop loss 
    exit_value <- (fut$close[nrow(fut)] - tail(fut$close[!is.na(fut$action) &fut$action =="buy"], 1)) / tail(fut$close[!is.na(fut$action) &fut$action =="buy"], 1)
    
    if (length(exit_value) == 0) {
      exit_value <- 0
    }
    
    fut$exit_value[nrow(fut)] <- exit_value
    fut$exit_condition <- fut$exit_value > takeprofit | fut$exit_value <= -stoploss
    
    
    # Deciding upon action -----------------------------------------------------
    
    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         fut$crossover[nrow(fut)] == "volume_higher" & fut$candle_type[nrow(fut)] == candle_action_long & fut$crossover_RSI[nrow(fut)] == "rsi_below" ) {
      
      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
      
      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") & (
      fut$exit_condition[nrow(fut)] == TRUE  | fut$crossover_RSI[nrow(fut)] =="rsi_above")) {
      
      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]
      
      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   & 
                fut$exit_condition[nrow(fut)] == FALSE  ) {
      
      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      
    } else {
      
      fut$action[nrow(fut)] <- "no action"
      
    }
    
    train_data <- fut
    # print(i)
  }
  return(train_data)
}
################################################################################
# Strategy 1

invert_EMAs_volume_Spikes <- function(fast_EMA, slow_EMA, times_sd, takeprofit, stoploss, n_mean_sd) {
  
  
  # Train and test datasets
  train_data[, c("mean_volume",
                 "sd",
                 paste0("EMA", "_", fast_EMA),
                 paste0("EMA", "_", slow_EMA),
                 "exit_value",
                 "exit_condition",
                 "crossover",
                 "crossover_Volume",
                 "action",
                 "Units",
                 "Price",
                 "id") := list(0, 0, NA, NA,NA, NA, NA, NA, NA, NA, NA, NA) ]
  
  test_data[, c("mean_volume",
                "sd",
                paste0("EMA", "_", fast_EMA),
                paste0("EMA", "_", slow_EMA),
                "exit_value",
                "exit_condition",
                "crossover",
                "crossover_Volume",
                "action",
                "Units",
                "Price",
                "id") := list(0, 0, NA, NA,NA, NA, NA, NA ,NA, NA, NA, NA) ]
  
  # Loop to evaluate last line of the test dataset
  for (i in 1:nrow(test_data)) {
    
    # Bind last row
    fut <- rbind(train_data, test_data[i, ])
    
    # Create indicators
    fut[, c(paste0("EMA", "_", fast_EMA),
            paste0("EMA", "_", slow_EMA)) := list(EMA(close, n = fast_EMA),
                                                  EMA(close, n = slow_EMA)) ]
    
    
    # Mean and sd of volume in train set
    fut$mean_volume[nrow(fut)] <- mean(tail(fut$volume, n_mean_sd))
    fut$sd[nrow(fut)] <- sd(tail(fut$volume), n_mean_sd)
    
    
    
    
    fut$crossover[get(paste0("EMA", "_", fast_EMA), fut) < get(paste0("EMA", "_", slow_EMA), fut)] <- "faster_EMA_lower"
    fut$crossover[get(paste0("EMA", "_", fast_EMA), fut) >= get(paste0("EMA", "_", slow_EMA), fut)] <- "faster_EMA_higher"
    
    # Volume Crossing of volume over the mean(volume, n_periods) + times * sd(volume, n_periods)
    fut$crossover_Volume[fut$volume > (fut$mean_volume +  times_sd * fut$sd)  ] <- "volume_higher"
    fut$crossover_Volume[fut$volume <= (fut$mean_volume +  times_sd * fut$sd)  ] <- "volume_lower"
    
    # Exit condition for stop loss 
    exit_value <- (fut$close[nrow(fut)] - tail(fut$close[!is.na(fut$action) &fut$action =="buy"], 1)) / tail(fut$close[!is.na(fut$action) &fut$action =="buy"], 1)
    
    if (length(exit_value) == 0) {
      exit_value <- 0
    }
    
    fut$exit_value[nrow(fut)] <- exit_value
    fut$exit_condition <- fut$exit_value > takeprofit | fut$exit_value <= -stoploss
    
    
    # Buy Condition
    if ((is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
        (fut$crossover[nrow(fut)] =="faster_EMA_lower"  & 
         fut$crossover_Volume[nrow(fut)] == "volume_higher") ){
      
      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)]*fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
      
      # Keep Condition
    } else if ( (fut$action[nrow(fut) - 1] %in% c("buy", "keep")) &
                (fut$exit_condition[nrow(fut)] == FALSE) ) {
      
      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      
      # Sell Condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") &  fut$exit_condition[nrow(fut)] == TRUE) {
      
      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]
      
      # No Action Condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("sell", "no action") | (is.na(fut$action[nrow(fut) - 1]) & fut$crossover[nrow(fut)] == "faster_EMA_lower" & fut$crossover_Volume[nrow(fut)] == "volume_lower")) {
      
      fut$action[nrow(fut)] <- "no action"
    }
    
    train_data <- fut
  }
  return(train_data)
}
################################################################################
# Strategy 00 ------------------------------------------------------------------

Volume_trading <- function(EMA_volume, takeprofit, stoploss_trail,stoploss_ult, times_vol, candle_action_long) {
  
  # Train and test datasets
  train_data[, c("EMA_volume",
                 "exit_value",
                 "exit_condition",
                 "crossover_volume",
                 "candle_type",
                 "action",
                 "Units",
                 "Price",
                 "tp",
                 "ult_sl",
                 "trail_sl",
                 "id") := list(NA, NA,NA, NA,NA,NA,NA, NA, NA, NA, NA, NA) ]
  
  test_data[, c("EMA_volume",
                "exit_value",
                "exit_condition",
                "crossover_volume",
                "candle_type",
                "action",
                "Units",
                "Price",
                "tp",
                "ult_sl",
                "trail_sl",
                "id") := list(NA, NA,NA, NA,NA,NA,NA, NA, NA, NA, NA, NA) ]
  
  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){
    
    fut <- rbind(train_data, test_data[i, ])
    
    # Mean and sd of volume in train set
    fut$EMA_volume <- EMA(fut$volume, n = EMA_volume)
    
    # Technical indicators -----------------------------------------------------
    
    # Volume Crossing of volume over the SMA(volume, n_periods)
    fut$crossover_volume[nrow(fut)] <- ifelse(fut$volume[nrow(fut)] > fut$EMA_volume[nrow(fut)] * times_vol ,
                                              "volume_higher", "volume_lower")
    
    fut$candle_type[fut$open > fut$close] <- "bearish"
    fut$candle_type[fut$open < fut$close] <- "bullish"
    fut$candle_type[fut$open == fut$close] <- "neutral"
    # Technical indicators -----------------------------------------------------

    
    # Exit condition for takeprofit  - Fixed
    tp <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) + takeprofit * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)
    
    if (length(tp) == 0) {
      tp <- 0
    }
    
    # Ultimate stop loss
    ult_sl <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) - stoploss_ult * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)
    
    if (length(ult_sl) == 0) {
      ult_sl <- 0
    }
    
    
    
    if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){
      
      trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      if( trail_sl < tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)){
        trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
        
      }else {
        trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
        
      }
      
      
    } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1] ) ){
      
      trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
    } else {
      
      trail_sl <-0
    }
    
    
    if(length(trail_sl) == 0 ){
      
      trail_sl <- 0
    }
    # browser()
    
    
    
    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl
    fut$trail_sl[nrow(fut)] <- trail_sl
    
    
    fut$exit_condition[nrow(fut)] <- fut$trail_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]
    
    
    
    # Deciding upon action -----------------------------------------------------
    
    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         fut$crossover_volume[nrow(fut)] == "volume_higher" & fut$candle_type[nrow(fut)] == candle_action_long) {
      
      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
      
      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") &
               fut$exit_condition[nrow(fut)] == TRUE ) {
      
      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]
      
      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   & 
                fut$exit_condition[nrow(fut)] == FALSE  ) {
      
      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      
    } else {
      
      fut$action[nrow(fut)] <- "no action"
      
    }
    
    train_data <- fut
    # print(i)
  }
  return(train_data)
}



# Strategy 0 ------------------------------------------------------------------- 
Long_term_trend_Slope_RSI <- function(RSI_Period,takeprofit, stoploss_trail,stoploss_ult,upper_RSI_Bound,lower_RSI_Bound, trend, trend_periods) {
  
  # Train and test datasets
  train_data[, c(paste0("RSI", "_", RSI_Period),
                 paste0("EMA_Trend", "_", trend),
                 "crossover",
                 "exit_value",
                 "exit_condition",
                 "RSI_Flag",
                 "Trend_Slope",
                 "tp",
                 "ult_sl",
                 "trail_sl",
                 "action",
                 "Units",
                 "Price",
                 "id") := list(NA,NA,NA,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]
  
  test_data[, c(paste0("RSI", "_", RSI_Period),
                paste0("EMA_Trend", "_", trend),
                "crossover",
                "exit_value",
                "exit_condition",
                "RSI_Flag",
                "Trend_Slope",
                "tp",
                "ult_sl",
                "trail_sl",
                "action",
                "Units",
                "Price",
                "id") := list(NA,NA,NA,NA, NA, NA,NA,NA, NA, NA, NA, NA, NA, NA)]
  
  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){
    
    # browser()
    fut <- rbind(train_data, test_data[i, ])
    fut[, c(paste0("RSI", "_", RSI_Period),
            paste0("EMA_Trend", "_", trend)) := list(RSI(close, n = RSI_Period),
                                                     EMA(close, n = trend)) ]
    # Technical indicators -----------------------------------------------------
    
    
    # RSI Crossing
    fut$RSI_Flag[get(paste0("RSI", "_", RSI_Period), fut) < lower_RSI_Bound] <- "RSI_Below" 
    fut$RSI_Flag[get(paste0("RSI", "_", RSI_Period), fut) > upper_RSI_Bound] <- "RSI_Above"
    fut$RSI_Flag[is.na(fut$RSI_Flag)] <- "In_Between"
    
    # Find if trend is positive or negative
    fut$Trend_Slope[nrow(fut)] <- get(paste0("EMA_Trend", "_", trend), fut[nrow(fut)]) - get(paste0("EMA_Trend", "_", trend), fut[nrow(fut) - trend_periods])
    
    # Exit condition for takeprofit  - Fixed
    tp <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) + takeprofit * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)
    
    if (length(tp) == 0) {
      tp <- 0
    }
    
    # Ultimate stop loss
    ult_sl <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) - stoploss_ult * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)
    
    if (length(ult_sl) == 0) {
      ult_sl <- 0
    }
    
    
    # Trailing stop loss
    
    # id_trail <- tail(fut$id[!is.na(fut$id)], 1)
    # 
    # if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){
    #   
    #   trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
    #   
    #   if(trail_sl > max(fut$trail_sl[which(fut$id == id_trail)+1], na.rm =T)){
    #     trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
    #     
    #   }else{
    #     
    #     trail_sl <- max(fut$trail_sl[which(fut$id == id_trail)+1])
    #   }
    #   
    #   
    # 
    # } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1]) ){
    #   
    #   trail_sl <- max(fut$trail_sl[which(fut$id == id_trail)])
    # 
    # } else {
    #   
    #   trail_sl <-0 
    # } 
    # 
    
    # if(length(trail_sl) == 0 ){
    #   
    #   trail_sl <- 0
    # }
    
    # Trailing stop loss
    # browser()
    if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){
      
      trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      if( trail_sl < tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)){
        trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
        
      }else {
        trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
        
      }
      
      
    } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1] ) ){
      
      trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
    } else {
      
      trail_sl <-0
    }
    
    
    if(length(trail_sl) == 0 ){
      
      trail_sl <- 0
    }
    # browser()
    
    
    
    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl
    fut$trail_sl[nrow(fut)] <- trail_sl
    
    
    fut$exit_condition[nrow(fut)] <- fut$trail_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]
    
    # Deciding upon action -----------------------------------------------------
    
    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         (fut$Trend_Slope[nrow(fut)] > 0  &  fut$RSI_Flag[nrow(fut)] == "RSI_Below")) {
      
      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
      
      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") & (
               fut$RSI_Flag[nrow(fut)] == "RSI_Above" | fut$exit_condition[nrow(fut)] == TRUE) ){
      
      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]
      
      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   & 
                fut$RSI_Flag[nrow(fut)] %in% c("RSI_Below", "In_Between" )
    )
      
    {
      
      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      
    } else {
      
      fut$action[nrow(fut)] <- "no action"
      
    }
    
    train_data <- fut
    # print(i)
  }
  return(train_data)
}


# 

# Strategy 1
invert_EMAs_volume <- function(fast_EMA, slow_EMA, RSI_period, RSI_below, Volume_EMA, takeprofit, stoploss_trail,stoploss_ult, times_vol) {
  

  
  # Train and test datasets
  train_data[, c("EMA_fast",
                 "EMA_slow",
                 "RSI",
                 "EMA_volume",
                 "exit_value",
                 "exit_condition",
                 "crossover",
                 "crossover_Volume",
                 "crossover_RSI",
                 "candle_type",
                 "action",
                 "Units",
                 "Price",
                 "tp",
                 "ult_sl",
                 "trail_sl",
                 "id") := list(NA, NA,NA, NA,NA,NA,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]
  
  test_data[, c("EMA_fast",
                "EMA_slow",
                "RSI",
                "EMA_volume",
                "exit_value",
                "exit_condition",
                "crossover",
                "crossover_Volume",
                "crossover_RSI",
                "candle_type",
                "action",
                "Units",
                "Price",
                "tp",
                "ult_sl",
                "trail_sl",
                "id") := list(NA,NA,NA, NA, NA,NA,NA, NA, NA, NA ,NA, NA, NA, NA, NA, NA, NA) ]
  
  # Loop to evaluate last line of the test dataset
  for (i in 1:nrow(test_data)) {
    
    # Bind last row
    fut <- rbind(train_data, test_data[i, ])
    
    
    # Create indicators
    fut[, c("EMA_fast",
            "EMA_slow",
            "RSI") := list(RSI(close, n = fast_EMA),
                                                           EMA(close, n = slow_EMA),
                                                           RSI(close, n = RSI_period)) ]
    
    # Candle type
    fut$candle_type[fut$open > fut$close] <- "bullish"
    fut$candle_type[fut$open < fut$close] <- "bearish"
    fut$candle_type[fut$open == fut$close] <- "neutral"
    
    # Volume Crossing of volume over the SMA(volume, n_periods)
    fut$crossover_Volume[nrow(fut)] <- ifelse(fut$volume[nrow(fut)] > fut$EMA_volume[nrow(fut)] * times_vol ,
                                              "volume_higher", "volume_lower")
    
    # RSI Crossing of upper or lower bounds
    fut$crossover[nrow(fut)] <- ifelse(fut$EMA_fast[nrow(fut)] < fut$EMA_slow[nrow(fut)] ,
                                           "EMA_lower", "EMA_higher")
    
    # RSI Crossing of upper or lower bounds
    fut$crossover_RSI[nrow(fut)] <- ifelse(fut$RSI[nrow(fut)] < RSI_below ,
                                       "RSI_lower", "RSI_higher")
    
    
    # Exit condition for takeprofit  - Fixed
    tp <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) + takeprofit * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)
    
    if (length(tp) == 0) {
      tp <- 0
    }
    
    # Ultimate stop loss
    ult_sl <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) - stoploss_ult * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)
    
    if (length(ult_sl) == 0) {
      ult_sl <- 0
    }
    
    
    
    # Trailing stop loss
    # browser()
    if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){
      
      trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      if( trail_sl < tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)){
        trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
        
      }else {
        trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
        
      }
      
      
    } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1] ) ){
      
      trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
    } else {
      
      trail_sl <-0
    }
    
    
    if(length(trail_sl) == 0 ){
      
      trail_sl <- 0
    }
    
    
    
    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl
    fut$trail_sl[nrow(fut)] <- trail_sl
    
    
    fut$exit_condition[nrow(fut)] <- fut$trail_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]
    
    
    # Buy Condition
    if ((is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
                                                     fut$crossover[nrow(fut)] == "EMA_lower"  & 
                                                     fut$crossover_RSI[nrow(fut)] == "RSI_lower" ){
      
      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)]*fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
      
    # Keep Condition
    } else if (fut$action[nrow(fut) - 1] %in% c("buy", "keep") &
               (fut$exit_condition[nrow(fut)] == FALSE   )) {
      
      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      
    # Sell Condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") &  fut$exit_condition[nrow(fut)] == TRUE) {
      
      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]
      
    # No Action Condition
    } else {
     
      fut$action[nrow(fut)] <- "no action"
    }
    
    train_data <- fut
  }
  return(train_data)
}

# Strategy 2 -------------------------------------------------------------------
# Crossing fast ema to slower with stoploss condition

cross_EMA_stoploss <- function(fast_EMA, slow_EMA, takeprofit, stoploss_trail,stoploss_ult) {
  
  # Train and test datasets$
  train_data[, c(paste0("EMA", "_", fast_EMA),
                 paste0("EMA", "_", slow_EMA),
                 "exit_value","exit_condition",
                 "crossover",
                 "crossover_Volume",
                 "action",
                 "Units",
                 "Price",
                 "tp",
                 "ult_sl",
                 "trail_sl",
                 "id") := list(NA, NA, NA,NA,NA,NA, NA, NA, NA, NA, NA, NA, NA) ]
  
  test_data[, c(paste0("EMA", "_", fast_EMA),
                paste0("EMA", "_", slow_EMA),
                "exit_value",
                "exit_condition",
                "crossover",
                "crossover_Volume",
                "action",
                "Units",
                "Price",
                "tp",
                "ult_sl",
                "trail_sl",
                "id") := list(NA, NA, NA,NA,NA,NA, NA, NA, NA ,NA, NA, NA, NA) ]
  
  # Loop to evaluate last line of the test dataset
  for (i in 1:nrow(test_data)) {
    
    # Bind last row
    fut <- rbind(train_data, test_data[i, ])
    
    # Create indicators
    fut[, c(paste0("EMA", "_", fast_EMA),
            paste0("EMA", "_", slow_EMA)) := list(EMA(close, n = fast_EMA),
                                                  EMA(close, n = slow_EMA)) ]
    
      
    fut$crossover[get(paste0("EMA", "_", fast_EMA), fut) > get(paste0("EMA", "_", slow_EMA), fut)] <- "faster_EMA_higher"
    fut$crossover[get(paste0("EMA", "_", fast_EMA), fut) <= get(paste0("EMA", "_", slow_EMA), fut)] <- "faster_EMA_lower"
      
    
    # Exit condition for takeprofit  - Fixed
    tp <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) + takeprofit * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)
    
    if (length(tp) == 0) {
      tp <- 0
    }
    
    # Ultimate stop loss
    ult_sl <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) - stoploss_ult * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)
    
    if (length(ult_sl) == 0) {
      ult_sl <- 0
    }
    
    
    # Trailing stop loss
    
    # id_trail <- tail(fut$id[!is.na(fut$id)], 1)
    # 
    # if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){
    #   
    #   trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
    #   
    #   if(trail_sl > max(fut$trail_sl[which(fut$id == id_trail)+1], na.rm =T)){
    #     trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
    #     
    #   }else{
    #     
    #     trail_sl <- max(fut$trail_sl[which(fut$id == id_trail)+1])
    #   }
    #   
    #   
    # 
    # } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1]) ){
    #   
    #   trail_sl <- max(fut$trail_sl[which(fut$id == id_trail)])
    # 
    # } else {
    #   
    #   trail_sl <-0 
    # } 
    # 
    
    # if(length(trail_sl) == 0 ){
    #   
    #   trail_sl <- 0
    # }
    
    # Trailing stop loss
    # browser()
    if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){
      
      trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      if( trail_sl < tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)){
        trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
        
      }else {
        trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
        
      }
      
      
    } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1] ) ){
      
      trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
    } else {
      
      trail_sl <-0
    }
    
    
    if(length(trail_sl) == 0 ){
      
      trail_sl <- 0
    }
    # browser()
    
    
    
    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl
    fut$trail_sl[nrow(fut)] <- trail_sl
    
    
    fut$exit_condition[nrow(fut)] <- fut$trail_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]
    
    # BUY Condition
    if ((is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action"))
                                                    & fut$crossover[nrow(fut)] == "faster_EMA_higher") {
      
      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)]*fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
    
    # Keep Condition  
    } else if (fut$action[nrow(fut) - 1] %in% c("buy", "keep") &
               fut$crossover[nrow(fut)] == "faster_EMA_higher" &
               fut$exit_condition[nrow(fut)] == FALSE ) {
      
      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
    
    # Sell Condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") &
              (fut$crossover[nrow(fut)] %in% c("faster_EMA_lower", "faster_EMA_higher") &  fut$exit_condition[nrow(fut)] == TRUE | fut$crossover[nrow(fut)] %in% c("faster_EMA_lower"))) {
      
      
      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]
    
    # No Action Condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("sell", "no action") | (is.na(fut$action[nrow(fut) - 1])
                                                       & fut$crossover[nrow(fut)] == "faster_EMA_lower")) {
      
      fut$action[nrow(fut)] <- "no action"
    }
    
    train_data <- fut
  }
  return(train_data)
}


# Stategy 3 --------------------------------------------------------------------
RSI_Crossover_EMA_Slope <- function(RSI_Period, upper_RSI_Bound, lower_RSI_Bound, trend_EMA) {
  
  # Train and test datasets
  train_data[, c("Slope_EMA",
                 paste0("RSI", "_", RSI_Period),
                 paste0("trend_EMA", "_", trend_EMA),
                 "crossover",
                 "action",
                 "Units",
                 "Price",
                 "id") := list(NA, NA, NA, NA, NA, NA, NA, NA) ]
  
  test_data[, c("Slope_EMA",
                paste0("RSI", "_", RSI_Period),
                paste0("trend_EMA", "_", trend_EMA),
                "crossover",
                "action",
                "Units",
                "Price",
                "id") := list(NA, NA, NA ,NA, NA, NA, NA, NA) ]
  
  # Loop to evaluate last line of the test dataset
  for (i in 1:nrow(test_data)) {
    
    # Bind last row
    fut <- rbind(train_data, test_data[i, ])
    
    # Create Indicators
    fut[, c(paste0("RSI", "_", RSI_Period)) := list(RSI(close, n = RSI_Period)) ]
    fut[, c(paste0("trend_EMA", "_", trend_EMA)) := list(EMA(close, n = trend_EMA)) ]
    
    # Slope
    fut$Slope_EMA[nrow(fut)] <- get(paste0("trend_EMA_", trend_EMA), fut[nrow(fut)]) - get(paste0("trend_EMA_", trend_EMA), fut[nrow(fut) -1])
    
    
    fut$crossover[get(paste0("RSI", "_", RSI_Period), fut) > upper_RSI_Bound] <- "RSI_Higher"
    fut$crossover[get(paste0("RSI", "_", RSI_Period), fut) <= lower_RSI_Bound] <- "RSI_Lower"
    fut$crossover[is.na(fut$crossover)] <- "In_Between"
    
    # BUY Condition
    if (  (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
          fut$crossover[nrow(fut)] =="RSI_Lower" & fut$Slope_EMA[nrow(fut)] > 0) {
      
      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)]*fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
    
    # Keep Condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep") &  fut$crossover[nrow(fut)] %in% c("In_Between","RSI_Lower")  ) {
      
      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
    
    # Sell Condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("keep", "buy") &  fut$crossover[nrow(fut)] %in% c("RSI_Higher")  ) {
      
      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]
    
    # No action condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("sell", "no action") | (is.na(fut$action[nrow(fut) - 1]) & fut$crossover[nrow(fut)] == "In_Between")) {
      
      fut$action[nrow(fut)] <- "no action"
    }
    
    train_data <- fut
  }
  return(train_data)
}

# Strategy 4 -------------------------------------------------------------------
# Combine crossover with volume, RSI and Overall trend
RSI_Swings <- function(fast_EMA, slow_EMA, Volume_EMA, RSI_Period, upper_RSI_Bound,lower_RSI_Bound, stoploss, trend, trend_periods) {
  
  # Train and test datasets
  train_data[, c(paste0("EMA", "_", fast_EMA),
                 paste0("EMA", "_", slow_EMA),
                 paste0("Volume_EMA", "_", Volume_EMA),
                 paste0("RSI", "_", RSI_Period),
                 paste0("EMA_Trend", "_", trend),
                 "crossover",
                 "crossover_Volume",
                 "RSI_Flag",
                 "exit_value",
                 "exit_condition",
                 "Trend_Slope",
                 "action",
                 "Units",
                 "Price",
                 "id") := list(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA ,NA,NA, NA, NA, NA) ]
  
  test_data[, c(paste0("EMA", "_", fast_EMA),
                paste0("EMA", "_", slow_EMA),
                paste0("Volume_EMA", "_", Volume_EMA),
                paste0("RSI", "_", RSI_Period),
                paste0("EMA_Trend", "_", trend),
                "crossover",
                "crossover_Volume",
                "RSI_Flag",
                "exit_value",
                "exit_condition",
                "Trend_Slope",
                "action",
                "Units",
                "Price",
                "id") := list(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA ,NA, NA, NA, NA, NA)]
  
  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){
    
    fut <- rbind(train_data, test_data[i, ])
    fut[, c(paste0("EMA", "_", fast_EMA),
            paste0("EMA", "_", slow_EMA),
            paste0("Volume_EMA", "_", Volume_EMA),
            paste0("RSI", "_", RSI_Period),
            paste0("EMA_Trend", "_", trend)) := list(EMA(close, n = fast_EMA),
                                                     EMA(close, n = slow_EMA),
                                                     EMA(volume, n = Volume_EMA),
                                                     RSI(close, n = RSI_Period),
                                                     EMA(close, n = trend)) ]
    
    # Technical indicators -----------------------------------------------------
    
    # EMA Crossing
    fut$crossover[get(paste0("EMA", "_", fast_EMA), fut) < get(paste0("EMA", "_", slow_EMA), fut)] <- "faster_EMA_lower"
    fut$crossover[get(paste0("EMA", "_", fast_EMA), fut) >= get(paste0("EMA", "_", slow_EMA), fut)] <- "faster_EMA_higher"
    
    # Volume Crossing
    fut$crossover_Volume[get(paste0("Volume_EMA", "_", Volume_EMA), fut) < fut$volume] <- "volume_higher" 
    fut$crossover_Volume[get(paste0("Volume_EMA", "_", Volume_EMA), fut) >= fut$volume] <- "volume_lower"
    
    # RSI Crossing
    fut$RSI_Flag[get(paste0("RSI", "_", RSI_Period), fut) < lower_RSI_Bound] <- "RSI_Below" 
    fut$RSI_Flag[get(paste0("RSI", "_", RSI_Period), fut) > upper_RSI_Bound] <- "RSI_Above"
    fut$RSI_Flag[is.na(fut$RSI_Flag)] <- "In_Between"
    
    # Find if trend is positive or negative
    fut$Trend_Slope[nrow(fut)] <- get(paste0("EMA_Trend", "_", trend), fut[nrow(fut)]) - get(paste0("EMA_Trend", "_", trend), fut[nrow(fut) - trend_periods])
    
    # Exit condition for stop loss ---------------------------------------------
    exit_value <- (fut$close[nrow(fut)] - tail(fut$close[!is.na(fut$action) & fut$action == "buy"], 1)) / tail(fut$close[!is.na(fut$action) & fut$action == "buy"], 1)
    
    if (length(exit_value) == 0) {
      exit_value <- 0
    }
    
    fut$exit_value[nrow(fut)] <- exit_value
    fut$exit_condition <- fut$exit_value <= -stoploss
    
    # Deciding upon action -----------------------------------------------------
    
    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         (fut$Trend_Slope[nrow(fut)] > 0 ) &
         (fut$crossover[nrow(fut)] == "faster_EMA_lower")  & (
           (fut$crossover_Volume[nrow(fut)] == "volume_higher" | fut$RSI_Flag[nrow(fut)] == "RSI_Below")) ){
      
      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
      
      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") &
               fut$crossover[nrow(fut)] == "faster_EMA_higher" &
               fut$RSI_Flag[nrow(fut)] == "RSI_Above") {
      
      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]
      
      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   & (
      (fut$crossover[nrow(fut)] == "faster_EMA_higher" &  fut$crossover_Volume[nrow(fut)] == "volume_higher" &  fut$RSI_Flag[nrow(fut)] == "RSI_Below") |
      (fut$crossover[nrow(fut)] == "faster_EMA_higher" &  fut$crossover_Volume[nrow(fut)] == "volume_higher" &  fut$RSI_Flag[nrow(fut)] == "In_Between") |
      (fut$crossover[nrow(fut)] == "faster_EMA_higher" &  fut$crossover_Volume[nrow(fut)] == "volume_lower" &  fut$RSI_Flag[nrow(fut)] == "RSI_Below") |
      (fut$crossover[nrow(fut)] == "faster_EMA_higher" &  fut$crossover_Volume[nrow(fut)] == "volume_lower" &  fut$RSI_Flag[nrow(fut)] == "In_Between") |
      (fut$crossover[nrow(fut)] == "faster_EMA_lower" &  fut$crossover_Volume[nrow(fut)] == "volume_higher" &  fut$RSI_Flag[nrow(fut)] == "RSI_Above") |
      (fut$crossover[nrow(fut)] == "faster_EMA_lower" &  fut$crossover_Volume[nrow(fut)] == "volume_lower" &  fut$RSI_Flag[nrow(fut)] == "In_Between") |
      (fut$crossover[nrow(fut)] == "faster_EMA_lower" &  fut$crossover_Volume[nrow(fut)] == "volume_lower" &  fut$RSI_Flag[nrow(fut)] == "RSI_Above") |
      ((fut$crossover[nrow(fut)] == "faster_EMA_lower")  & (
        (fut$crossover_Volume[nrow(fut)] == "volume_higher" | fut$RSI_Flag[nrow(fut)] == "RSI_Below")))))
      
    {
      
      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      
    } else {
      
      fut$action[nrow(fut)] <- "no action"
      
    }
    
    train_data <- fut
  }
  return(train_data)
}
# ------------------------------------------------------------------------------
# Strategy 4
cross_4_EMA_stoploss <- function(EMA1, EMA2, EMA3, stoploss, takeprofit) {
  
  # Train and test datasets$
  train_data[, c(paste0("EMA", "_", EMA1),
                 paste0("EMA", "_", EMA2),
                 paste0("EMA", "_", EMA3),
                 "exit_value",
                 "exit_condition",
                 "crossover",
                 "crossover_Volume",
                 "action",
                 "Units",
                 "Price",
                 "id") := list(NA, NA, NA,  NA, NA, NA, NA, NA, NA, NA, NA) ]
  
  test_data[, c(paste0("EMA", "_", EMA1),
                paste0("EMA", "_", EMA2),
                paste0("EMA", "_", EMA3),
                "exit_value",
                "exit_condition",
                "crossover",
                "crossover_Volume",
                "action",
                "Units",
                "Price",
                "id") := list(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]
  
  # Loop to evaluate last line of the test dataset
  for (i in 1:nrow(test_data)) {
    
    # Bind last row
    fut <- rbind(train_data, test_data[i, ])
    
    # Create indicators
    fut[, c(paste0("EMA", "_", EMA1),
            paste0("EMA", "_", EMA2),
            paste0("EMA", "_", EMA3)) := list(EMA(close, n = EMA1),
                                                  EMA(close, n = EMA2),
                                              EMA(close, n = EMA3)) ]
    
    
    fut$crossover[get(paste0("EMA", "_", EMA1), fut) > get(paste0("EMA", "_", EMA2), fut) & get(paste0("EMA", "_", EMA2), fut) > get(paste0("EMA", "_", EMA3), fut)  ] <- "faster_EMA_higher"
    fut$crossover[is.na(fut$crossover)] <- "faster_EMA_lower"
    # Exit condition for stop loss 
    exit_value <- (fut$close[nrow(fut)] - tail(fut$close[!is.na(fut$action) &fut$action =="buy"], 1)) / tail(fut$close[!is.na(fut$action) &fut$action =="buy"], 1)
    
    if (length(exit_value) == 0) {
      exit_value <- 0
    }
    
    fut$exit_value[nrow(fut)] <- exit_value
    fut$exit_condition <- fut$exit_value > takeprofit | fut$exit_value <= -stoploss
    
    # BUY Condition
    if ((is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) & fut$crossover[nrow(fut)] == "faster_EMA_higher") {
      
      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)]*fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
      
      # Keep Condition  
    } else if (fut$action[nrow(fut) - 1] %in% c("buy", "keep") &(
               fut$crossover[nrow(fut)] == "faster_EMA_higher" &
               fut$exit_condition[nrow(fut)] == FALSE )) {
      
      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      
      # Sell Condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") &
  (fut$exit_condition[nrow(fut)] == TRUE | fut$crossover[nrow(fut)] %in% c("faster_EMA_lower"))) {
      
      
      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]
      
      # No Action Condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("sell", "no action") | (is.na(fut$action[nrow(fut) - 1])
                                                                         & fut$crossover[nrow(fut)] == "faster_EMA_lower")) {
      
      fut$action[nrow(fut)] <- "no action"
    }
    
    train_data <- fut
    # print(i)
  }
  return(train_data)
}

# Ninja trader Volume reversal with RSI ----------------------------------------
Volume_Reversal_RSI_NJ <- function(stoploss, rsi_bound, rsi_period, periods_volume, times_EMA_Vol, EMA_periods) {
  
  # Train and test datasets
  train_data[, c("exit_value",
                 "exit_condition",
                 "EMA_volume",
                 "EMA",
                 "RSI",
                 "crossover_volume",
                 "candle_pattern",
                 "Long_singal",
                 "action",
                 "Units",
                 "Price",
                 "id") := list(0,NA, NA,NA,NA,NA, NA, 0, NA, NA, NA, NA) ]
  
  test_data[, c("exit_value",
                "exit_condition",
                "EMA_volume",
                "EMA",
                "RSI",
                "crossover_volume",
                "candle_pattern",
                "Long_singal",
                "action",
                "Units",
                "Price",
                "id") := list(0, NA,NA,NA,NA,NA, NA, 0, NA, NA, NA, NA) ]
  
  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){
    
    fut <- rbind(train_data, test_data[i, ])
    
    
    # Technical indicators -----------------------------------------------------
    
    
    fut$EMA_volume <- EMA(fut$volume, periods_volume)
    fut$crossover_volume[nrow(fut)-1] <-  ifelse(fut$volume[nrow(fut)-1] >  fut$EMA_volume[nrow(fut)-1]*times_EMA_Vol, "Volume_Spike","No_Spike")
    # fut$crossover_volume[nrow(fut)-1] <- ifelse(fut$volume[nrow(fut)-1] >  max(fut$volume[(nrow(fut)-periods_volume): (nrow(fut)-2)]), "Volume_Spike","No_Spike")
    fut$RSI <- RSI(fut$close, n = rsi_period)
    
    fut$candle_pattern[nrow(fut)] <-  (fut$low[nrow(fut)-2] <= fut$low[nrow(fut)-1]) & (fut$low[nrow(fut)-2] <= fut$low[nrow(fut)]) & 
      (fut$close[nrow(fut)-2] >= fut$close[nrow(fut)-1]) & (fut$close[nrow(fut)-2] >= fut$open[nrow(fut)-2]) &
      (fut$close[nrow(fut)-2] - fut$open[nrow(fut)-2]) < (fut$high[nrow(fut)-2] - fut$low[nrow(fut)-2])
    
    
    fut$Long_singal[nrow(fut)][fut$candle_pattern[nrow(fut)] == TRUE & fut$crossover[nrow(fut)-1] =="Volume_Spike"] <- TRUE
    
    fut$EMA <- EMA(fut$close, n = EMA_periods)
    
    # Exit condition for stop loss 
    exit_value <- (fut$close[nrow(fut)] - tail(fut$close[!is.na(fut$action) & fut$action %in% c("buy", "keep")], 1)) / tail(fut$close[!is.na(fut$action) &fut$action %in% c("buy", "keep")], 1)
    
    if (length(exit_value) == 0) {
      exit_value <- 0
    }
    
    fut$exit_value[nrow(fut)] <- exit_value
    fut$exit_condition <-  fut$exit_value <= -stoploss
    
    
    
    # Deciding upon action -----------------------------------------------------
    
    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         fut$Long_singal[nrow(fut)] == TRUE & fut$RSI[nrow(fut)] < rsi_bound) {
      
      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
      
      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") &
               fut$exit_condition[nrow(fut)] == TRUE ) {
      
      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]
      
      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   & 
                fut$exit_condition[nrow(fut)] == FALSE  ) {
      
      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      
    } else {
      
      fut$action[nrow(fut)] <- "no action"
      
    }
    
    train_data <- fut
    # print(i)
  }
  return(train_data)
}

 
# Test  ------------------------------------------------------------------------
Pure_RSI_Volume_Trailing_test <- function(RSI_Period,RSI_above,  RSI_below, EMA_volume, takeprofit, stoploss_trail,stoploss_ult, times_vol) {
  
  # Train and test datasets
  train_data[, c("RSI",
                 "EMA_volume",
                 "exit_value",
                 "exit_condition",
                 "crossover_volume",
                 "crossover_RSI",
                 "action",
                 "Units",
                 "Price",
                 "tp",
                 "ult_sl",
                 "trail_sl",
                 "id") := list(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]
  
  test_data[, c("RSI",
                "EMA_volume",
                "exit_value",
                "exit_condition",
                "crossover_volume",
                "crossover_RSI",
                "action",
                "Units",
                "Price",
                "tp",
                "ult_sl",
                "trail_sl",
                "id") := list(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]
  
  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){
    
    fut <- rbind(train_data, test_data[i, ])
    
    # RSI and Volume
    fut$RSI <- RSI(fut$close, n = RSI_Period)
    fut$EMA_volume <- EMA(fut$volume, n = EMA_volume)
    
    # Technical indicators -----------------------------------------------------
    
    
    # Volume Crossing of volume over the SMA(volume, n_periods)
    fut$crossover_volume[nrow(fut)] <- ifelse(fut$volume[nrow(fut)] > fut$EMA_volume[nrow(fut)] * times_vol ,
                                              "volume_higher", "volume_lower")
    
    
    # RSI Crossing of upper or lower bounds
    if(fut$RSI[nrow(fut)] < RSI_below){
      
      fut$crossover_RSI[nrow(fut)] <- "RSI_lower"
      
    } else if (fut$RSI[nrow(fut)] > RSI_above){
      
      fut$crossover_RSI[nrow(fut)] <- "RSI_above"
    } else {
      
      fut$crossover_RSI[nrow(fut)] <- "RSI_between"
    }
    
    
    
    # Exit condition for takeprofit  - Fixed
    tp <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) + takeprofit * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)
    
    if (length(tp) == 0) {
      tp <- 0
    }
    
    # Ultimate stop loss
    ult_sl <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) - stoploss_ult * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)
    
    if (length(ult_sl) == 0) {
      ult_sl <- 0
    }
    
    
    # Trailing stop loss
    
    # id_trail <- tail(fut$id[!is.na(fut$id)], 1)
    # 
    # if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){
    #   
    #   trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
    #   
    #   if(trail_sl > max(fut$trail_sl[which(fut$id == id_trail)+1], na.rm =T)){
    #     trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
    #     
    #   }else{
    #     
    #     trail_sl <- max(fut$trail_sl[which(fut$id == id_trail)+1])
    #   }
    #   
    #   
    # 
    # } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1]) ){
    #   
    #   trail_sl <- max(fut$trail_sl[which(fut$id == id_trail)])
    # 
    # } else {
    #   
    #   trail_sl <-0 
    # } 
    # 
    
    # if(length(trail_sl) == 0 ){
    #   
    #   trail_sl <- 0
    # }
    
    # Trailing stop loss
    # browser()
    if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){
      
      trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      if( trail_sl < tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)){
        trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
        
      }else {
        trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
        
      }
      
      
    } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1] ) ){
      
      trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
    } else {
      
      trail_sl <-0
    }
    
    
    if(length(trail_sl) == 0 ){
      
      trail_sl <- 0
    }
    # browser()
    
    
    
    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl
    fut$trail_sl[nrow(fut)] <- trail_sl
    
    
    fut$exit_condition[nrow(fut)] <- fut$trail_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]
    
    
    # Deciding upon action -----------------------------------------------------
    
    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         fut$crossover_volume[nrow(fut)] == "volume_higher" &  fut$crossover_RSI[nrow(fut)] == "RSI_above" ) {
      
      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
      
      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") & (
      fut$exit_condition[nrow(fut)] == TRUE  )) {
      
      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]
      
      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   & 
                fut$exit_condition[nrow(fut)] == FALSE  ) {
      
      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      
    } else {
      
      fut$action[nrow(fut)] <- "no action"
      
    }
    
    train_data <- fut
    print(i)
  }
  return(train_data)
}



# ------------------------------------------------------------------------------
# Intented for Forex

EMA_below <- function(EMA_period, percent_below, takeprofit, stoploss_trail,stoploss_ult) {
  
  # Train and test datasets
  train_data[, c("EMA",
                 "exit_condition",
                 "action",
                 "Units",
                 "Price",
                 "tp",
                 "ult_sl",
                 "trail_sl",
                 "id") := list(NA, NA, NA, NA, NA, NA, NA, NA, NA) ]
  
  test_data[, c("EMA",
                "exit_condition",
                "action",
                "Units",
                "Price",
                "tp",
                "ult_sl",
                "trail_sl",
                "id") := list(NA, NA, NA, NA, NA, NA, NA, NA, NA) ]
  
  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){
    
    fut <- rbind(train_data, test_data[i, ])
    
    # RSI and Volume
    fut$EMA <- EMA(fut$close, n = EMA_period)
    
  
    
  
    # Exit condition for takeprofit  - Fixed
    tp <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) + takeprofit * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)
    
    if (length(tp) == 0) {
      tp <- 0
    }
    
    # Ultimate stop loss
    ult_sl <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) - stoploss_ult * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)
    
    if (length(ult_sl) == 0) {
      ult_sl <- 0
    }
    
    
    # Trailing stop loss
    
    # id_trail <- tail(fut$id[!is.na(fut$id)], 1)
    # 
    # if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){
    #   
    #   trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
    #   
    #   if(trail_sl > max(fut$trail_sl[which(fut$id == id_trail)+1], na.rm =T)){
    #     trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
    #     
    #   }else{
    #     
    #     trail_sl <- max(fut$trail_sl[which(fut$id == id_trail)+1])
    #   }
    #   
    #   
    # 
    # } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1]) ){
    #   
    #   trail_sl <- max(fut$trail_sl[which(fut$id == id_trail)])
    # 
    # } else {
    #   
    #   trail_sl <-0 
    # } 
    # 
    
    # if(length(trail_sl) == 0 ){
    #   
    #   trail_sl <- 0
    # }
    
    # Trailing stop loss
    # browser()
    if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){
      
      trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      if( trail_sl < tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)){
        trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
        
      }else {
        trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
        
      }
      
      
    } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1] ) ){
      
      trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
    } else {
      
      trail_sl <-0
    }
    
    
    if(length(trail_sl) == 0 ){
      
      trail_sl <- 0
    }
    # browser()
    
    
    
    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl
    fut$trail_sl[nrow(fut)] <- trail_sl
    
    
    fut$exit_condition[nrow(fut)] <- fut$trail_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]
    
    
    # Deciding upon action -----------------------------------------------------
    
    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
          fut$close[nrow(fut)]  < fut$EMA[nrow(fut)]*(1-percent_below)) {
      
      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
      
      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") & (
      fut$exit_condition[nrow(fut)] == TRUE  )) {
      
      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]
      
      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   & 
                fut$exit_condition[nrow(fut)] == FALSE  ) {
      
      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      
    } else {
      
      fut$action[nrow(fut)] <- "no action"
      
    }
    
    train_data <- fut
    print(i)
  }
  return(train_data)
}

# ------------------------------------------------------------------------------
# Enchanced previous version of RSI and Volume trailing stop loss with resistance and support levels

Pure_RSI_Volume_Trailing_Dynamic_SRL <- function(RSI_Period, RSI_below, EMA_volume, takeprofit, stoploss_trail,stoploss_ult, times_vol, df_tail, mean_n) {
  
  # Train and test datasets
  train_data[, c("RSI",
                 "EMA_volume",
                 "exit_value",
                 "exit_condition",
                 "support",
                 "resistance",
                 "crossover_SL_RS",
                 "crossover_volume",
                 "crossover_RSI",
                 "action",
                 "Units",
                 "Price",
                 "tp",
                 "ult_sl",
                 "trail_sl",
                 "id") := list(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]
  
  test_data[, c("RSI",
                "EMA_volume",
                "exit_value",
                "exit_condition",
                "support",
                "resistance",
                "crossover_SL_RS",
                "crossover_volume",
                "crossover_RSI",
                "action",
                "Units",
                "Price",
                "tp",
                "ult_sl",
                "trail_sl",
                "id") := list(NA, NA, NA,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]
  
  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)) {
    
    fut <- rbind(train_data, test_data[i, ])
    
    # RSI and Volume
    fut$RSI <- RSI(fut$close, n = RSI_Period)
    fut$EMA_volume <- EMA(fut$volume, n = EMA_volume)
    
    # Technical indicators -----------------------------------------------------
    
    
    # Volume Crossing of volume over the SMA(volume, n_periods)
    fut$crossover_volume[nrow(fut)] <- ifelse(fut$volume[nrow(fut)] > fut$EMA_volume[nrow(fut)] * times_vol ,
                                              "volume_higher", "volume_lower")
    
    
    # RSI Crossing of upper or lower bounds
    fut$crossover_RSI[nrow(fut)] <- ifelse(fut$RSI[nrow(fut)] < RSI_below ,
                                           "RSI_lower", "RSI_higher")
    
    # calculate resistance and support level
    fut$support[nrow(fut)] <- mean(tail(sort(tail(fut$close, df_tail)), mean_n))
    fut$resistance[nrow(fut)] <- mean(head(sort(tail(candles$close, df_tail)), mean_n))
    
    if( fut$close[nrow(fut)] < fut$support[nrow(fut)]) {
      
      fut$crossover_SL_RS[nrow(fut)] <- "Below_support"
      
    } else if (fut$close[nrow(fut)] > fut$resistance[nrow(fut)] ) {
      
      fut$crossover_SL_RS[nrow(fut)] <- "Above_resistance"
    }else{
      
      fut$crossover_SL_RS[nrow(fut)] <- "Above_resistance"
    }
    
    # abline( h =   fut$resistance[nrow(fut)])
    # abline( h = fut$support[nrow(fut)] )
    
    
    # Exit condition for takeprofit  - Fixed
    tp <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) + takeprofit * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)
    
    if (length(tp) == 0) {
      tp <- 0
    }
    
    # Ultimate stop loss
    ult_sl <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) - stoploss_ult * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)
    
    if (length(ult_sl) == 0) {
      ult_sl <- 0
    }
    
    
    
    # Trailing stop loss
    # browser()
    if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){
      
      trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      if( trail_sl < tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)){
        trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
        
      }else {
        trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
        
      }
      
      
    } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1] ) ){
      
      trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
    } else {
      
      trail_sl <-0
    }
    
    
    if(length(trail_sl) == 0 ){
      
      trail_sl <- 0
    }
    # browser()
    
    
    
    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl
    fut$trail_sl[nrow(fut)] <- trail_sl
    
    
    fut$exit_condition[nrow(fut)] <- fut$trail_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]
    
    
    # Deciding upon action -----------------------------------------------------
    
    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         fut$crossover_volume[nrow(fut)] == "volume_higher" &  fut$crossover_RSI[nrow(fut)] == "RSI_lower"  & fut$crossover_SL_RS[nrow(fut)] == "Below_support") {
      
      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
      
      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") & (
      fut$exit_condition[nrow(fut)] == TRUE  )) {
      
      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]
      
      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   & 
                fut$exit_condition[nrow(fut)] == FALSE  ) {
      
      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      
    } else {
      
      fut$action[nrow(fut)] <- "no action"
      
    }
    
    train_data <- fut
    print(i)
  }
  return(train_data)
}


# ------------------------------------------------------------------------------
simple_OHLC <- function(interval, pair){
  
  
  what <- tryCatch(
    {
      url <- paste0('https://api.kraken.com/0/public/OHLC?pair=',pair,'&interval=', interval)
      dat <- jsonlite::fromJSON(url)
    },
    error = function(e){})
  
  df <- as.data.table(dat$result[1])
  colnames(df) <- c("time", "open", "high", "low", "close",
                    "vwap", "volume", "count")
  df[, Date_POSIXct := anytime(as.numeric(as.character(time)))]
  df$Date_POSIXct <- as.character(df$Date_POSIXct)
  
  
  # as numeric
  df$open <- as.numeric(df$open)
  df$high <- as.numeric(df$high)
  df$low <- as.numeric(df$low)
  df$close <- as.numeric(df$close)
  df$volume <- as.numeric(df$volume)
  
  return(df)
  
}


###
# Calculate profits and number of trades ---------------------------------------

calculate_profits <- function(dataset, params){
  
  calcu <- dataset[action %in% c("buy", "sell"), ]
  calcu <- subset(calcu,  !calcu$id %in% names(which(table(calcu$id) ==1)))
  if (nrow(calcu) > 0) {
  
  profit <- c()
  profit_sum <- c()
  ids <- unique(calcu$id)
  for(i in 1:length(ids)){
    
    profit[i] <-   calcu$Price[calcu$action =="sell" & calcu$id == ids[i]] - calcu$Price[calcu$action =="buy" & calcu$id == ids[i]] 
  }
  profit_sum <- sum(profit)
  dd <- data.frame(profit = profit_sum, n_trades = length(unique(calcu$id)),
                   enter_date = unique(calcu$Date)[1], exit_date = tail(unique(calcu$Date), 1))
  } else {
    
    dd <- data.frame(profit = 0, n_trades = 0, enter_date = as.Date("2020-04-07"), exit_date =as.Date("2020-04-07"))
  }
  
  if(paraller_exec ==TRUE){
    dd$params <- params
  }
  
  write.table(dd, "/media/chris/DATA/Documents/Bot_Trading/Historical_data/myDF.csv",
              sep = ",", row.names = FALSE, col.names = !file.exists("/media/chris/DATA/Documents/Bot_Trading/Historical_data/myDF.csv"), append = T)  
  return(dd)
}

# Private API calls ------------------------------------------------------------
myfun <- function (url, key, secret) {
  
  # Nonce and post info
  nonce <- as.character(as.numeric(Sys.time()) * 1000000)
  post_data <- paste0("nonce=", nonce)
  
  # Strip kraken url
  method_path <- gsub("^.*?kraken.com", "", url)
  
  # Secret APi key 
  sign <- hmac(key =  RCurl::base64Decode(secret, mode = "raw"), 
               object = c(charToRaw(method_path), digest(object = paste0(nonce, post_data),
                                                         algo = "sha256",
                                                         serialize = FALSE, 
                                                         raw = TRUE)),
               algo = "sha512", raw = TRUE)
  # Header
  httpheader <- c(`API-Key` = key, `API-Sign` =  RCurl::base64Encode(sign))
  
  curl <- RCurl::getCurlHandle(useragent = paste("Rbitcoin", packageVersion("Rbitcoin")))
  query_result_json <- rawToChar(RCurl::getURLContent(curl = curl, 
                                                      url = url,
                                                      binary = TRUE,
                                                      postfields = post_data, 
                                                      httpheader = httpheader))
  query_result <- jsonlite::fromJSON(query_result_json)
  
  return(query_result)
}

# Download OLHC data and calcualte indicators. Write last line and append each time
#-------------------------------------------------------------------------------

OHLC_action <- function(pair, interval){
  repeat{
    # 1. Get the OHLC - Repeat this call every x interval
    what <- tryCatch(
      {
        url <- paste0('https://api.kraken.com/0/public/OHLC?pair=',pair,'&interval=', interval)
        dat <- jsonlite::fromJSON(url)
      },
      error = function(e){})
    
    if(is.null(dat$result[1])) next # error, skip
    if(nrow(as.data.frame(dat$result[1])) == 0) break # last batch empty

    df <- as.data.table(dat$result$XETHZEUR)
    colnames(df) <- c("time", "open", "high", "low", "close",
                      "vwap", "volume", "count")
    df[, Date_POSIXct := anytime(as.numeric(as.character(time)))]
    
    # as numeric
    df$open <- as.numeric(df$open)
    df$high <- as.numeric(df$high)
    df$low <- as.numeric(df$low)
    df$close <- as.numeric(df$close)
    
    # 2. Add Indicators
    df[, c(paste0("EMA", "_", fast_EMA), paste0("EMA", "_", slow_EMA), "crossover", "action", "Units", "Price", "id") := list(NA, NA,0, NA, NA, NA, NA) ]
    df[, c(paste0("EMA", "_", fast_EMA), paste0("EMA", "_", slow_EMA)) := list( EMA(close, n = fast_EMA), EMA(close, n = slow_EMA) ) ]
    df$crossover[get(paste0("EMA", "_", fast_EMA), df) > get(paste0("EMA", "_", slow_EMA), df)] <- 1
    
    print(tail(df, 1))
    Sys.sleep(interval*60)
  }
}


# Strategy 2 -------------------------------------------------------------------
# Crossing fast ema to slower with stoploss condition

cross_EMA_stoploss_trail <- function(fast_EMA, slow_EMA,takeprofit, stoploss_trail,stoploss_ult) {
  
  # Train and test datasets$
  train_data[, c(paste0("EMA", "_", fast_EMA),
                 paste0("EMA", "_", slow_EMA),
                 "exit_value","exit_condition",
                 "crossover",
                 "crossover_Volume",
                 "action",
                 "tp",
                 "ult_sl",
                 "trail_sl",
                 "Units",
                 "Price",
                 "id") := list(NA, NA,NA,NA,NA, NA, NA, NA, NA, NA, NA, NA, NA) ]
  
  test_data[, c(paste0("EMA", "_", fast_EMA),
                paste0("EMA", "_", slow_EMA),
                "exit_value",
                "exit_condition",
                "crossover",
                "crossover_Volume",
                "action",
                "tp",
                "ult_sl",
                "trail_sl",
                "Units",
                "Price",
                "id") := list(NA, NA, NA,NA,NA,NA, NA, NA, NA ,NA, NA, NA, NA) ]
  
  # Loop to evaluate last line of the test dataset
  for (i in 1:nrow(test_data)) {
    
    # Bind last row
    fut <- rbind(train_data, test_data[i, ])
    
    # Create indicators
    fut[, c(paste0("EMA", "_", fast_EMA),
            paste0("EMA", "_", slow_EMA)) := list(EMA(close, n = fast_EMA),
                                                  EMA(close, n = slow_EMA)) ]
    
    
    fut$crossover[get(paste0("EMA", "_", fast_EMA), fut) > get(paste0("EMA", "_", slow_EMA), fut)] <- "faster_EMA_higher"
    fut$crossover[get(paste0("EMA", "_", fast_EMA), fut) <= get(paste0("EMA", "_", slow_EMA), fut)] <- "faster_EMA_lower"
    
    # Exit condition for takeprofit  - Fixed
    tp <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) + takeprofit * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)
    
    
    if (length(tp) == 0) {
      tp <- 0
    }
    
    # Ultimate stop loss
    ult_sl <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) - stoploss_ult * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)
    
    if (length(ult_sl) == 0) {
      ult_sl <- 0
    }
    
    
    # Trailing stop loss
    
    # id_trail <- tail(fut$id[!is.na(fut$id)], 1)
    # 
    # if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){
    #   
    #   trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
    #   
    #   if(trail_sl > max(fut$trail_sl[which(fut$id == id_trail)+1], na.rm =T)){
    #     trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
    #     
    #   }else{
    #     
    #     trail_sl <- max(fut$trail_sl[which(fut$id == id_trail)+1])
    #   }
    #   
    #   
    # 
    # } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1]) ){
    #   
    #   trail_sl <- max(fut$trail_sl[which(fut$id == id_trail)])
    # 
    # } else {
    #   
    #   trail_sl <-0 
    # } 
    # 
    
    # if(length(trail_sl) == 0 ){
    #   
    #   trail_sl <- 0
    # }
    
    # Trailing stop loss
    if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){
      
      trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      if( trail_sl < tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)){
        trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
        
      }else {
        trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
        
      }
      
      
    } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1] ) ){
      
      trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
    } else {
      
      trail_sl <-0
    }
    
    
    if(length(trail_sl) == 0 ){
      
      trail_sl <- 0
    }
    
    
    
    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl
    fut$trail_sl[nrow(fut)] <- trail_sl
    
    
    fut$exit_condition[nrow(fut)] <- fut$trail_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]
    
    
    # BUY Condition
    if ((is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action"))
        & fut$crossover[nrow(fut)] == "faster_EMA_higher") {
      
      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)]*fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
      
      # Keep Condition  
    } else if (fut$action[nrow(fut) - 1] %in% c("buy", "keep") &
               fut$exit_condition[nrow(fut)] == FALSE ) {
      
      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      
      # Sell Condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") &
               fut$exit_condition[nrow(fut)] == TRUE) {
      
      
      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]
      
      # No Action Condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("sell", "no action") | (is.na(fut$action[nrow(fut) - 1])
                                                                         & fut$crossover[nrow(fut)] == "faster_EMA_lower")) {
      
      fut$action[nrow(fut)] <- "no action"
    }
    
    train_data <- fut
  }
  return(train_data)
}


cross_EMA_stoploss_trail_simple <- function(slow_EMA,takeprofit, stoploss_trail,stoploss_ult) {
  
  # Train and test datasets$
  train_data[, c("EMA",
                 "exit_value","exit_condition",
                 "crossover",
                 "crossover_Volume",
                 "action",
                 "tp",
                 "ult_sl",
                 "trail_sl",
                 "Units",
                 "Price",
                 "id") := list(NA,NA,NA,NA, NA, NA, NA, NA, NA, NA, NA, NA) ]
  
  test_data[, c("EMA",
                "exit_value",
                "exit_condition",
                "crossover",
                "crossover_Volume",
                "action",
                "tp",
                "ult_sl",
                "trail_sl",
                "Units",
                "Price",
                "id") := list(NA, NA,NA,NA,NA, NA, NA, NA ,NA, NA, NA, NA) ]
  
  # Loop to evaluate last line of the test dataset
  for (i in 1:nrow(test_data)) {
    
    # Bind last row
    fut <- rbind(train_data, test_data[i, ])
    
    # Create indicators
    fut$EMA <- EMA(fut$close, n = slow_EMA)
    
    
    fut$crossover[nrow(fut)] <- ifelse(fut$close[nrow(fut)] > fut$EMA[nrow(fut)]  ,
                                              "price_higher", "price_lower")
    
    # Exit condition for takeprofit  - Fixed
    tp <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) + takeprofit * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)
    
    
    if (length(tp) == 0) {
      tp <- 0
    }
    
    # Ultimate stop loss
    ult_sl <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) - stoploss_ult * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)
    
    if (length(ult_sl) == 0) {
      ult_sl <- 0
    }
    
    
   
    
    # Trailing stop loss
    if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){
      
      trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      if( trail_sl < tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)){
        trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
        
      }else {
        trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
        
      }
      
      
    } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1] ) ){
      
      trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
    } else {
      
      trail_sl <-0
    }
    
    
    if(length(trail_sl) == 0 ){
      
      trail_sl <- 0
    }
    
    
    
    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl
    fut$trail_sl[nrow(fut)] <- trail_sl
    
    
    fut$exit_condition[nrow(fut)] <- fut$trail_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]
    
    
    # BUY Condition
    if ((is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action"))
        & fut$crossover[nrow(fut)] == "price_higher") {
      
      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)]*fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
      
      # Keep Condition  
    } else if (fut$action[nrow(fut) - 1] %in% c("buy", "keep") &
               fut$exit_condition[nrow(fut)] == FALSE ) {
      
      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      
      # Sell Condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") &
               fut$exit_condition[nrow(fut)] == TRUE) {
      
      
      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]
      
      # No Action Condition
    } else  {
      
      fut$action[nrow(fut)] <- "no action"
    }
    
    train_data <- fut
  }
  return(train_data)
}


portfolio_rsi_reversal <- function(ATR_period, RSI_period, roll_max_period, roll_min_period ,takeprofit, stoploss_trail,stoploss_ult) {
  
  # Train and test datasets
  train_data[, c("ATR",
                 "RSI",
                 "Roll_Max",
                 "Roll_Min",
                 "M_minus",
                 "Signal",
                 "exit_condition",
                 "action",
                 "Units",
                 "Price",
                 "tp",
                 "ult_sl",
                 "trail_sl",
                 "id") := list(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]
  
  test_data[, c("ATR",
                "RSI",
                "Roll_Max",
                "Roll_Min",
                "M_minus",
                "Signal",
                "exit_condition",
                "action",
                "Units",
                "Price",
                "tp",
                "ult_sl",
                "trail_sl",
                "id") := list(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]
  
  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){
    
    fut <- rbind(train_data, test_data[i, ])
    
    # Technical indicators -----------------------------------------------------
    fut$ATR <- ATR(fut[,c("High","Low","Close")], n = ATR_period)[,2]
    fut$RSI <- RSI(fut$Close, n = RSI_period)
    fut$Roll_Max <- runMax(fut$Close, n = roll_max_period, cumulative = FALSE)
    fut$Roll_Min <- runMin(fut$Close, n = roll_min_period, cumulative = FALSE)
    
    fut$M_minus <- (fut$Close - fut$Roll_Min)/fut$ATR
    fut$Signal <- ifelse(fut$M_minus <= 2 & fut$RSI > 30, "long", "nothing")
    
    # Exit condition for takeprofit  - Fixed
    tp <- tail(fut$Close[fut$action == "buy"][!is.na(fut$Close[fut$action == "buy"])], 1) + takeprofit * tail(fut$Close[fut$action == "buy"][!is.na(fut$Close[fut$action == "buy"])], 1)
    
    if (length(tp) == 0) {
      tp <- 0
    }
    
    # Ultimate stop loss
    ult_sl <- tail(fut$Close[fut$action == "buy"][!is.na(fut$Close[fut$action == "buy"])], 1) - stoploss_ult * tail(fut$Close[fut$action == "buy"][!is.na(fut$Close[fut$action == "buy"])], 1)
    
    if (length(ult_sl) == 0) {
      ult_sl <- 0
    }
    
    
    # Trailing stop loss
    # browser()
    if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$Close[nrow(fut)] > fut$Close[nrow(fut)-1] )  ){
      
      trail_sl <- fut$Close[nrow(fut)] - stoploss_trail * fut$Close[nrow(fut)]
      if( trail_sl < tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)){
        trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
        
      }else {
        trail_sl <- fut$Close[nrow(fut)] - stoploss_trail * fut$Close[nrow(fut)]
        
      }
      
      
    } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$Close[nrow(fut)] <= fut$Close[nrow(fut)-1] ) ){
      
      trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
    } else {
      
      trail_sl <-0
    }
    
    
    if(length(trail_sl) == 0 ){
      
      trail_sl <- 0
    }
    # browser()
    
    
    
    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl
    fut$trail_sl[nrow(fut)] <- trail_sl
    
    
    fut$exit_condition[nrow(fut)] <- fut$trail_sl[nrow(fut)] > fut$Close[nrow(fut)] | fut$ult_sl[nrow(fut)] > fut$Close[nrow(fut)] | fut$tp[nrow(fut)] < fut$Close[nrow(fut)]
    
    
    # Deciding upon action -----------------------------------------------------
    
    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         fut$Signal[nrow(fut)] == "long") {
      
      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$Close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$Close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
      
      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") &
               (fut$exit_condition[nrow(fut)] == TRUE  | fut$RSI[nrow(fut)] > 70 | fut$RSI[nrow(fut)] < 15)   ) {
      
      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$Close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]
      
      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   & 
                fut$exit_condition[nrow(fut)] == FALSE  ) {
      
      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      
    } else {
      
      fut$action[nrow(fut)] <- "no action"
      
    }
    
    train_data <- fut
    print(i)
  }
  return(train_data)
}
