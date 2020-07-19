      # Start ------------------------------------------------------------------------
      # Live trading module
      rm(list=ls())
      # screen -S Live_Trading R
      
      source(paste("/media/chris/DATA/Documents/Bot_Trading/Coinmaker", "10 Utils.R", sep = "/"))
      
      
      # Set parameters
      fast_EMA <- 15
      slow_EMA <- 65
      interval <- 15
      n_mean_sd <- 20
      times_sd <- 0
      options(digits = 5)
      takeprofit <- 0.02
      stoploss <- 0.01
      # API info
      api_info <- read.table(paste("/media/chris/DATA/Documents/Bot_Trading", "API_Keys.txt", sep = "/"), sep = ";", header = T)
      API_Key <- as.character(api_info$API_Key)
      API_Sign <- as.character(api_info$API_Sign)
      
      url      <- "https://api.kraken.com/0/private/Balance"
      
      # Choose pair
      pair <- "XETHZEUR"
      
      da <- data.frame()
      d <- data.frame() 
      
      
      repeat {
        
        d <- da  
        # print(da)
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
        df$volume <- as.numeric(df$volume)
        
        # 2. Add Indicators
        df[, c("mean_volume",
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
        
        df[, c(paste0("EMA", "_", fast_EMA),
               paste0("EMA", "_", slow_EMA)) := list( EMA(close, n = fast_EMA),
                                                               EMA(close, n = slow_EMA) ) ]
        
        
        # Mean and sd of volume in train set
        df$mean_volume[nrow(df)] <- mean(tail(df$volume, n_mean_sd))
        df$sd[nrow(df)] <- sd(tail(df$volume), n_mean_sd)
        
        # EMAs Flags
        df$crossover[get(paste0("EMA", "_", fast_EMA), df) < get(paste0("EMA", "_", slow_EMA), df)] <- "faster_EMA_lower"
        df$crossover[get(paste0("EMA", "_", fast_EMA), df) >= get(paste0("EMA", "_", slow_EMA), df)] <- "faster_EMA_higher"
        
        # Volume Crossing of volume over the mean(volume, n_periods) + times * sd(volume, n_periods)
        df$crossover_Volume[df$volume > (df$mean_volume +  times_sd * df$sd)  ] <- "volume_higher"
        df$crossover_Volume[df$volume <= (df$mean_volume +  times_sd * df$sd)  ] <- "volume_lower"
        
        da <- rbind(d, tail(df, 1))
        
        # Exit condition for stop loss 
        exit_value <- (da$close[nrow(da)] - tail(da$close[!is.na(da$action) & da$action =="buy"], 1)) / tail(da$close[!is.na(da$action) & da$action =="buy"], 1)
        
        if (length(exit_value) == 0) {
          exit_value <- 0
        }
        
        da$exit_value[nrow(da)] <- exit_value
        da$exit_condition <- da$exit_value > takeprofit | da$exit_value <= -stoploss
        
        if (nrow(da) > 1) {
          
          # BUY Condition ------------------------------------------------------------
          if ((is.na(da$action[nrow(da) - 1]) |  da$action[nrow(da) - 1] %in% c("sell", "no action")) &
              da$crossover[nrow(da)] == "faster_EMA_lower"  & da$crossover_Volume[nrow(da)] == "volume_higher"){
             
            # get initial balance in EUR
            # initial_budget <- 500
            init_balance <- get_balance(url = "https://api.kraken.com/0/private/Balance",
                                        key = API_Key, secret = API_Sign)
            initial_budget <- as.numeric(init_balance$result$ZEUR)
            initial_budget <- initial_budget - 10
            # Give API Order to buy at market
            buy_it <- add_market_order(url = "https://api.kraken.com/0/private/AddOrder",
                                       key = API_Key, secret = API_Sign, pair = pair, type = "buy",
                                       ordertype = "market", volume = initial_budget / da$close[nrow(da)])

            print(buy_it)
            da$action[nrow(da)] <- "buy"
            da$Units[nrow(da)] <- initial_budget / da$close[nrow(da)]
            da$Price[nrow(da)] <- da$Units[nrow(da)]*da$close[nrow(da)]
            da$id[nrow(da)] <- buy_it$result$txid
          
            
            # KEEP condition
          } else if ( da$action[nrow(da) - 1] %in% c("buy", "keep") &   da$exit_condition[nrow(da)] == FALSE) {
            
            da$action[nrow(da)] <- "keep"
            da$Units[nrow(da)] <- da$Units[nrow(da) -1 ]
            da$id[nrow(da)] <- da$id[nrow(da)-1]
           
            # SELL condition 
          } else if (da$action[nrow(da) - 1] %in% c("keep", "buy") &   da$exit_condition[nrow(da)] == TRUE) {
            
            
            crypto_hold <- get_balance(url = "https://api.kraken.com/0/private/Balance",
                                        key = API_Key, secret = API_Sign)
            crypto_hold_eth <- as.numeric(crypto_hold$result$XETH)
            
            # Give API Order to buy at market
            sell_it <- add_market_order(url = "https://api.kraken.com/0/private/AddOrder",
                                        key = API_Key, secret = API_Sign, pair = pair, type = "sell",
                                        ordertype = "market", volume = crypto_hold_eth)

            print(sell_it)
            da$action[nrow(da)] <- "sell"
            da$Units[nrow(da)] <- da$Units[nrow(da) -1]
            da$Price[nrow(da)] <- da$close[nrow(da)]* da$Units[nrow(da)]
            da$id[nrow(da)] <- sell_it$result$txid
            
          } else {
            
            da$action[nrow(da)] <- "no action"
            
            
          }
          
          print(da)
          Sys.sleep(5)
          
          } else {
        
          da$action[nrow(da)] <- "no action"
        
        }
      }
      
