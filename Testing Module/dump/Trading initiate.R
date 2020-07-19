# screen -S Live_test_15MIN_20_42 R
rm(list=ls())
source(paste("/media/chris/DATA/Documents/Bot_Trading", "10 Utils.R", sep = "/"))


fast_EMA <- 13
slow_EMA <- 34
interval <- 15
Volume_EMA <-10
options(digits = 5)

# Choose pair
pair <- "XETHZEUR"


da <- data.frame()
d <- data.frame() 
initial_budget <- 500 
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
    df[, c(paste0("EMA", "_", fast_EMA), paste0("EMA", "_", slow_EMA), paste0("Volume_EMA", "_", Volume_EMA),
           "crossover","crossover_Volume", "action", "Units", "Price", "id") := list(NA, NA, NA, NA, NA ,NA, NA, NA, NA) ]
    
    df[, c(paste0("EMA", "_", fast_EMA), paste0("EMA", "_", slow_EMA),
           paste0("Volume_EMA", "_", Volume_EMA)) := list( EMA(close, n = fast_EMA), EMA(close, n = slow_EMA),EMA(volume, n = Volume_EMA) ) ]
    
    # EMAs Flags
    df$crossover[get(paste0("EMA", "_", fast_EMA), df) < get(paste0("EMA", "_", slow_EMA), df)] <- "faster_EMA_lower"
    df$crossover[get(paste0("EMA", "_", fast_EMA), df) >= get(paste0("EMA", "_", slow_EMA), df)] <- "faster_EMA_higher"
    
    df$crossover_Volume[get(paste0("Volume_EMA", "_", Volume_EMA), df) < df$volume] <- "volume_higher" 
    df$crossover_Volume[get(paste0("Volume_EMA", "_", Volume_EMA), df) >= df$volume] <- "volume_lower" 
    
    da <- rbind(d, tail(df, 1))
    
    if (da$crossover[nrow(da)] == "faster_EMA_lower" & length(da$action[nrow(da) - 1] ) == 0  & da$crossover_Volume[nrow(da)] == "volume_higher"){
      
      da$action[nrow(da)] <- "buy"
      da$Units[nrow(da)] <- initial_budget / da$close[nrow(da)]
      da$Price[nrow(da)] <- da$Units[nrow(da)]*da$close[nrow(da)]
      da$id[nrow(da)] <- round(runif(1, 10000, 5000000))
  
      
    } else if( (da$crossover[nrow(da)] %in% c("faster_EMA_higher", "faster_EMA_lower") | da$crossover_Volume[nrow(da)] == "volume_lower") & length(da$action[nrow(da) - 1]) == 0 ){
      da$action[nrow(da)] <- "no action"
      
    } else if (da$crossover[nrow(da)] == "faster_EMA_lower" &  da$crossover_Volume[nrow(da)] == "volume_higher" &  (da$action[nrow(da) - 1] %in% c("no action") | is.na(da$action[nrow(da) - 1]) ) ){
      
      da$action[nrow(da)] <- "buy"
      da$Units[nrow(da)] <- initial_budget / da$close[nrow(da)]
      da$Price[nrow(da)] <- da$Units[nrow(da)]*da$close[nrow(da)]
      da$id[nrow(da)] <- round(runif(1, 10000, 5000000))
      
      
    } else if (da$action[nrow(da) - 1] %in% c("buy", "keep") & (da$crossover[nrow(da)] %in% c("faster_EMA_higher", "faster_EMA_lower") |  da$crossover_Volume[nrow(da)] == "volume_lower")) {
      
      da$action[nrow(da)] <- "keep"
      da$Units[nrow(da)] <- da$Units[nrow(da) -1 ]
      da$id[nrow(da)] <- da$id[nrow(da)-1]
      
    } else if (da$action[nrow(da) - 1] %in% c("keep") & da$crossover[nrow(da)] == "faster_EMA_higher" & da$crossover_Volume[nrow(da)] == "volume_higher" ) {
      
      da$action[nrow(da)] <- "sell"
      da$Units[nrow(da)] <- da$Units[nrow(da) -1]
      da$Price[nrow(da)] <- da$close[nrow(da)]* da$Units[nrow(da)]
      da$id[nrow(da)] <- da$id[nrow(da)-1]
      initial_budget <- da$Price[nrow(da)]
      
    } else if (da$action[nrow(da) - 1] %in% c("buy") &  da$crossover[nrow(da)] == "faster_EMA_higher" & da$crossover_Volume[nrow(da)] == "volume_higher"){
      
      da$action[nrow(da)] <- "sell"
      da$Units[nrow(da)] <- da$Units[nrow(da) -1]
      da$Price[nrow(da)] <- da$close[nrow(da)]* da$Units[nrow(da)]
      da$id[nrow(da)] <- da$id[nrow(da)-1]
      initial_budget <- da$Price[nrow(da)]
    }
  print(da)
    
    
    Sys.sleep(5)
}
     