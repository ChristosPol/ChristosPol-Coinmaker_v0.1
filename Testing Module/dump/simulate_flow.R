  # Train and test datasets
  fast_EMA <- 15
  slow_EMA <- 65
  n_mean_sd <- 20
  times_sd <- 1
  
  takeprofit <- 0.03
  stoploss <- 0.01
  i <- 1
  train_set <- subset(frame1, frame1$Date < "2020-05-01") 
  test_set <- subset(frame1, frame1$Date >= "2020-05-01")
  cond_row <- data.frame()
  d <- data.frame()
  
  for (i in 1:nrow(test_set)){
    # Sys.sleep(0.5)
    d <- cond_row
    idx <- 1:i
    tmp <- rbind(train_set, test_set[idx, ])
    tmp1 <- tmp[, .(high = max(price), low = min(price), open = first(price),
                    close = last(price), volume = sum(volume)),
                by = .(Date, interval)]
    # Create indicators
    empty_frame <- data.table("EMA_fast" = EMA(tmp1$close, n = fast_EMA),
                              "EMA_slow" = EMA(tmp1$close, n = slow_EMA), 
                              mean_volume = 0,
                              sd = 0,
                              "crossover" = 0,
                              "crossover_Volume" = 0,
                              "action" = NA,
                              "exit_value" = 0,
                              "exit_condition"= NA,
                              "Units" = 0,
                              "Price" = 0,
                              "id" = NA)
  
    empty_frame$mean_volume[nrow(empty_frame)] <- mean(tail(tmp1$volume, n_mean_sd))
    empty_frame$sd[nrow(empty_frame)] <- sd(tail(tmp1$volume, n_mean_sd))
    
    empty_frame$crossover[empty_frame$EMA_fast < empty_frame$EMA_slow] <- "faster_EMA_lower"
    empty_frame$crossover[empty_frame$EMA_fast >= empty_frame$EMA_slow] <- "faster_EMA_higher"
    
    # Volume Crossing of volume over the mean(volume, n_periods) + times * sd(volume, n_periods)
    empty_frame$crossover_Volume[nrow(empty_frame)] <- ifelse(tmp1$volume[nrow(tmp1)] > (empty_frame$mean_volume[nrow(empty_frame)] +  times_sd * empty_frame$sd[nrow(empty_frame)]),"volume_higher", "volume_lower")
    empty_frame$close <- tmp1$close
    empty_frame$Date <- tmp1$Date
    empty_frame$interval <- tmp1$interval
    empty_frame$volume <- tmp1$volume
    
    
    
    
    cond_row <- rbind(d, tail(empty_frame, 1))
    
    exit_value <- (cond_row$close[nrow(cond_row)] - tail(cond_row$close[!is.na(cond_row$action) &cond_row$action =="buy"], 1)) / tail(cond_row$close[!is.na(cond_row$action) & cond_row$action =="buy"], 1)
    
    if (length(exit_value) == 0) {
      exit_value <- 0
    }
    
    cond_row$exit_value[nrow(cond_row)] <- exit_value
    cond_row$exit_condition[nrow(cond_row)] <- cond_row$exit_value[nrow(cond_row)] > takeprofit | cond_row$exit_value[nrow(cond_row)] <= -stoploss
    
    
    if(nrow(cond_row) > 1) {
    # Buy Condition   
    if ((is.na(cond_row$action[nrow(cond_row) - 1]) |  cond_row$action[nrow(cond_row) - 1] %in% c("sell", "no action")) &
        (cond_row$crossover[nrow(cond_row)] =="faster_EMA_lower"  & 
         cond_row$crossover_Volume[nrow(cond_row)] == "volume_higher") ){
      
      cond_row$action[nrow(cond_row)] <- "buy"
      cond_row$Units[nrow(cond_row)] <- initial_budget / cond_row$close[nrow(cond_row)]
      cond_row$Price[nrow(cond_row)] <- cond_row$Units[nrow(cond_row)] * cond_row$close[nrow(cond_row)]
      cond_row$id[nrow(cond_row)] <- round(runif(1, 10000, 5000000))
      
      # Keep Condition
    } else if ( (cond_row$action[nrow(cond_row) - 1] %in% c("buy", "keep")) &
                (cond_row$exit_condition[nrow(cond_row)] == FALSE) ) {
      
      cond_row$action[nrow(cond_row)] <- "keep"
      cond_row$Units[nrow(cond_row)] <- cond_row$Units[nrow(cond_row) -1 ]
      cond_row$id[nrow(cond_row)] <- cond_row$id[nrow(cond_row)-1]
      
      # Sell Condition
    } else if (cond_row$action[nrow(cond_row) - 1] %in% c("keep", "buy") &  cond_row$exit_condition[nrow(cond_row)] == TRUE) {
      
      cond_row$action[nrow(cond_row)] <- "sell"
      cond_row$Units[nrow(cond_row)] <- cond_row$Units[nrow(cond_row) -1]
      cond_row$Price[nrow(cond_row)] <- cond_row$close[nrow(cond_row)]* cond_row$Units[nrow(cond_row)]
      cond_row$id[nrow(cond_row)] <- cond_row$id[nrow(cond_row)-1]
      initial_budget <- cond_row$Price[nrow(cond_row)]
      
      # No Action Condition
    } else{
      
      cond_row$action[nrow(cond_row)] <- "no action"
      
      } 
    }else {
      cond_row$action[nrow(cond_row)] <- "no action"
    
      }
    print(cond_row)
     
  }
  View(cond_row)
