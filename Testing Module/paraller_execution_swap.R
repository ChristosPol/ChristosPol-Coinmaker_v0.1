library(foreach)
library(doParallel)

# # Badget
paraller_exec <-TRUE
initial_budget <- 200


RSI_Period <- data.frame(RSI_Period = c(5, 10, 15), flag = 1)
RSI_lower <- data.frame(RSI_lower = c(20, 30, 40, 50), flag = 1)
RSI_upper <- data.frame(RSI_upper = c(60, 70, 80, 90), flag = 1)
spar <- data.frame(spar = c(0.5, 0.6, 0.7, 0.8, 0.9), flag = 1)
stoploss_ult <- data.frame(stoploss_ult = c(0.01, 0.02), flag = 1)
plot.it <- T


testing_params <- left_join(RSI_Period, RSI_lower) %>% left_join(RSI_upper) %>%
  left_join(spar) %>% left_join(stoploss_ult)

testing_params$flag <- NULL
testing_params <- as.data.table(testing_params)
all_results <- list()
for (i in 1:length(klines)){
  
  # out_of_sample <- subset(klines[[i]], klines[[i]]$Date >= "2020-08-01")
  
  # select period of data 
  candles_recent <- klines[[i]]
  
  
  # Intitial data frame
  train_n <- ceiling(nrow(candles_recent) / 3)
  train_data <- candles_recent[1:train_n, ]
  
  # Test, same 
  test_data <- candles_recent[(train_n + 1):nrow(candles_recent), ]
  
  cl <- parallel::makeForkCluster(3)
  doParallel::registerDoParallel(cl)
  start_time <- Sys.time()
  results <- foreach(i = 1:nrow(testing_params), .combine = 'rbind') %dopar% {
    myresult <- RSI_splines(RSI_Period = testing_params$RSI_Period[i],
                                         RSI_lower = testing_params$RSI_lower[i],
                                         RSI_upper = testing_params$RSI_upper[i],
                                         spar = testing_params$spar[i],
                                         stoploss_ult = testing_params$stoploss_ult[i],
                                        plot.it = FALSE)
    # Close last position
    if(myresult$action[nrow(myresult)] == "keep") {
      myresult$action[nrow(myresult)] <- "sell"
      myresult$Price[nrow(myresult)] <- myresult$close[nrow(myresult)] * myresult$Units[nrow(myresult)]
    }
    
    params <- paste(RSI_Period = testing_params$RSI_Period[i],
                    RSI_lower = testing_params$RSI_lower[i],
                    RSI_upper = testing_params$RSI_upper[i],
                    spar = testing_params$spar[i],
                    stoploss_ult = testing_params$stoploss_ult[i],
                    sep ="_")
    res <- calculate_profits(myresult, params = params)
    
    gc()
    res
  }
  end_time <- Sys.time()
  end_time - start_time
  parallel::stopCluster(cl)
  all_results[[i]] <- results 
}
filelist <- mapply(cbind, all_results, "Interval"=names(klines), SIMPLIFY=F)
final <- do.call(rbind, filelist)
final$pair <- pair


saveRDS(final, file ="/media/chris/DATA/Documents/Bot_Trading/Backtesting_results/latest_tp_sl_fast_slow_splines_cross_60min_201906_ETHEUR.rds")
View(final)
