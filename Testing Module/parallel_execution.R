library(foreach)
library(doParallel)

# # Badget
paraller_exec <-TRUE
initial_budget <- 200

# testing parameters
spar_fast <- data.frame(spar_fast = seq(0.1, 0.9, 0.1), flag = 1)
spar_slow <- data.frame(spar_slow = seq(0.1, 1, 0.1), flag = 1)
tp <- data.frame(tp = c(0.01, 0.02, 0.03, 1000), flag = 1)
sl <- data.frame(sl = c(0.01, 0.02,  0.03, 1000), flag = 1)

testing_params <- left_join(spar_fast, spar_slow) %>% left_join(tp)%>% left_join(sl)

testing_params <- subset(testing_params, testing_params$spar_slow > testing_params$spar_fast)
testing_params$flag <- NULL
testing_params <- as.data.table(testing_params)

# testing_params <- testing_params[1:25,]
all_results <- list()
start_time <- Sys.time()
for (i in 1:length(klines)){
  
  # out_of_sample <- subset(klines[[i]], klines[[i]]$Date >= "2020-08-01")
  
  # select period of data 
  candles_recent <- klines[[i]]
  
  # Intitial data frame
  train_n <- ceiling(nrow(candles_recent) / 8)
  train_data <- candles_recent[1:train_n, ]
  
  # Test, same 
  test_data <- candles_recent[(train_n + 1):nrow(candles_recent), ]
  
  cl <- parallel::makeForkCluster(2)
  doParallel::registerDoParallel(cl)
  start_time <- Sys.time()
  results <- foreach(i = 1:nrow(testing_params),.verbose =T , .combine = 'rbind') %dopar% {
    myresult <- splines_fast_slow_cross(takeprofit = testing_params$tp[i],
                                        stoploss_ult = testing_params$sl[i],
                                        spar_fast = testing_params$spar_fast[i],
                                        spar_slow = testing_params$spar_slow[i],
                                        plot.it = FALSE)
    
    params <- paste(spar_fast = testing_params$spar_fast[i],
                    spar_slow = testing_params$spar_slow[i],
                    takeprofit = testing_params$tp[i],
                    stoploss_ult = testing_params$sl[i],
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
end_time <- Sys.time()
print(end_time - start_time)

filelist <- mapply(cbind, all_results, "Interval" = names(klines), SIMPLIFY = F)
final <- do.call(rbind, filelist)
final$pair <- pair
View(final)

saveRDS(final, file ="/media/chris/DATA/Documents/Bot_Trading/Backtesting_results/latest_tp_sl_fast_slow_splines_cross_60min_201906_ETHEUR.rds")
View(final)
