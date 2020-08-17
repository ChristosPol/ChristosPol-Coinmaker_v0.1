# Best cross EMA so far
# 40_110_200_0.03_0.05
# 675.91
# 102
# 2020-01-10	2020-06-19	40_110_200_0.03_0.05
# # Badget
initial_budget <- 500

# # select period of data 
candles_recent <- candles
# colnames(candles_recent) <- c("Date", "interval", "High", "Low", "Open", "Close", "Volume"  )

# # Plot if like 
# plot_candlesticks(dta = candles_recent, Ns = nrow(candles_recent), asset = pair)
# plot_candlesticks(dta = candles_recent, Ns = 100, asset = pair)
# 
# # N Training Data
train_n <- ceiling(nrow(candles_recent) / 17)
train_data <- candles_recent[1:train_n, ]
# 
# # Test, same 
test_data <- candles_recent[(train_n + 1):nrow(candles_recent), ]

library(foreach)

# testing parameters
roll <- data.frame(roll = c(20, 50, 100, 150, 200), flag = 1)
n_sort <- data.frame(n_sort = c(3, 6, 10, 20), flag = 1)
takeprofit <- data.frame(takeprofit = c(0.05, 0.08, 0.1), flag = 1)
stoploss_trail <- data.frame(stoploss_trail = c(0.01, 0.02), flag = 1)
stoploss_ult <- data.frame(stoploss_ult = c(0.01, 0.02), flag = 1)

#
testing_params <- left_join(roll, n_sort) %>%
  left_join(takeprofit)%>% left_join(stoploss_trail)%>% left_join(stoploss_ult)
testing_params$flag <- NULL

library("doParallel")
library("foreach")
cl <- parallel::makeForkCluster(3)
doParallel::registerDoParallel(cl)
start_time <- Sys.time()
results <- foreach(i = 1:nrow(testing_params), .combine = 'rbind') %dopar% {
  myresult <- Dynamic_SR_Lines(roll = testing_params$roll[i],
                               n_sort = testing_params$n_sort[i],
                               takeprofit = testing_params$takeprofit[i],
                               stoploss_trail = testing_params$stoploss_trail[i],
                               stoploss_ult = testing_params$stoploss_ult[i])
  # er <- tryCatch(
  #    {
  res <- calculate_profits(myresult)
  res$params <- paste(roll = testing_params$roll[i],
                      n_sort = testing_params$n_sort[i],
                      takeprofit = testing_params$takeprofit[i],
                      stoploss_trail = testing_params$stoploss_trail[i],
                      stoploss_ult = testing_params$stoploss_ult[i],
                      sep ="_")
  # 
  gc()
  # error = function(e){})
  # if(length(er) == 0){
  # res <- data.frame(profit = 0, n_trades = 0, params = "0", enter_date = as.Date("2020-04-07"), exit_date =as.Date("2020-04-07"))

res
  }
end_time <- Sys.time()
end_time - start_time
parallel::stopCluster(cl)
View(results)
saveRDS(results, file ="/media/chris/DATA/Documents/Bot_Trading/Historical_data/Backtesting/Pure_RSI_Trailing_parameters_18june.rds")
