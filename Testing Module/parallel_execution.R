# Best cross EMA so far
# 40_110_200_0.03_0.05
# 675.91
# 102
# 2020-01-10	2020-06-19	40_110_200_0.03_0.05
# # Badget
paraller_exec <-TRUE
initial_budget <- 500

# # select period of data 
candles_recent <- candles
# colnames(candles_recent) <- c("Date", "interval", "High", "Low", "Open", "Close", "Volume"  )

# # Plot if like 
# plot_candlesticks(dta = candles_recent, Ns = nrow(candles_recent), asset = pair)
# plot_candlesticks(dta = candles_recent, Ns = 100, asset = pair)
# 
# # N Training Data
train_n <- ceiling(nrow(candles_recent) / 50)
train_data <- candles_recent[1:train_n, ]
# 
# # Test, same 
test_data <- candles_recent[(train_n + 1):nrow(candles_recent), ]

library(foreach)

# testing parameters
RSI_Period <- data.frame(RSI_Period = c(5, 10, 14), flag = 1)
EMA_volume <- data.frame(EMA_volume = c(10), flag = 1)
RSI_below <- data.frame(RSI_below = c(40, 45), flag = 1)
times_vol <- data.frame(times_vol = c(1), flag = 1)
takeprofit <- data.frame(takeprofit = c(0.01, 0.015, 0.02, 0.025, 0.03), flag = 1)
stoploss_trail <- data.frame(stoploss_trail = c(0.02, 0.03, 0.05, 0.07,1), flag = 1)
stoploss_ult <- data.frame(stoploss_ult = c(1), flag = 1)


testing_params <- left_join(RSI_Period, EMA_volume)%>%
  left_join(RSI_below)%>% left_join(times_vol)%>% left_join(takeprofit)%>%
  left_join(stoploss_trail)%>%left_join(stoploss_ult)
testing_params$flag <- NULL
testing_params <- as.data.table(testing_params)

library("doParallel")
library("foreach")
cl <- parallel::makeForkCluster(3)
doParallel::registerDoParallel(cl)
start_time <- Sys.time()
results <- foreach(i = 1:nrow(testing_params), .combine = 'rbind') %dopar% {
  myresult <- Pure_RSI_Volume_Trailing(RSI_Period = testing_params$RSI_Period[i],
                                       EMA_volume = testing_params$EMA_volume[i],
                                       RSI_below = testing_params$RSI_below[i],
                                       times_vol = testing_params$times_vol[i],
                                       takeprofit = testing_params$takeprofit[i],
                                       stoploss_trail = testing_params$stoploss_trail[i],
                                       stoploss_ult = testing_params$stoploss_ult[i],
                                        plot.it = FALSE)
  # er <- tryCatch(
  #    {
  params <- paste(RSI_Period = testing_params$RSI_Period[i],
                  EMA_volume = testing_params$EMA_volume[i],
                  RSI_below = testing_params$RSI_below[i],
                  times_vol = testing_params$times_vol[i],
                  takeprofit = testing_params$takeprofit[i],
                  stoploss_trail = testing_params$stoploss_trail[i],
                  stoploss_ult = testing_params$stoploss_ult[i],
                  sep ="_")
  res <- calculate_profits(myresult, params = params)
  
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
saveRDS(results, file ="/media/chris/DATA/Documents/Bot_Trading/Historical_data/Backtesting/Pure_RSI_traiing_ETH_1hour.rds")
