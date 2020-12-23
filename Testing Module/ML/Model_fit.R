# Idea is to build a robust model selecting the best parameters
# Use this model to predict t+1 of the time series

# Subsetting dates from loading data
from_date <- "2018-01-01"
to_date <- "2020-12-17"
# Train model from: 2018-01-01 00:00:00 to:  2020-10-23 05:00:00
# Test model from 2020-11-07 13:00:00 to: 2020-12-16 23:00:00

# Dataset
df <- klines[[1]]

# Define predictors

# EMAs
ema1_n <- data.frame(ema1_n = c(7), flag = 1)#, 10, 12, 15, 20), flag = 1)
ema2_n <- data.frame(ema2_n = c(14), flag = 1) #18, 25, 30), flag = 1)
ema3_n <- data.frame(ema3_n = c(21), flag = 1) #28, 35, 45, 60), flag = 1)
ema4_n <- data.frame(ema4_n = c(100), flag = 1) #100, 120, 150), flag = 1)

testing_params <- left_join(ema1_n, ema2_n) %>%
  left_join(ema3_n) %>% left_join(ema4_n)
testing_params <- subset(testing_params,
                         testing_params$ema1_n < testing_params$ema2_n & 
                         testing_params$ema2_n < testing_params$ema3_n &
                         testing_params$ema3_n < testing_params$ema4_n)
# RSI
rsi_n <- data.frame(rsi_n = c(14), flag = 1)

# SD
sd_n <- data.frame(sd_n = c(14), flag = 1)

# MFI
mfi_n <- data.frame(mfi_n = c(14), flag = 1)

# Bolinger bands
bb_n <- data.frame(bb_n = c(20), flag = 1)
bb_sd_n <- data.frame(bb_sd_n = c(2), flag = 1)

# Final set of parameters
testing_params <- left_join(testing_params, rsi_n) %>%
  left_join(sd_n) %>% left_join(mfi_n) %>%
  left_join(bb_n) %>% left_join(bb_sd_n)

testing_params$flag <- NULL
# Now loop over the set of parameters and decide on the best model

cl <- parallel::makeForkCluster(2)
doParallel::registerDoParallel(cl)
start_time <- Sys.time()

metrics <- foreach(i = 1:nrow(testing_params) , .combine = 'rbind') %dopar% {
  
  # Calculate indicators ---------
  
  # EMAs
  df$EMA_1 <- EMA(df$close, n = testing_params$ema1_n[i])
  df$EMA_2 <- EMA(df$close, n = testing_params$ema2_n[i])
  df$EMA_3 <- EMA(df$close, n = testing_params$ema3_n[i])
  df$EMA_4 <- EMA(df$close, n = testing_params$ema4_n[i])
  
  # Candle description
  df$HL <- df$high - df$low
  df$OC <- df$open - df$close
  
  # RSI
  df$RSI <- RSI(df$close, n = testing_params$rsi_n[i])
  
  # Standard deviation
  df$sd_14 <- rollapplyr(df$close, testing_params$sd_n[i], sd, fill = NA)
  
  # MACD default
  macd <- MACD(df[, "close"])
  df <- cbind(df,macd)
  
  # MFI
  df$mfi <- MFI(df[, c("high", "low", "close")], df[, "volume"],
                n = testing_params$mfi_n[i])
  
  # OBV default
  df$obv <- OBV(df[, "close"], df[, "volume"])
  
  # Bollinger
  bollinger <- BBands(df[ ,c("high", "low", "close")], n = testing_params$bb_n[i],
                      sd = testing_params$bb_sd_n[i])
  df <- cbind(df, bollinger)
  
  # Stohastic default
  stochOSC <- stoch(df[, c("high", "low", "close")])
  df <- cbind(df, stochOSC)
  
  # Create prediction variable
  df$prediction <- shift(df$close, type = "lead")
  
  # Define datasets
  train_data <- df[full_date_time <= "2020-10-23 05:00:00 CET"]
  test_data <- df[full_date_time >= "2020-11-07 13:00:00 CET"]
  
  # train_data <- df[full_date_time <= "2020-11-18 05:00:00 CET"]
  # test_data <- df[full_date_time > "2020-11-18 05:00:00 CET"]
  # 
  
  # Fit a classifier
  model <- randomForest(prediction ~  EMA_1 + EMA_2 + EMA_3 + EMA_4 + 
                           HL + OC + RSI + sd_14 + macd + signal +
                           mfi + obv + dn + mavg + up + pctB +
                           fastK + fastD + slowD + volume + close,
                         data = na.omit(train_data), importance = T, ntree = 500)
  
  fit1 <- predict(model, test_data)
  
  eval1 <- cbind(test_data, fit1)
  eval1 <- eval1[!is.na(prediction), ]
  
  metrics <- data.frame(MAPE = mape(eval1$prediction, eval1$fit1),
                             MSE = mean((eval1$prediction - eval1$fit1)^2),
                             MAE = mae(eval1$prediction, eval1$fit1),
                             RMSE = rmse(eval1$prediction, eval1$fit1),
                             RSQ = cor(eval1$prediction, eval1$fit1)^2)
  metrics
}
parallel::stopCluster(cl)
end_time <- Sys.time()
end_time - start_time

parameter_results <- cbind(testing_params, metrics)

saveRDS(parameter_results, file = paste0("/media/chris/DATA/Documents/Bot_Trading/ML_parameters",
        paste0("/" ,pair,"_", intervals,"_", Sys.time(), ".rds")))
