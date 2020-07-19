# the idea behind this process is to load the last 720 ticks of data run optimization
# loop and calculate resistance and support levels

# Run every x number of days and write the best set of parameters

rm(list = ls())

source(paste("/media/chris/DATA/Documents/Bot_Trading/Coinmaker", "10 Utils.R", sep = "/"))
setDTthreads(1)


api_info <- read.table(paste("/media/chris/DATA/Documents/Bot_Trading", "API_Keys.txt", sep = "/"), sep = ";", header = T)

API_Key <- as.character(api_info$API_Key)
API_Sign <- as.character(api_info$API_Sign)

# Choose pair
pair <- "ETHEUR"

# set  fake budget
initial_budget <- 500

# Path to save results0
data_path <- "/media/chris/DATA/Documents/Bot_Trading/Coinmaker/Parameter optimization/Parameters"

# Create pair directory
dir.create(paste(data_path, pair, sep ="/"), showWarnings = FALSE)

# Fix path
pair_data_results <- paste(data_path, pair, sep ="/")

# Get Tick data
df <- simple_OHLC(interval = 5, pair = pair)

# select an existing part of the data to be able to calculate the indicators
rows_needed <- ceiling(nrow(df) / 20)
df_needed <- df[1:rows_needed, ]
train_data <- df_needed

# Dataset that will be treated as test data  
test_data <- df[(rows_needed + 1):nrow(df), ]


# Define parameters to loop over
RSI_Period <- data.frame(RSI_Period = c(5, 10, 14, 20), flag = 1)
RSI_below <- data.frame(RSI_below = c(20, 25, 30, 35, 40, 45, 50), flag = 1)
EMA_volume <- data.frame(EMA_volume = c(5 ,10 ,15 ), flag = 1)
takeprofit <- data.frame(takeprofit = c(0.01, 0.015, 0.02, 0.025, 0.03, 0.05), flag = 1)
stoploss_trail <- data.frame(stoploss_trail = c(0.01, 0.02, 0.03, 0.05), flag = 1)
stoploss_ult <- data.frame(stoploss_ult = c(0.01,0.02, 0.05), flag = 1)
times_vol <- data.frame(times_vol = c(1, 2), flag = 1)

testing_params <- left_join(RSI_Period, RSI_below) %>%
  left_join(EMA_volume)%>% left_join(takeprofit)%>% left_join(stoploss_trail)%>% left_join(times_vol)%>%left_join(stoploss_ult)
testing_params$flag <- NULL

# Execute in paraller or not
cl <- parallel::makeForkCluster(3)
doParallel::registerDoParallel(cl)
start_time <- Sys.time()
results <- foreach(i = 1:nrow(testing_params), .combine = 'rbind') %dopar% {
  myresult <- Pure_RSI_Volume_Trailing(RSI_Period = testing_params$RSI_Period[i],
                                       RSI_below =  testing_params$RSI_below[i] ,
                                       times_vol = testing_params$times_vol[i],
                                       EMA_volume = testing_params$EMA_volume[i],
                                       takeprofit = testing_params$takeprofit[i],
                                       stoploss_trail = testing_params$stoploss_trail[i],
                                         stoploss_ult = testing_params$stoploss_ult[i])
  
  res <- calculate_profits(myresult)
  res$params <- paste(RSI_Period = testing_params$RSI_Period[i],
                      RSI_below =  testing_params$RSI_below[i] ,
                      times_vol = testing_params$times_vol[i],
                      EMA_volume = testing_params$EMA_volume[i],
                      takeprofit = testing_params$takeprofit[i],
                      stoploss_trail = testing_params$stoploss_trail[i],
                      stoploss_ult = testing_params$stoploss_ult[i],
                      sep ="_")
  
  res
}
end_time <- Sys.time()
end_time - start_time
parallel::stopCluster(cl)


# Sloppy solution for fees
results$clean_profit <-results$profit - (2*(500*(0.26/100)) * results$n_trades)   
best_params <- results$params[which.max(results$clean_profit)]
best_params <- strsplit(best_params, "_")
best_params <- data.frame(RSI_Period = best_params[[1]][1],
           RSI_below = best_params[[1]][2],
           times_vol = best_params[[1]][3],
           EMA_volume = best_params[[1]][4],
           takeprofit = best_params[[1]][5],
           stoploss_trail = best_params[[1]][6],
           stoploss_ult = best_params[[1]][7],
           time = timestamp())

write.table(best_params, file = paste(pair_data_results, "best params.csv", sep ="/" ),
            row.names = FALSE,
            col.names = FALSE,
            append = TRUE,
            sep = ",")

