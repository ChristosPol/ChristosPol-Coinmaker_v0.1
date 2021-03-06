# the idea behind this process is to load the last 720 ticks of data run optimization
# Run every x number of days and write the best set of parameters
rm(list = ls())

# Source function
source(paste("/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1", "10 Utils.R", sep = "/"))

# control threads, overthreading
setDTthreads(1)

# Path to save optim parameters
param_path <- "/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1/Parameter optimization/Parameters_multi"

# Path to save historical data pulled for each pair
data_path <- "/media/chris/DATA/Documents/Bot_Trading/Historical_data/Historical_multi"

# Load API information
api_info <- read.table(paste("/media/chris/DATA/Documents/Bot_Trading", "API_Keys.txt", sep = "/"), sep = ";", header = T)
API_Key <- as.character(api_info$API_Key)
API_Sign <- as.character(api_info$API_Sign)

# Choose pair - Here you get all available pairs and loop the over
avail_pairs <- myfun("https://api.kraken.com/0/public/AssetPairs", secret = API_Sign, key = API_Key)
all_pairs <- names(avail_pairs[[2]])

# Get only EUR related crypto pairs
EUR_pairs <- grep("EUR", all_pairs, value = T)

# Remove Forex pairs
to_remove <- grep(paste(c("USD",
                          ".d",
                          "AUD",
                          "CAD",
                          "JPY",
                          "CHF",
                          "GBP",
                          "REP",
                          "PAX",
                          "DAI",
                          "BAT",
                          "BAL",
                          "CRV",
                          "COMP",
                          "DOT",
                          "ICX",
                          "KAVA",
                          "SCE",
                          "STORJ",
                          "SNX",
                          "TRX",
                          "KSM",
                          "OXT"
                          ), collapse ="|"), EUR_pairs, value = T)
EUR_pairs <- EUR_pairs[!EUR_pairs %in% to_remove]

for (i in 1:length(EUR_pairs)) {

  pair <- EUR_pairs[i]
  # set  fake budget
  initial_budget <- 20
  
  # Create parameter pair directory
  dir.create(paste(param_path, pair, sep = "/"), showWarnings = FALSE)
  
  # Fix path for parameters
  pair_param_results <- paste(param_path, pair, sep ="/")
  
  # Create pair directory for historical data
  dir.create(paste(data_path, pair, sep ="/"), showWarnings = FALSE)
  
  # Fix path for hist data
  pair_data_results <- paste(data_path, pair, sep ="/")
  
  # Select time in epoch
  options("width" = 60)
  v <- nanotime(Sys.time() - as.difftime(60, unit = "days"))
  initial_id <- as.integer64(v)
  
  # Pull historical trades since initial id from epoch time
  hist_trades_pair(sleep = 4, hist_id = initial_id, pair = pair)
  
  # Load historical data and optimize
  # Loading Data for operations --------------------------------------------------
  options(digits = 5)
  
  # Csv file saved with historical trades
  file <- paste0(paste(pair_data_results, pair, sep = "/"), ".csv")
  
  # Read it
  frame <- fread(file)
  
  # # Fix 
  frame[, Date_POSIXct := anytime(as.numeric(as.character(V3)))]
  frame[, Time := strftime(Date_POSIXct, format = "%H:%M:%S")]
  colnames(frame) <- c("price", "volume", "epoch_time", "buy_sell", "market_limit",
                       "miscellaneous", "last_id", "Date_POSIXct", "Time")
  frame[, Date := as.Date(Date_POSIXct)]
  frame[, Hour := substr(frame$Time, 1,5)]
  frame[, miscellaneous := NULL]
  frame1 <- unique(frame)
  
  # Select interval
  frame1[, interval := strftime(ceiling_date(as.POSIXct(Date_POSIXct), '60 minutes') , format = '%H:%M:%S')]
  
  # Create candle stick dataset
  candles <- frame1[, .(high = max(price), low = min(price), open = first(price),
                        close = last(price), volume = sum(volume)),
                    by = .(Date, interval)]
  
  # select an existing part of the data to be able to calculate the indicators
  rows_needed <- ceiling(nrow(candles) / 40)
  df_needed <- candles[1:rows_needed, ]
  train_data <- df_needed
  
  # Dataset that will be treated as test data  
  test_data <- candles[(rows_needed + 1):nrow(candles), ]
  
  # testing parameters
  RSI_Period <- data.frame(RSI_Period = c(5, 10, 14), flag = 1)
  EMA_volume <- data.frame(EMA_volume = c(10), flag = 1)
  RSI_below <- data.frame(RSI_below = c(20,25,30,35, 40, 45, 50, 55), flag = 1)
  times_vol <- data.frame(times_vol = c(1), flag = 1)
  takeprofit <- data.frame(takeprofit = c(0.01, 0.015, 0.02, 0.025, 0.03, 0.05, 0.1, 1), flag = 1)
  stoploss_trail <- data.frame(stoploss_trail = c(0.01, 0.015, 0.02, 0.03, 0.05, 1), flag = 1)
  stoploss_ult <- data.frame(stoploss_ult = c(1), flag = 1)
  
  
  testing_params <- left_join(RSI_Period, EMA_volume)%>%
    left_join(RSI_below)%>% left_join(times_vol)%>% left_join(takeprofit)%>%
    left_join(stoploss_trail)%>%left_join(stoploss_ult)
  testing_params$flag <- NULL
  testing_params <- as.data.table(testing_params)
  
  library("doParallel")
  library("foreach")
  paraller_exec <- TRUE
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
    
    params <- paste(RSI_Period = testing_params$RSI_Period[i],
                    EMA_volume = testing_params$EMA_volume[i],
                    RSI_below = testing_params$RSI_below[i],
                    times_vol = testing_params$times_vol[i],
                    takeprofit = testing_params$takeprofit[i],
                    stoploss_trail = testing_params$stoploss_trail[i],
                    stoploss_ult = testing_params$stoploss_ult[i],
                    sep ="_")
    res <- calculate_profits(myresult, params = params)
     
    gc()
    
    res
  }
  end_time <- Sys.time()
  end_time - start_time
  parallel::stopCluster(cl)
  
  # Sloppy solution for fees
  results$clean_profit <-results$profit - (2*(20*(0.26/100)) * results$n_trades)   
  
  best_params <- results$params[which.max(results$clean_profit)]
  pr <- results$profit[results$params == best_params]
  cl_pr <- results$clean_profit[results$params == best_params]
  n <- results$n_trades[results$params == best_params]
  
  best_params <- strsplit(best_params, "_")
  best_params  <- data.frame(spar = best_params[[1]][1],
                            takeprofit = best_params[[1]][2],
                            stoploss_trail = best_params[[1]][3],
                            stoploss_ult = best_params[[1]][4],
                            winratio = best_params[[1]][5],
                            pr = pr,
                            cl_pr = cl_pr,
                            n  = n,
                            time = timestamp())
  
  write.table(best_params, file = paste(pair_param_results, paste0(pair, "_bestparams.csv"), sep ="/" ),
              row.names = FALSE,
              col.names = FALSE,
              append = TRUE,
              sep = ",")
}
