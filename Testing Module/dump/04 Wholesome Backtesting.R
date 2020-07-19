.rs.restartR()
rm(list = ls())

options(digits=20)
options(scipen=999)

source(paste("/media/chris/DATA/Documents/Bot_Trading/Coinmaker", "10 Utils.R", sep = "/"))

# ------------------------------------------------------------------------------
# Choose pair
pair <- "XETHZEUR"

# Path to save results
data_path <- "/media/chris/DATA/Documents/Bot_Trading/Historical_data"

# Create pair directory
dir.create(paste(data_path, pair, sep ="/"), showWarnings = FALSE)

# Fix path
pair_data_results <- paste(data_path, pair, sep ="/")


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

# Choose Date Interval
frame1 <- subset(frame, frame$Date >= "2020-01-01")
frame1 <- unique(frame1)

rm(frame)

Kraken_intervals <- c("15 minutes") #, "30 minutes", "1 hour", "2 hour")

candle_list <- list()

for (i in 1:length(Kraken_intervals)) {
  
  frame1$interval <- strftime(ceiling_date(as.POSIXct(frame1$Date_POSIXct), Kraken_intervals[i]) , format = '%H:%M:%S')
  candles <- copy(frame1)
  candle_list[[i]] <- candles[, .(high = max(price), low = min(price), open = first(price),
                         close = last(price), volume = sum(volume)),
                     by = .(Date, interval)]
}

rm(candles)
rm(frame1)

# testing parameters
test_fast <- c(seq(5, 30, by = 5))
test_slow <- c(seq(40, 100, by = 10))
test_fast <- 14
test_slow <- 65
testing_params <- data.frame(f = rep(test_fast, length(test_slow)), s = rep(test_slow, each = length(test_fast)))
testing_params$flag <- 1
 
rsi_p <- data.frame(rsi_period = c(5, 15, 25), flag = 1)
upper <- data.frame(up = c(60, 70, 80), flag = 1)
lower <- data.frame(low = c(40, 30, 20), flag = 1)
tr <- data.frame(tr = c(30, 70, 120, 200), flag = 1)
tr_per <- data.frame(tr_per = c(30, 100), flag = 1)
vol <- data.frame(vol = c(5, 20, 40), flag = 1)
testing_params <- left_join(testing_params, rsi_p) %>% left_join(upper) %>%
  left_join(lower) %>% left_join(tr) %>% left_join(tr_per)%>% left_join(vol)

testing_params$flag <- NULL

testing_params$bool <- testing_params$f < testing_params$s
testing_params <- subset(testing_params, testing_params$bool == T) 

# j_result <- list()
# 
# for (j in 1:length(Kraken_intervals)){

# N Training -------------------------------------------------------------------
train_n <- ceiling(nrow(candle_list[[1]]) / 3) 
train_data <- candle_list[[1]][1:train_n, ]

# Test, same -------------------------------------------------------------------
test_data <- candle_list[[1]][(train_n + 1):nrow(candle_list[[1]]), ]

initial_budget <- 500

myresult <- list()

for (i in 1:nrow(testing_params)){
  myresult[[i]] <- RSI_Swings(fast_EMA = testing_params$f[i], slow_EMA = testing_params$s[i], Volume_EMA = testing_params$vol[i], RSI_Period = testing_params$rsi_period[i],
                              upper_RSI_Bound = testing_params$up[i], lower_RSI_Bound = testing_params$low[i], stoploss = 0.01, trend = testing_params$tr[i],
                              trend_periods = testing_params$tr_per[i])
  print(i/nrow(testing_params))
}


res <- list()
for( i in 1:length(myresult)){
  res[[i]] <- calculate_profits(myresult[[i]])
}


final <- do.call(rbind, res)
final$params <- NULL
final_15min <- cbind(final, testing_params[1:length(myresult),])
final_15min$fees <- 1.5 *final_15min$n_trades
final_15min$clean_profit <- final_15min$profit- final_15min$fees
View(final_15min)


final <- do.call(rbind, res)
final$params <- NULL
final_30min <- cbind(final, testing_params[1:length(myresult),])
final_30min$fees <- 1.5 *final_30min$n_trades
final_30min$clean_profit <- final_30min$profit- final_30min$fees
View(final_30min)


final <- do.call(rbind, res)
final$params <- NULL
final_1hour <- cbind(final, testing_params[1:length(myresult),])
final_1hour$fees <- 1.5 *final_1hour$n_trades
final_1hour$clean_profit <- final_1hour$profit- final_1hour$fees
View(final_1hour)


saveRDS(final_15min, file = paste("/media/chris/DATA/Documents/Bot_Trading/Testing Module/Backtesting_results", "15_min_start_20200101.rds", sep ="/"))
saveRDS(final_30min, file = paste("/media/chris/DATA/Documents/Bot_Trading/Testing Module/Backtesting_results", "30_min_start_20200101.rds", sep ="/"))
saveRDS(final_1hour, file = paste("/media/chris/DATA/Documents/Bot_Trading/Testing Module/Backtesting_results", "1hour_start_20200101.rds", sep ="/"))
saveRDS(final_2hour, file = paste("/media/chris/DATA/Documents/Bot_Trading/Testing Module/Backtesting_results", "2hour_start_20200101.rds", sep ="/"))
