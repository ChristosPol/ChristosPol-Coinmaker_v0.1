rm(list = ls())
.rs.restartR()

### retire this git, go to binance
## Take best funtionalities from here move to binance
# Source functions
path_source <- "/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1/Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)

# Choose which unix time to use for pulling data
# Choose from ["start_of_time", "manually", "latest_available"]
unix_time <- "manually"

# Choose any pair to pull
# pair <- "XETHZEUR"
pair <- "ETHEUR"
# pair <- "ALGOEUR"
# pair <- "KAVAEUR"
# pair <- "GNOEUR"
# pair <- "ADAEUR"
# pair <- "STORJEUR"
# Path to save results
data_path <- "/media/chris/DATA/Documents/Bot_Trading/Historical_data"

# Create pair directory
dir.create(paste(data_path, pair, sep ="/"), showWarnings = FALSE)

# Fix path
pair_data_results <- paste(data_path, pair, sep ="/")

# Or choose a single one
ticks <- c(60)
units <- c(rep("minutes", 1))
intervals <- paste(ticks, units, sep = " ")
# from_date = "2021-02-22"
# to_date = "2021-02-23"

file <- paste0(paste(pair_data_results, pair, sep = "/"), ".csv.gz")
frame <- fread(file)
colnames(frame) <- c("price", "volume", "epoch_time", "buy_sell", "market_limit",
                     "last_id", "Date_POSIXct", "Time", "Date", "Hour")
# frame1 <- subset(frame, frame$Date >= from_date & frame$Date < to_date)
frame1 <- subset(frame, frame$Date_POSIXct >= "2021-02-22 15:15:00" & frame$Date_POSIXct <= "2021-02-22 15:25:00")
frame1 <- tail(frame, 10000)

copied <- copy(frame1)

trades <- data.table(current_price = NA,price_action = NA, at = NA, action = "no_action", pos_perc = NA,
                     tp = 0.01, exit = NA, signal  =NA)
all_trades <- data.table(current_price = NA, price_action = NA, at = NA, action = "no_action", pos_perc = NA,
                         tp = 0.01, exit = NA, signal = NA)
for(i in 1:nrow(copied)){

copied1 <- copied[1:(i+2500), ]
copied1[, interval := strftime(floor_date(as.POSIXct(Date_POSIXct), intervals),
                               format = '%Y-%m-%d %H:%M:%S')]
copied1 <- copied1[, .(high = max(price), low = min(price), open = first(price),
           close = last(price), volume = sum(volume)),
       by = .(interval)]
copied1[, diff := ((copied1[, tail(close, 1)]- copied1[, tail(close, 2)][1]) / copied1[, tail(close, 1)])*100]

signal <- copied1[, tail(diff, 1)] 

if(signal < -1 & (all_trades[, tail(action, 1)] == "no_action"  | all_trades[, tail(action, 1)] == "sold")) {
  
  trades[, "price_action"] <- copied1[, tail(close, 1)]
  trades[, "current_price"] <- copied1[, tail(close, 1)]
  trades[, "at"] <- copied1[, tail(interval, 1)]
  trades[, "action"] <- "long"
  trades[, "pos_perc"] <- ((all_trades[, tail(current_price, 1)]  - trades[, "price_action"] )/ all_trades[, tail(current_price, 1)] )*100
  # trades[, "exit"] <-  tail(trades[, "price_bought"],1) * tail(trades[, tp], 1) +  tail(trades[, "price_bought"],1) < tail(trades[, "price_bought"],1) 
  
} else if (tail(all_trades[, "pos_perc"], 1) > 1 &  all_trades[, tail(action, 1)] %in% c("long","keep") ){
  trades[, "action"] <- "sold"
  trades[, "price_action"] <- all_trades[, tail(price_action, 1)]
  trades[, "current_price"] <- copied1[, tail(close, 1)]
  trades[, "at"] <- copied1[, tail(interval, 1)]
  trades[, "pos_perc"] <- ((all_trades[, tail(current_price, 1)]  - trades[, "price_action"] )/ all_trades[, tail(current_price, 1)])*100
  # trades[, "pos_perc"] <-  (trades[, "price_bought"] - tail(copied1[, close], 1) )/ trades[, "price_bought"]
  # trades[, "pos_perc"] <-  (trades[, "price_bought"] - tail(copied1[, close], 1) )/ trades[, "price_bought"]
} else if (all_trades[, tail(action, 1)] %in% c("long", "keep") & ( tail(all_trades[, "pos_perc"],1) < 1)  ) {
  
  trades[, "action"] <- "keep"
  trades[, "current_price"] <- copied1[, tail(close, 1)]
  trades[, "at"] <- copied1[, tail(interval, 1)]
  trades[, "price_action"] <- all_trades[, tail(price_action, 1)]
  
  trades[, "pos_perc"] <- ((all_trades[, tail(current_price, 1)]  - trades[, "price_action"] )/ all_trades[, tail(current_price, 1)] )*100
  # trades[, "pos_perc"] <-  (trades[, "price_bought"] - tail(copied1[, close], 1) )/ trades[, "price_bought"]
  # trades[, "pos_perc"] <-  (trades[, "price_bought"] - tail(copied1[, close], 1) )/ trades[, "price_bought"]
} else {
  
  trades[, "action"] <- "no_action"
  trades[, "current_price"] <- copied1[, tail(close, 1)]
  trades[, "at"] <- copied1[, tail(interval, 1)]
  trades[, "pos_perc"] <- NA
}
all_trades <- rbind(all_trades, trades)
all_trades$signal[nrow(all_trades)] <-  signal
# x <- ((copied1[, tail(close, 1)]- copied1[, tail(close, 2)][1]) / copied1[, tail(close, 1)])*100
print(all_trades)
Sys.sleep(0.1)
plot_candlesticks(dta = copied1, Ns = nrow(copied1), asset = pair)

plot_df <- all_trades[!is.na(all_trades$at)]


if("long" %in% plot_df$action){
  
  
  df_points_buy <- data.frame(x = which(na.omit(unique(copied1$interval)) %in% plot_df$at[plot_df$action == "long"]),
                              y = plot_df$price_action[plot_df$action == "long"])
}
if("sold" %in% plot_df$action){
  df_points_sell <- data.frame(x = which(unique(copied1$interval) %in% plot_df$at[plot_df$action == "sold"]),
                              y = plot_df$current_price[plot_df$action == "sold"])
}
if(exists("df_points_buy")){
  points(df_points_buy$x, df_points_buy$y, col = "blue", pch = 19)
}
if(exists("df_points_sell")){
  points(df_points_sell$x, df_points_sell$y, col = "red", pch = 19)
}
# print(i)
}
View(all_trades)
View(all_trades[action != "no_action",])
View(all_trades[!action %in% c("no_action", "keep"),])

candles[[i]] <- copied[, .(high = max(price), low = min(price), open = first(price),
                           close = last(price), volume = sum(volume)),
                       by = .(interval)]
candles[[i]]$full_date_time <- as.POSIXct(paste(candles[[i]]$Date,
                                                candles[[i]]$interval),
                                          format="%Y-%m-%d %H:%M:%S")





df <- klines[[1]]
df[, diff := c(diff(df[, close]), 0)]
df[, per_diff :=  (diff / close)*100]
df$diff_per1 <- c(0, df$per_diff[-length(df$per_diff)])



ggplot(data = df, aes(x = full_date_time, y = diff)) +
  geom_line() +
  geom_hline(yintercept = mean(df[diff > 0, diff]), col = "green") +
  geom_hline(yintercept = mean(df[diff < 0, diff]), col = "red") +
  geom_hline(yintercept = median(df[diff > 0, diff]), col = "green") +
  geom_hline(yintercept = median(df[diff < 0, diff]), col = "red")


x <- as.Date("2021-01-22")
wday(x, getOption("lubridate.week.start", 1)) #4

wday(ymd(080101))
wday(ymd(080101), label = TRUE, abbr = FALSE)
wday(ymd(080101), label = TRUE, abbr = TRUE)
wday(ymd(080101) + days(-2:4), label = TRUE, abbr = TRUE)

x <- as.Date("2009-09-02")
yday(x) #245
mday(x) #2
yday(x) <- 1  #"2009-01-01"
yday(x) <- 366 #"2010-01-01"
mday(x) > 3

1028.87 - 1023.46
5.41 / 1028
