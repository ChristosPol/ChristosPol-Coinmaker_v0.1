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
frame1 <- subset(frame1, frame1$Date >= "2018-06-01")


# Select interval
frame1[, interval := strftime(ceiling_date(as.POSIXct(Date_POSIXct), '30 minutes') , format = '%H:%M:%S')]

# Create candle stick dataset
candles <- frame1[, .(high = max(price), low = min(price), open = first(price),
                       close = last(price), volume = sum(volume)),
                  by = .(Date, interval)]
dim(candles)

# Plot asset and select how many intervals 
par(mfrow=c(1,1))
plot_candlesticks(dta = candles, Ns = nrow(candles), asset = pair)

# Remove and clean
rm(frame)
rm(frame1)
gc()