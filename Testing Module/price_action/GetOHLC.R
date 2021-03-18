rm(list=ls())

# Source functions
path_source <- "/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1/Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)
path <- "/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1/Testing Module/price_action/ohlc/"

# Choose pair and interval
pair <- c("ETH/EUR", "ADA/EUR", "BTC/EUR", "DOT/EUR", "XRP/EUR", "LINK/EUR", "LTC/EUR")
interval = 60

for (i in 1:length(pair)){
  OHLC <- simple_OHLC(interval, pair[i])
  OHLC_last_bar <- OHLC[nrow(OHLC)-1, ]
  write.csv(OHLC_last_bar, paste0(path ,"OHLC_last_bar_", gsub("/", "-",pair[i]), ".csv"))
  Sys.sleep(8)
}
