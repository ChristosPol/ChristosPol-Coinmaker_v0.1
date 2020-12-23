# Loading Data for operations --------------------------------------------------

# Create candlesticks for different intervals
ticks <- c(5, 60, 2, 4, 6, 12, 24)
units <- c(rep("minutes", 2), rep("hours", 5))

# Or choose a single one
ticks <- c(60)
units <- c(rep("minutes", 1))
intervals <- paste(ticks, units, sep = " ")

# Load trades and conver to OHLC, applies filtering
klines <- trades_to_OHLC(pair = pair,
               interval = intervals,
               from_date = "2020-07-01",
               to_date = "2020-10-01",
               date_subset = F)
names(klines) <- gsub(" ", "_", intervals)

# Get a first visual
df <- klines[[1]]
fig <- df %>% plot_ly(x = ~full_date_time , type="candlestick",
                      open = ~open, close = ~close,
                      high = ~high, low = ~low) 
fig <- fig %>% layout(title = pair,
                      xaxis = list(rangeslider = list(visible = F)))
fig


df_ETH <- copy(df)

df_BTC <- copy(klines[[1]])
cols <- c("close", "volume", "full_date_time")
df_BTC <- df_BTC[, ..cols] 
colnames(df_BTC)[1] <- "close_BTC"
colnames(df_BTC)[2] <- "volume_BTC"

# JOIN
df_comb <- left_join(df_ETH, df_BTC)
View(df_comb)
