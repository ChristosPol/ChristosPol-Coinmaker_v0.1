# Loading Data for operations --------------------------------------------------

# Create candlesticks for different intervals
ticks <- c(5, 60, 2, 4, 6, 12, 24)
units <- c(rep("minutes", 2), rep("hours", 5))

ticks <- c(60)
units <- c(rep("minutes", 1))


intervals <- paste(ticks, units, sep = " ")
klines <- trades_to_OHLC(pair = pair,
               interval = intervals,
               from_date = "2020-01-01",
               date_subset = T)

names(klines) <- gsub(" ", "_", intervals)
df <- klines[[1]]
df$x <- 1:nrow(df)
ggplot(data= df, aes(x=x, y=close)) +
  geom_line(alpha = 0.5) 

