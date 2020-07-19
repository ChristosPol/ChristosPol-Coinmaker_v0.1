# ------------------------------------------------------------------------------
url <- "https://api.kraken.com/0/public/Depth?pair=ETHEUR&count=1752"
dat <- jsonlite::fromJSON(url)

df_bids <- as.data.table(dat$result[1]$XETHZEUR$bids)
colnames(df_bids) <- c("price", "volume", "time")
df_bids[, Date_POSIXct := anytime(as.numeric(as.character(time)))]
df_bids$Date_POSIXct <- as.character(df_bids$Date_POSIXct)

# as numeric
df_bids$price <- as.numeric(df_bids$price)
df_bids$volume <- as.numeric(df_bids$volume)
df_bids$time <- as.numeric(df_bids$time)
df_bids$day <- substr(df_bids$Date_POSIXct, 1, 10)
df_bids <- df_bids %>%group_by(day)%>% summarise(price =sum(price), volume =sum(volume))
# ------------------------------------------------------------------------------
dat <- jsonlite::fromJSON(url)

df_asks <- as.data.table(dat$result[1]$XETHZEUR$asks)
colnames(df_asks) <- c("price", "volume", "time")
df_asks[, Date_POSIXct := anytime(as.numeric(as.character(time)))]
df_asks$Date_POSIXct <- as.character(df_asks$Date_POSIXct)

# as numeric
df_asks$price <- as.numeric(df_asks$price)
df_asks$volume <- as.numeric(df_asks$volume)
df_asks$time <- as.numeric(df_asks$time)
df_asks <- df_asks %>%group_by(Date_POSIXct, price)%>% mutate(volume =sum(volume))
View(df_asks)

par(mfrow=c(3,1))
plot(rev(df_bids$price), type ="l")
plot(df_bids$volume, type ="l")

plot(rev(df_asks$price), type ="l")
plot(df_asks$volume, type ="l")


df<- simple_OHLC(interval = 1, pair = "ETHEUR")
plot(df$close, type ="l")
