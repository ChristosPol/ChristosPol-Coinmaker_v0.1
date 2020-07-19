# Define historical demand and supply based on volume of buy and sell orders
# Point is to define a reversal of a trend
# In the future order should be added for more accurate predictions

rm(list = ls())
source(paste("/media/chris/DATA/Documents/Bot_Trading/Coinmaker", "10 Utils.R", sep = "/"))
setDTthreads(1)
options(digits =5)
# API info
api_info <- read.table(paste("/media/chris/DATA/Documents/Bot_Trading", "API_Keys.txt", sep = "/"), sep = ";", header = T)

API_Key <- as.character(api_info$API_Key)
API_Sign <- as.character(api_info$API_Sign)

# Choose pair
pair <- "XETHZEUR"

# Initial ID must be defined in epoch time in nanoseconds 

epoch <- nanotime(Sys.time() - as.difftime(10, unit = "mins"))
initial_id <- as.character(as.integer64(epoch))

hist_id <- initial_id
dd <- data.table()
da <- data.table()
repeat{
Sys.sleep(3)
  what <- tryCatch(
  {
    url <- paste0('https://api.kraken.com/0/public/Trades?pair=',pair,'&since='
                  ,hist_id)
    dat <- jsonlite::fromJSON(url)
    },
    error = function(e){})
  if(is.null(dat$result[1])) next # error, skip
  if(nrow(as.data.frame(dat$result[1])) == 0) break # last batch empty
    temp <- cbind(data.frame(dat$result[1]), dat$result$last)
    hist_id <- dat$result$last
    da <- rbind(da, temp)
}

da$Date_POSIXct <- anytime(as.numeric(as.character(da$XETHZEUR.3)))
da$Time <- strftime(da$Date_POSIXct, format = "%H:%M:%S")

colnames(da) <- c("price", "volume", "epoch_time", "buy_sell", "market_limit",
                    "miscellaneous", "last_id", "Date_POSIXct", "Time")
da$Date <-  substr(da$Date_POSIXct,1, 10)
da[, Hour := substr(da$Time, 1,2)]
da[, Min := substr(da$Time, 4,5)]


f <- da %>% group_by(buy_sell,Date, Hour, Min)%>%summarise(vol_sum = sum(as.numeric(volume))) %>% arrange(Date, Hour, Min)
f
f <- da %>% group_by(buy_sell,Date, Hour, Min)%>%summarise(vol_sum = sum(as.numeric(volume))) %>% arrange(Date, Hour, Min)%>%
  ungroup() %>% group_by(Date, Hour, Min) %>% summarise(ratio = round(vol_sum[buy_sell=="b"]/vol_sum[buy_sell=="s"], digits=2))
f


df <- simple_OHLC(interval = 60, pair=pair)
df$day <- substr(df$Date_POSIXct,1,13)
f$day <- paste(f$Date, f$Hour, sep =" ")

View(left_join(df, f, by = "day"))
df <- subset(df, df$day%in% f$day)
plot(df$close, type ="l")
plot(f$ratio, type ="l")

par(mfrow=c(2,1))
plot(df$close, type ="l")
plot(f1$ratio, type="l")
abline(h =1)

# View(tedamp)
# frame[, Date := as.Date(Date_POSIXct)]
# frame[, Hour := substr(frame$Time, 1,5)]
# frame[, miscellaneous := NULL]
# View(temp)
# plot(as.numeric(as.character(temp$price)))
# set up the historical trades, accumulate and summarise demand and supply by summarizing volume by buy 
# and sell
# temp%>%group_by(buy_sell)%>% summarise(sum(as.numeric(volume)))
