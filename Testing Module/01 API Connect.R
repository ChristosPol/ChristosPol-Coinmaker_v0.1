# Preamble ---------------------------------------------------------------------

# screen -S pull_rest_eth_data R
.rs.restartR()
rm(list = ls())
options(digits=22)
options(scipen=999)
library(nanotime)
source(paste("/media/chris/DATA/Documents/Bot_Trading/Coinmaker", "10 Utils.R", sep = "/"))
setDTthreads(1)

# Pulling historical data ------------------------------------------------------

# API info
api_info <- read.table(paste("/media/chris/DATA/Documents/Bot_Trading", "API_Keys.txt", sep = "/"), sep = ";", header = T)

API_Key <- as.character(api_info$API_Key)
API_Sign <- as.character(api_info$API_Sign)

# Choose pair
pair <- "XETHZEUR"
# pair <- "BTCEUR"
# pair <- "QTUMEUR"
# pair <- "ETCEUR"
# # pair <- "ZECEUR"
# pair <- "EURUSD"
# pair <- "TRXEUR"

# Path to save results0
data_path <- "/media/chris/DATA/Documents/Bot_Trading/Historical_data"

# Create pair directory
dir.create(paste(data_path, pair, sep ="/"), showWarnings = FALSE)

# Fix path
pair_data_results <- paste(data_path, pair, sep ="/")

# Choose initial ID for the first pull
initial_id <- 1592985088058073687



hist_trades_pair(sleep = 3, hist_id = initial_id, pair = pair)

library(bit64)
options("width"=60)

v <- nanotime(Sys.time() - as.difftime(60, unit = "days"))
initial_id <- as.integer64( v )
as.numeric(as.character(initial_id))

    url <- paste0('https://api.kraken.com/0/public/Trades?pair=',pair,'&since='
                  ,as.numeric(initial_id))
    dat <- jsonlite::fromJSON(url)
temp <- cbind(data.frame(dat$result[1]), dat$result$last)


temp$Date_POSIXct <- anytime(as.numeric(as.character(temp$XETHZEUR.3)))
temp$Time <- strftime(temp$Date_POSIXct, format = "%H:%M:%S")

colnames(temp) <- c("price", "volume", "epoch_time", "buy_sell", "market_limit",
                     "miscellaneous", "last_id", "Date_POSIXct", "Time")
View(temp)
frame[, Date := as.Date(Date_POSIXct)]
frame[, Hour := substr(frame$Time, 1,5)]
frame[, miscellaneous := NULL]
View(temp)
plot(as.numeric(as.character(temp$price)))
set up the historical trades, accumulate and summarise demand and supply by summarizing volume by buy 
 and sell
 temp%>%group_by(buy_sell)%>% summarise(sum(as.numeric(volume)))
 