# Preamble ---------------------------------------------------------------------
# screen -S pullingETH R
.rs.restartR()
rm(list = ls())
### retire this git, go to binance
## Take best funtionalities from here move to binance
# Source functions
path_source <- "/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1/Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)

# Choose any pair to pull
pair <- "XETHZEUR"
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

# Loading Data for operations --------------------------------------------------

# Or choose a single one
ticks <- c(60)
units <- c(rep("minutes", 1))
intervals <- paste(ticks, units, sep = " ")

# Load trades and conver to OHLC, applies filtering
klines <- trades_to_OHLC(pair = pair,
                         interval = intervals,
                         from_date = "2018-01-01",
                         to_date = "2020-12-10",
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

# Determine volatility
next_l <- c(df$low[-1], 0)
df[, next_low := next_l]
df[, jump_enter := round( ((next_low - close)/close)*100, 4)]

df <- df[-nrow(df), ]

decrease <- df[jump_enter < 0, ]

mean_decrease <- mean(decrease[, jump_enter])
median_decrease <- median(decrease[, jump_enter])
hist(decrease[, jump_enter], breaks = 30)

boxplot(decrease[, jump_enter])
