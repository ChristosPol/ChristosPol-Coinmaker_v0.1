# Preamble ---------------------------------------------------------------------

# screen -S pull_BTC R
.rs.restartR()
rm(list = ls())
source(paste("/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1", "10 Utils.R", sep = "/"))
setDTthreads(1)

# Pulling historical data ------------------------------------------------------

# API info
api_info <- read.table(paste("/media/chris/DATA/Documents/Bot_Trading", "API_Keys.txt", sep = "/"), sep = ";", header = T)

API_Key <- as.character(api_info$API_Key)
API_Sign <- as.character(api_info$API_Sign)

# Choose pair
# pair <- "BTCEUR"
pair <- "XETHZEUR"

# Path to save results0
data_path <- "/media/chris/DATA/Documents/Bot_Trading/Historical_data"

# Create pair directory
dir.create(paste(data_path, pair, sep ="/"), showWarnings = FALSE)

# Fix path
pair_data_results <- paste(data_path, pair, sep ="/")

# Choose initial ID for the first pull
# Either select a period in actual days
options("width" = 60)
v <- nanotime(Sys.time() - as.difftime(300, unit = "days"))
initial_id <- as.integer64(v)

# Or pull from last ID
initial_id <- 1598160109670382691
initial_id <- as.numeric(as.character(initial_id))

# Pull historical trades since initial id from epoch time
hist_trades_pair(sleep = 3, hist_id = initial_id, pair = pair)
