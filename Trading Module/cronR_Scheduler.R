# Description ------------------------------------------------------------------
# Scheduler for trading at the end of each candle and not intra-candle
library(cronR)

# Path of live trading Rscript
# path <- "/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1/Trading Module/Live_trading.R"
path <- "/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1/Trading Module/Multi_EUR_pair_Live_Trading.R"

# Command
cmd <- cron_rscript(path)

# add frequency and intervals
cron_add(cmd, frequency = '*/60 * * * *', id = 'Live trading', description = 'Live trading', at = '19:00')

# Check all jobs
cron_ls()

# Stop Job
# cron_clear(ask = FALSE)
cron_rm(id = "Live trading")

