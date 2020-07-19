# Description ------------------------------------------------------------------
# Scheduler for trading at the end of each candle and not intra-candle
library(cronR)

# Path of live trading Rscript
path <- "/media/chris/DATA/Documents/Bot_Trading/Coinmaker/Parameter optimization/01 Get Data and Optimize.R"

# Command
cmd <- cron_rscript(path)

# add frequency and intervals   
cron_add(cmd, frequency = 'daily', id = 'Optimize parameters', description = 'Optimize parameters', at = '23:00')

# Check all jobs
cron_ls()

# Stop Job
cron_rm(id = "Optimize parameters")
