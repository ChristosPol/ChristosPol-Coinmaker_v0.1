trading_table_path <- "/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1/Trading Module/Trading_Table/"

dirs <- list.files(trading_table_path, full.names = T) 
pair_names <- gsub("_table.csv", "",list.files(trading_table_path)) 


trade_tables <- lapply(dirs, function(x){read.table(x, header = FALSE,
                                                    sep = ",", stringsAsFactors = FALSE)})
View(trade_tables[[1]])
names(trade_tables) <- pair_names
columns <- c("time", "open", "high", "low", "close",         
             "vwap", "volume", "count", "Date_POSIXct", "servertime",    
             "systemtime", "x", "spline", "deriv", "sign_derivs",
             "change_sign", "exit_condition", "action", "Units", "Price",         
             "tp", "ult_sl", "trail_sl", "id" )

trade_tables <- lapply(trade_tables, setNames, columns)
for(i in 1:length(trade_tables)){
  trade_tables[[i]]$pair <- pair_names[i]
}
all_trade_tables <- do.call(rbind, trade_tables)
all_trade_tables$Price_all <- all_trade_tables$close*all_trade_tables$Units
rownames(all_trade_tables) <- NULL
View(all_trade_tables)
View(subset(all_trade_tables, all_trade_tables$action %in% c("buy", "sell")))

all_trade_tables <- as.data.table(all_trade_tables)
