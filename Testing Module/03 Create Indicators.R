# # Testing strategies ------------------------------------------------------------

# Badget 
initial_budget <- 500

# select period of data 
candles_recent <- candles
# 
# training data here is not the traditional training set, but more of a 
# placeholder initial dataset to calculate the indicators
train_n <- ceiling(nrow(candles_recent) / 30)
train_data <- candles_recent[1:train_n, ]

# Test, same 
test_data <- candles_recent[(train_n + 1):nrow(candles_recent), ]

# One the many strategies tested, this one goes long when RSI is below threshold
# and finds spike of sold volume
myresult <- Pure_RSI_Volume_Trailing(RSI_Period = 5,
                               RSI_below = 40,
                               EMA_volume = 5,
                               takeprofit = 0.1,
                               stoploss_trail = 0.02,
                               stoploss_ult = 1,
                               times_vol = 1)

# Close last position
if(myresult$action[nrow(myresult)] == "keep") {
  myresult$action[nrow(myresult)] <- "sell"
  myresult$Price[nrow(myresult)] <- myresult$close[nrow(myresult)] * myresult$Units[nrow(myresult)]
}

# Calculate profits
calculate_profits(myresult)

# how many trades were succesful (to be functioned)
mah <- subset(myresult,myresult$action %in% c("buy","sell") )
profitable_trades <- list()
ids_s <- unique(mah$id)
for(i in 1:length(unique(mah$id))){
  
  profitable_trades[[i]] <- mah$close[mah$id == ids_s[i] & mah$action =="sell"]-mah$close[mah$id == ids_s[i] & mah$action =="buy"]
}
table(unlist(profitable_trades) > 0)

# Plot each trade's indicators and price action (to be functioned)
mytest <- myresult
idents <- unique(mytest$id)[!is.na(unique(mytest$id))]
par(mfrow = c(3, 1))

i <- 1
for (i in 1:length(idents)){
  
  h <- head(which(mytest$id == idents[i]),1) -200
  
  if(h < 0){
    h <- 1
  }
  t <- tail(which(mytest$id == idents[i]),1) + 200
  mytest <- myresult[h:t, ]
  ident <- idents[i]
  
  plot(1:nrow(mytest), mytest$close, type = "l")
  buyprice <- mytest$close[mytest$action =="buy" & mytest$id ==ident][!is.na(mytest$close[mytest$action =="buy" & mytest$id ==ident])]
  sellprice <- mytest$close[mytest$action =="sell" & mytest$id ==ident][!is.na(mytest$close[mytest$action =="sell" & mytest$id ==ident])]
  
  mtext(round((sellprice - buyprice)/buyprice, digits = 3), side = 3)

  points(which(mytest$action =="buy" & mytest$id ==ident), mytest$close[mytest$action =="buy" & mytest$id ==ident][!is.na(mytest$close[mytest$action =="buy" & mytest$id ==ident])], pch =19, col ="green")
  points(which(mytest$action =="sell"& mytest$id ==ident), mytest$close[mytest$action =="sell"& mytest$id ==ident][!is.na(mytest$close[mytest$action =="sell"& mytest$id ==ident])], pch =19, col ="red")
  
  plot(mytest$RSI, col ="blue", type ="l")#, ylim = c(mn, mx))
  points(which(mytest$action =="buy" & mytest$id ==ident), mytest$RSI[mytest$action =="buy" & mytest$id ==ident][!is.na(mytest$RSI[mytest$action =="buy" & mytest$id ==ident])], pch =19, col ="blue")
  points(which(mytest$action =="sell"& mytest$id ==ident), mytest$RSI[mytest$action =="sell"& mytest$id ==ident][!is.na(mytest$RSI[mytest$action =="sell"& mytest$id ==ident])], pch =19, col ="black")
  
  abline(h = 40)
  
  plot(mytest$volume, type ="l")
  lines(mytest$EMA_volume, col ="red")
  points(which(mytest$action =="buy" & mytest$id ==ident), mytest$volume[mytest$action =="buy" & mytest$id ==ident][!is.na(mytest$volume[mytest$action =="buy" & mytest$id ==ident])], pch =19, col ="blue")
  points(which(mytest$action =="sell"& mytest$id ==ident), mytest$volume[mytest$action =="sell"& mytest$id ==ident][!is.na(mytest$volume[mytest$action =="sell"& mytest$id ==ident])], pch =19, col ="black")

  
  mytest <- myresult
  print(i)
}
