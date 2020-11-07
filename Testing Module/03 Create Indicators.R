# # Testing strategies ------------------------------------------------------------
paraller_exec <- FALSE

# Badget 
initial_budget <- 500

# select period of data 
candles_recent <- as.data.table(klines[[1]])
# 
# training data here is not the traditional training set, but more of a 
# placeholder initial dataset to calculate the indicators
train_n <- ceiling(nrow(candles_recent) / 1.2)
train_data <- candles_recent[1:train_n, ]
test_data <- candles_recent[(train_n + 1):nrow(candles_recent), ]

# myresult <- splines_fast_slow(spar_fast=0.5, spar_slow= 0.9, plot.it = T)
myresult <- splines_fast_slow_cross(spar_slow = 0.7,
                                    spar_fast = 0.4,
                                    takeprofit = 0.05,
                                    stoploss_ult = 0.02,
                                    plot.it = F)
# myresult <- Pure_RSI_Volume_Trailing(RSI_Period=10, RSI_below=30, EMA_volume=10,
#                                      takeprofit=0.025, stoploss_trail=0.025,stoploss_ult=0.025,
#                                      times_vol=0, plot.it=F)

calculate_profits(myresult)

myresult <- RSI_splines(RSI_Period = 10,
                                     RSI_lower = 40,
                                     RSI_upper= 75,
                                     EMA_volume = 10,
                                     times_vol= 1,
                                     spar = 0.3,
                                     plot.it = T,
                                     takeprofit = 0.05,
                                     stoploss_trail= 0.02,
                                     stoploss_ult= 0.02)
calculate_profits(myresult)


# Close last position
if(myresult$action[nrow(myresult)] == "keep") {
  myresult$action[nrow(myresult)] <- "sell"
  myresult$Price[nrow(myresult)] <- myresult$close[nrow(myresult)] * myresult$Units[nrow(myresult)]
}

myresult$x <- 1:nrow(myresult)

segment_buy <- myresult[action %in%c("buy"), ]
segment_sell <- myresult[action %in%c("sell"), ]

p1 <- ggplot(data= myresult, aes(x=x, y=close)) +
  geom_line(alpha = 0.5) +
  #geom_line(aes(x = x, y = spline_fast), color ="red")+
  #geom_line(aes(x = x, y = spline_slow), color ="green")+
  geom_point(data = segment_buy, aes(x=x, y=close),
             color ="green", size = 1) +
  geom_point(data = segment_sell, aes(x=x, y=close),
             color ="red", size = 1);p1 

df_red <- myresult[0:2500,] 
segment_buy <- df_red[action %in%c("buy"), ]
segment_sell <- df_red[action %in%c("sell"), ]

p1 <- ggplot(data= df_red, aes(x=x, y=close)) +
  geom_line(alpha = 0.5) +
  geom_point(data = segment_buy, aes(x=x, y=close),
             color ="green", size = 1) +
  geom_point(data = segment_sell, aes(x=x, y=close),
             color ="red", size = 1);p1 


# Plot each trade's indicators and price action (to be functioned)
mytest <- myresult
idents <- unique(mytest$id)[!is.na(unique(mytest$id))]
par(mfrow = c(2, 1))

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
  
  # abline(h = mytest$SL[mytest$action =="buy"& mytest$id ==ident][!is.na(mytest$SL[mytest$action =="buy"& mytest$id ==ident])], col = "green")
  # abline(h = mytest$RL[mytest$action =="sell"& mytest$id ==ident][!is.na(mytest$RL[mytest$action =="sell"& mytest$id ==ident])], col = "red")
  
  # abline(h = 40)
  # 
  plot(mytest$deriv, type ="l")
  abline(h = 0, col ="red")
  
  
  points(which(mytest$action =="buy" & mytest$id ==ident), mytest$deriv[mytest$action =="buy" & mytest$id ==ident][!is.na(mytest$deriv[mytest$action =="buy" & mytest$id ==ident])], pch =19, col ="green")
  points(which(mytest$action =="sell"& mytest$id ==ident), mytest$deriv[mytest$action =="sell"& mytest$id ==ident][!is.na(mytest$deriv[mytest$action =="sell"& mytest$id ==ident])], pch =19, col ="red")
    
  mytest <- myresult
  print(i)
}
