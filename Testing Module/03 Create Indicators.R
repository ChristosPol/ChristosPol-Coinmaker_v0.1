# # Testing strategies ------------------------------------------------------------
paraller_exec <- FALSE

# Badget 
initial_budget <- 200

# select period of data 
candles_recent <- as.data.table(klines[[1]])
# 
# training data here is not the traditional training set, but more of a 
# placeholder initial dataset to calculate the indicators
train_n <- ceiling(nrow(candles_recent) / 10)

train_data <- candles_recent[1:train_n, ]

test_data <- candles_recent[(train_n + 1):nrow(candles_recent), ]


myresult <- Support_Resistance(takeprofit = 0.015, stoploss_trail = 1000,stoploss_ult = 0.02
                               ,plot.it= T,n_sort = 3, roll= 50)

myresult <- Volume_trading(EMA_volume = 10,
                           takeprofit = 0.02,
                           stoploss_trail = 1000,
                           stoploss_ult = 0.03, 
                           times_vol = 3, 
                           candle_action_long= "bullish",
                           rsi_period = 10,
                           plot.it = T)
  
myresult <- splines_fast_slow_cross(spar_fast=0.3,spar_slow=1, takeprofit=0.01,
                                    stoploss_ult=0.03,plot.it=T)
  
# Strategy using volumes spikes and RSI oversold conditions
myresult <- Pure_RSI_Volume_Trailing(RSI_Period=10, 
                         RSI_below=20, EMA_volume=10,
                                     takeprofit=0.02, stoploss_trail=10000,stoploss_ult=0.04,
                                     times_vol=0, plot.it=T)

myresult <- trend_obv(obv_ema=120, sma_ema=120, takeprofit=0.03,stoploss_ult=0.02, plot.it=TRUE)
View(myresult)
# myresult <- splines_fast_slow_cross_eff(spar_fast = 0.1, spar_slow = 0.6)
# df <- myresult
# df$returns <- c(lag(diff(df$close)), 0)
# df$actions <- ifelse(df$EMA_fast > df$EMA_slow, 1, 0)
# df$position <- lag(df$action)
# df$profits <- df$returns * df$position
# prof<- sum(df$profits[df$position == 1], na.rm = T)
# myresult <- splines_fast_slow(spar_fast=0.5, spar_slow= 0.9, plot.it = T)
# myresult <- splines_fast_slow_cross(spar_slow = 1,
#                                     spar_fast = 0.9,
#                                     takeprofit = 0.08,
#                                     stoploss_ult = 1000,
#                                     plot.it = F)

# myresult <- cross_EMA_stoploss(EMA_fast = 10,
#                                EMA_slow = 15,
#                                EMA_volume = 10,
#                                takeprofit = 0.2, 
#                                stoploss_ult = 1000, plot.it = F)

# myresult <- splines_fast_slow_cross(spar_fast = 0.5,
#                                     spar_slow = 0.8,
#                                     takeprofit = 0.05,
#                                     stoploss_ult = 0.02, 
#                                     plot.it = F)
  


# Volume_trading(EMA_volume = 30, takeprofit = 0.02, stoploss_trail = 0.01,stoploss_ult=0.01, times_vol=3, candle_action_long=
#                  "bullish")


myresult <- cross_EMA_stoploss_trail_simple(slow_SMA= 180,takeprofit=0.03, stoploss_trail=0.02,stoploss_ult=0.01)
  
  
calculate_profits(myresult)

# Close last position
if(myresult$action[nrow(myresult)] == "keep") {
  myresult$action[nrow(myresult)] <- "sell"
  myresult$Price[nrow(myresult)] <- myresult$close[nrow(myresult)] * myresult$Units[nrow(myresult)]
}

dfplot <- myresult[7500:10000,]
dfplot$x <- 1:nrow(dfplot)

segment_buy <- dfplot[action %in%c("buy"), ]
segment_sell <- dfplot[action %in%c("sell"), ]

p1 <- ggplot(data= dfplot, aes(x=full_date_time, y=close)) +
  geom_line(alpha = 0.5) +
  geom_point(data = segment_buy, aes(x=full_date_time, y=close),
             color ="green", size = 2) +
  geom_point(data = segment_sell, aes(x=full_date_time, y=close),
             color ="red", size = 2) +
  geom_line(aes(x=full_date_time, y=SMA), color ="red", size = 0.2)
  # geom_line(aes(x=full_date_time, y=Upper), color ="red", size = 0.2)+
  # geom_line(aes(x=full_date_time, y=Lower), color ="red", size = 0.2);p1
p1

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
