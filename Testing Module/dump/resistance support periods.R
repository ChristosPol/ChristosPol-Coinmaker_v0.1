# candles
# candles <- simple_OHLC(interval = 5, pair = "ADAEUR");df
# df$only_date <- substr(df$Date_POSIXct, 1, 10)
df1 <- candles[1:11000,]
df2 <- df1
# df2 <- subset(df1, !df1$only_date %in% names(table(df1$only_date))[c(1,7)])
plot_candlesticks(dta = df1, Ns = nrow(df1), asset = pair)

myts <- ts(df2$close)

air.model <- Arima(myts, order=c(2,2,1),lambda=0)
plot(forecast(air.model,h=1000))
lines(AirPassengers)

summary(wo(myts))
mytsdecomp <- decompose(myts)
plot(mytsdecomp)

mytsdecomp$seasonal

plot(myts)
mytsforecasts <- HoltWinters(myts, gamma = F)
plot(mytsforecasts)
library("forecast")
mytsforecasts2 <- forecast(mytsforecasts, h = dim(candles)[1]- nrow(df1))

par(mfrow=c(1,1))
# plot(mytsforecasts2)
plot(candles$close, type ="l", xlim  =c(8000, 13000))
lines(c(df2$close, mytsforecasts2$mean), col ="red")
abline(v =700)
# df <- simple_OHLC(interval = 5, pair = "EURUSD");df
# df <- simple_OHLC(interval = 5, pair = "BTCEUR");df

plot_candlesticks(dta = df, Ns = nrow(df), asset = pair)

abline( v = 600)


candles <- df



abline(h= mean(head(sort(candles[1:100,]$close),1 )),col ="red")
abline(h= mean(head(sort(candles[101:200,]$close),1 )),col ="red")
abline(h= mean(head(sort(candles[201:300,]$close),1 )),col ="red")
abline(h= mean(head(sort(candles[301:400,]$close),1 )),col ="red")
abline(h= mean(head(sort(candles[401:500,]$close),1 )),col ="red")
abline(h= mean(head(sort(candles[501:600,]$close),1 )),col ="red")
abline(h= mean(head(sort(candles[680:720,]$close),1 )),col ="red")

abline(h= mean(tail(sort(candles[1:100,]$close),1 )),col ="green")
abline(h= mean(tail(sort(candles[101:200,]$close),1 )),col ="green")
abline(h= mean(tail(sort(candles[201:300,]$close),1 )),col ="green")
abline(h= mean(tail(sort(candles[301:400,]$close),1 )),col ="green")
abline(h= mean(tail(sort(candles[401:500,]$close),1 )),col ="green")
abline(h= mean(tail(sort(candles[501:600,]$close),1 )),col ="green")
abline(h= mean(tail(sort(candles[680:720,]$close),1 )),col ="green")



# # Testing strategies ------------------------------------------------------------
# 
# # Badget  
initial_budget <- 500

# # select period of data 
candles_recent <- candles
# 
# # Plot if like 
# plot_candlesticks(dta = candles_recent, Ns = nrow(candles_recent), asset = pair)
# plot_candlesticks(dta = candles_recent, Ns = 100, asset = pair)
# 
# # N Training Data
train_n <- ceiling(nrow(candles_recent) / 20)
train_data <- candles_recent[1:train_n, ]
# 
# # Test, same 
test_data <- candles_recent[(train_n + 1):nrow(candles_recent), ]


RSI_Period <- 10
RSI_below <- 30
EMA_volume <- 5
takeprofit <-  0.03
stoploss_trail <- 0.02
stoploss_ult  <- 0.05
times_vol <- 1
df_tail <- 200
mean_n <-5

Pure_RSI_Volume_Trailing_Dynamic_SRL <- function(RSI_Period, RSI_below, EMA_volume, takeprofit, stoploss_trail,stoploss_ult, times_vol, df_tail, mean_n) {
  
  # Train and test datasets
  train_data[, c("RSI",
                 "EMA_volume",
                 "exit_value",
                 "exit_condition",
                 "support",
                 "resistance",
                 "crossover_SL_RS",
                 "crossover_volume",
                 "crossover_RSI",
                 "action",
                 "Units",
                 "Price",
                 "tp",
                 "ult_sl",
                 "trail_sl",
                 "id") := list(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]
  
  test_data[, c("RSI",
                "EMA_volume",
                "exit_value",
                "exit_condition",
                "support",
                "resistance",
                "crossover_SL_RS",
                "crossover_volume",
                "crossover_RSI",
                "action",
                "Units",
                "Price",
                "tp",
                "ult_sl",
                "trail_sl",
                "id") := list(NA, NA, NA,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]
  
  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)) {
    
    fut <- rbind(train_data, test_data[i, ])
    
    # RSI and Volume
    fut$RSI <- RSI(fut$close, n = RSI_Period)
    fut$EMA_volume <- EMA(fut$volume, n = EMA_volume)
    
    # Technical indicators -----------------------------------------------------
    
    
    # Volume Crossing of volume over the SMA(volume, n_periods)
    fut$crossover_volume[nrow(fut)] <- ifelse(fut$volume[nrow(fut)] > fut$EMA_volume[nrow(fut)] * times_vol ,
                                              "volume_higher", "volume_lower")
    
    
    # RSI Crossing of upper or lower bounds
    fut$crossover_RSI[nrow(fut)] <- ifelse(fut$RSI[nrow(fut)] < RSI_below ,
                                           "RSI_lower", "RSI_higher")
    
    # calculate resistance and support level
    fut$support[nrow(fut)] <- mean(tail(sort(tail(fut$close, df_tail)), mean_n))
    fut$resistance[nrow(fut)] <- mean(head(sort(tail(candles$close, df_tail)), mean_n))
    
    if( fut$close[nrow(fut)] < fut$support[nrow(fut)]) {
      
      fut$crossover_SL_RS[nrow(fut)] <- "Below_support"
      
    } else if (fut$close[nrow(fut)] > fut$resistance[nrow(fut)] ) {
      
      fut$crossover_SL_RS[nrow(fut)] <- "Above_resistance"
    }else{
      
      fut$crossover_SL_RS[nrow(fut)] <- "Above_resistance"
    }
    
    # abline( h =   fut$resistance[nrow(fut)])
    # abline( h = fut$support[nrow(fut)] )
 
    
    # Exit condition for takeprofit  - Fixed
    tp <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) + takeprofit * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)
    
    if (length(tp) == 0) {
      tp <- 0
    }
    
    # Ultimate stop loss
    ult_sl <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) - stoploss_ult * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)
    
    if (length(ult_sl) == 0) {
      ult_sl <- 0
    }
    
    

    # Trailing stop loss
    # browser()
    if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){
      
      trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      if( trail_sl < tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)){
        trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
        
      }else {
        trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
        
      }
      
      
    } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1] ) ){
      
      trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
    } else {
      
      trail_sl <-0
    }
    
    
    if(length(trail_sl) == 0 ){
      
      trail_sl <- 0
    }
    # browser()
    
    
    
    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl
    fut$trail_sl[nrow(fut)] <- trail_sl
    
    
    fut$exit_condition[nrow(fut)] <- fut$trail_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]
    
    
    # Deciding upon action -----------------------------------------------------
    
    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         fut$crossover_volume[nrow(fut)] == "volume_higher" &  fut$crossover_RSI[nrow(fut)] == "RSI_lower"  & fut$crossover_SL_RS[nrow(fut)] == "Below_support") {
      
      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
      
      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") & (
      fut$exit_condition[nrow(fut)] == TRUE  )) {
      
      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]
      
      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   & 
                fut$exit_condition[nrow(fut)] == FALSE  ) {
      
      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      
    } else {
      
      fut$action[nrow(fut)] <- "no action"
      
    }
    
    train_data <- fut
    print(i)
  }
  return(train_data)
}
