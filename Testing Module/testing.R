# # Testing strategies------------------------------------------------------------
paraller_exec <- FALSE

# Badget 
initial_budget <- 200

# select period of data 
candles_recent <- as.data.table(klines[[1]])
# 
# training data here is not the traditional training set, but more of a 
# placeholder initial dataset to calculate the indicators
train_n <- ceiling(nrow(candles_recent) / 5)

train_data <- candles_recent[1:train_n, ]
test_data <- candles_recent[(train_n + 1):nrow(candles_recent), ]
myresult <- my.bllgr(look.back=100,
                     times.sd= 2
                     ,takeprofit =0.03,
                     stoploss_ult=0.02,
                     plot.it=F)
calculate_profits(myresult)
my.bllgr <- function(look.back, times.sd,takeprofit,stoploss_ult,plot.it) {

  
  # Train and test datasets
  train_data[, c("x",
                 "Mean",
                 "Sd",
                 "Upper",
                 "Lower",
                 "exit_condition",
                 "tp",
                 "ult_sl",
                 "action",
                 "Units",
                 "Price",
                 "id") := list(NA, NA, NA, NA, NA, NA,
                               NA, NA, NA, NA, NA, NA) ]
  
  test_data[, c("x",
                "Mean",
                "Sd",
                "Upper",
                "Lower",
                "exit_condition",
                "tp",
                "ult_sl",
                "action",
                "Units",
                "Price",
                "id") := list(NA, NA, NA, NA, NA, NA,
                              NA, NA, NA, NA, NA, NA) ]
  
  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){
    
    fut <- rbind(train_data, test_data[i, ])
    fut$x <- 1:nrow(fut)
    
    fut[ ,Mean := runMean(close, n = look.back)]
    fut[ ,Sd := runSD(close, n = look.back)]
    fut[ ,Upper := Mean + times.sd*Sd]
    fut[ ,Lower := Mean - times.sd*Sd]
    
    
    
    if(plot.it == TRUE){
      
      plot_df <- tail(fut, 200)
      
      # sr <- SR_lines(fut, roll = 100, Ns = nrow(fut), n_sort = 15, pair = pair,plot.it = F)
      # fut <- tail(fut, 250)
      df_points_buy <- data.frame(x = na.omit(plot_df$x[plot_df$action =="buy"]),
                                  y = na.omit(plot_df$close[plot_df$action =="buy"]))
      df_points_sell <- data.frame(x = na.omit(plot_df$x[plot_df$action =="sell"]),
                                   y = na.omit(plot_df$close[plot_df$action == "sell"]))
      
      par(mfrow = c(1, 1))
      plot(plot_df$close, type ="l", main = paste0("profits = ", tail(na.omit(plot_df$Price), 1)))
      lines(plot_df$spline_slow, col ="blue")
      lines(plot_df$spline_fast, col ="red")
      
      points(rownames(plot_df)[plot_df$x %in% df_points_buy$x], df_points_buy$y, col ="green", pch = 19)
      points(rownames(plot_df)[plot_df$x %in% df_points_sell$x], df_points_sell$y, col ="red", pch = 19)
      # abline(h = sr$SL)
      # abline(h =sr$RL)
      # plot(plot_df$deriv_slow, type ="l", main = paste0("sign: ",
      #                                                   " sign deriv: ", plot_df$sign_derivs_slow[nrow(plot_df)], " deriv ", plot_df$deriv_slow[nrow(plot_df)]))
      # abline(h = 0, col = "red", lty = 5, lwd = 2)
      # 
      # plot(plot_df$deriv_fast, type ="l", main = paste0("sign: ",
      #                                                   " sign deriv: ", plot_df$sign_derivs_fast[nrow(plot_df)], " deriv ", plot_df$deriv_fast[nrow(plot_df)]))
      # abline(h = 0, col = "red", lty = 5, lwd = 2)
    }
    
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
    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl
    
    fut$exit_condition[nrow(fut)] <-  fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]
    
    # Deciding upon action -----------------------------------------------------
    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         (fut$close[nrow(fut)] < fut$Lower[nrow(fut)])) {
      
      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)] - (0.0026 * fut$Units[nrow(fut)] * fut$close[nrow(fut)])
      # fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
      
      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") & (
      fut$exit_condition[nrow(fut)] == TRUE  )) {
      
      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]  - (0.0026 * fut$Units[nrow(fut)] * fut$close[nrow(fut)])
      # fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]
      
      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   &
                (fut$exit_condition[nrow(fut)] == FALSE )) {
      
      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      
    } else {
      
      fut$action[nrow(fut)] <- "no action"
      
    }
    
    train_data <- fut
    # print(i)
    if(plot.it == TRUE){
      Sys.sleep(0.1)
      # flush.console()
    }
    # print(i)
  }
  return(train_data)
}
