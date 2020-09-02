library(gridExtra)
library(ggplot2)
# Data frame is candles
glimpse(candles)

# create data table
df <- data.table(y=candles$close, x=1:nrow(candles))

# Create spline
smoothingSpline = smooth.spline(df[,y] ~ df[,x] , spar = 0.60)
df[, spline := predict(smoothingSpline)$y]
df[, derivs := predict(smoothingSpline, deriv = 1)$y]
df[, sign_derivs := c(sign(df$derivs))]
df[, change_sign := c(0, diff(sign(df$derivs)))]
df[, points_y_buy:= ifelse(change_sign == 2, spline, NA)]
df[, points_x_buy:= ifelse(change_sign == 2, x, NA)]
df[, points_y_sell:= ifelse(change_sign == -2, spline, NA)]
df[, points_x_sell:= ifelse(change_sign == -2, x, NA)]

df_segment_buy <- na.omit(df[, c("points_y_buy", "points_x_buy")])
df_segment_sell <- na.omit(df[, c("points_y_sell", "points_x_sell")])

p1 <- ggplot(data= df, aes(x=x, y=y)) +
            geom_point(alpha = 0.1) +
            geom_line(aes(x = x, y = spline), color ="red") +
            geom_point(data = df_segment_buy, aes(x=points_x_buy, y=points_y_buy),
                       color ="green", size = 3) +
  geom_point(data = df_segment_sell, aes(x=points_x_sell, y=points_y_sell),
             color ="red", size = 3) 
p2 <- ggplot(data= df, aes(x=x, y = derivs)) +
  geom_point(color ="red", size = 0.1)

grid.arrange(p1, p2, nrow =2)



# # Testing strategies ------------------------------------------------------------
paraller_exec <- FALSE

# Badget 
initial_budget <- 500

# select period of data 
candles_recent <- candles
# 
# training data here is not the traditional training set, but more of a 
# placeholder initial dataset to calculate the indicators
train_n <- ceiling(nrow(candles_recent) / 100)
train_data <- candles_recent[1:train_n, ]

# Test, same 
test_data <- candles_recent[(train_n + 1):nrow(candles_recent), ]

myresult <- Splines_Tangent(spar = 0.5, 
                            takeprofit = 1,
                            stoploss_trail = 2,
                            stoploss_ult = 2)

View(myresult)
View(myresult[action %in% c("buy", "keep", "sell")])
spar <-0.6
# Strategy using volumes spikes and RSI oversold conditions
Splines_Tangent <- function(takeprofit, stoploss_trail,stoploss_ult, spar) {
  
  # Train and test datasets
  train_data[, c("spline",
                 "deriv",
                 "sign_derivs",
                 "change_sign",
                 "exit_condition",
                 "action",
                 "Units",
                 "Price",
                 "tp",
                 "ult_sl",
                 "trail_sl",
                 "id") := list(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]
  
  test_data[, c("spline",
                "deriv",
                "sign_derivs",
                "change_sign",
                "exit_condition",
                "action",
                "Units",
                "Price",
                "tp",
                "ult_sl",
                "trail_sl",
                "id") := list(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]
  
  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){
    
    fut <- rbind(train_data, test_data[i, ])
    
    # Calculate spline - derivative
    smoothingSpline = smooth.spline(fut[, close] ~ as.numeric(rownames(fut)) , spar = spar)
    fut[, spline := predict(smoothingSpline)$y]
    fut[, deriv := predict(smoothingSpline, deriv = 1)$y]
    
    # Sign of deriv - [-2 for desc, 2 for asc] 
    fut[, sign_derivs := c(sign(deriv))]
    fut[, change_sign := c(0, diff(sign(deriv)))]
    
    # par(mfrow =c(2,1))
    # plot(fut$close, type ="l")
    # lines(fut$spline, col ="red")
    # plot(fut$deriv, type ="l", main = paste0("sign: ", fut$change_sign[nrow(fut)], " sign deriv: ", fut$sign_derivs[nrow(fut)], " deriv ", fut$deriv[nrow(fut)]))
    # abline(h =0)
    # print(nrow(fut))

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
    if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){

      trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      if( trail_sl < tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)){
        trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)

      } else {
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

    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl
    fut$trail_sl[nrow(fut)] <- trail_sl


    fut$exit_condition[nrow(fut)] <- fut$trail_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]


    # Deciding upon action -----------------------------------------------------

    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         fut$deriv[nrow(fut)] > 0) {

      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))

      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") & (
      fut$exit_condition[nrow(fut)] == TRUE | fut$deriv[nrow(fut)] < 0 )) {

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
    # Sys.sleep(0.2)
    # flush.console()
  }
  return(train_data)
}
