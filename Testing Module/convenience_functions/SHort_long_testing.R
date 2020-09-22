# Badget 
initial_budget <- 500

# select period of data 
candles_recent <- candles
# 
# training data here is not the traditional training set, but more of a 
# placeholder initial dataset to calculate the indicators
train_n <- ceiling(nrow(candles_recent) / 150)
train_data <- candles_recent[1:train_n, ]
test_data <- candles_recent[(train_n + 1):nrow(candles_recent), ]


myresult <- Splines_Tangent_SL(takeprofit = 0.02, stoploss_trail = 1,
                               stoploss_ult = 1, spar = 0.95, plot.it = F, rsi_period = 14)

calculate_profits_LS(myresult)


plot_df <- myresult
plot_df[, points_y_enter_long:= ifelse(action %in% c("enter_long"), close, NA)]
plot_df[, points_x_enter_long:= ifelse(action %in% c("enter_long"), x, NA)]
plot_df[, points_y_exit_long:= ifelse(action %in% c("exit_long"), close, NA)]
plot_df[, points_x_exit_long:= ifelse(action %in% c("exit_long"), x, NA)]

plot_df[, points_y_enter_short:= ifelse(action %in% c("enter_short"), close, NA)]
plot_df[, points_x_enter_short:= ifelse(action %in% c("enter_short"), x, NA)]
plot_df[, points_y_exit_short:= ifelse(action %in% c("exit_short"), close, NA)]
plot_df[, points_x_exit_short:= ifelse(action %in% c("exit_short"), x, NA)]



df_segment_enter_long <- na.omit(plot_df[, c("points_y_enter_long", "points_x_enter_long")])
df_segment_exit_long <- na.omit(plot_df[, c("points_y_exit_long", "points_x_exit_long")])

df_segment_enter_short <- na.omit(plot_df[, c("points_y_enter_short", "points_x_enter_short")])
df_segment_exit_short <- na.omit(plot_df[, c("points_y_exit_short", "points_x_exit_short")])


p1 <- ggplot(data= plot_df, aes(x=x, y=close)) +
  geom_line(alpha = 0.5) +
  geom_point(data = df_segment_enter_long, aes(x=points_x_enter_long, y=points_y_enter_long),
             color ="green", size = 1) +
  geom_point(data = df_segment_exit_long, aes(x=points_x_exit_long, y=points_y_exit_long),
             color ="red", size = 1) +
  geom_point(data = df_segment_enter_short, aes(x=points_x_enter_short, y=points_y_enter_short),
             color ="blue", size = 1) +
  geom_point(data = df_segment_exit_short, aes(x=points_x_exit_short, y=points_y_exit_short),
             color ="black", size = 1)
  
p2 <- ggplot(data= df, aes(x=x, y = derivs)) +
  geom_point(color ="red", size = 0.1)

grid.arrange(p1, p2, nrow =2)





View(subset(myresult,myresult$action %in%c("enter_short", "exit_short", "enter_long", "exit_long") ))


calculate_profits()
dataset <- myresult
calcu <- dataset[action %in% c("enter_short", "exit_short"), ]
calcu <- dataset[action %in% c("enter_long", "exit_long"), ]
calcu <- subset(calcu,  !calcu$id %in% names(which(table(calcu$id) ==1)))
if (nrow(calcu) > 0) {
  
  profit <- c()
  profit_sum <- c()
  ids <- unique(calcu$id)
  for(i in 1:length(ids)){
    
    profit[i] <-   calcu$Price[calcu$action =="enter_long" & calcu$id == ids[i]] - calcu$Price[calcu$action =="exit_long" & calcu$id == ids[i]] 
  }
  profit_sum <- sum(profit)
  dd <- data.frame(profit = profit_sum, n_trades = length(unique(calcu$id)),
                   enter_date = unique(calcu$Date)[1], exit_date = tail(unique(calcu$Date), 1))
} else {
  
  dd <- data.frame(profit = 0, n_trades = 0, enter_date = as.Date("2020-04-07"), exit_date =as.Date("2020-04-07"))
}

