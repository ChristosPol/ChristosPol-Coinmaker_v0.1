# This would only with technical idnicators and not smoothers like splines
# rough idea for vectorized backtesting
df <- klines[[1]]

# Exhaustive search of best curves
EMA_fast <- data.frame(EMA_fast = seq(5, 300, 5), flag = 1)
EMA_slow <- data.frame(EMA_slow = seq(10, 200, 5), flag = 1)
testing_params <- left_join(EMA_fast, EMA_slow)
testing_params <- subset(testing_params,
                         testing_params$EMA_slow > testing_params$EMA_fast)
testing_params$flag <- NULL
testing_params <- as.data.table(testing_params)

ggplot(data= df, aes(x=full_date_time, y=close)) +
  geom_line(alpha = 0.5)
leverage <- 1
prof <- c()
for (i in 1:nrow(testing_params)){
  
  df$EMA_fast <- EMA(df$close, n = testing_params$EMA_fast[i])
  df$EMA_slow <- EMA(df$close, n = testing_params$EMA_slow[i])
  df$returns <- c(lag(diff(df$close)), 0)
  df$actions <- ifelse(df$EMA_fast > df$EMA_slow, leverage, 0)
  df$position <- lag(df$action)
  df$profits <- df$returns * df$position
  runs <- rle(df$actions)
  longs <- which(runs$values ==leverage)
  shorts <- which(runs$values == 0)
  cums <- cumsum(runs$lengths)
  starts <- cums[longs - 1] + 1
  ends <-  cums[shorts - 1] + 1
  df$signal <- NA
  df$signal[starts] <- 1
  df$signal[ends] <- 0
  
  prof[i] <- sum(df$profits[df$position == leverage], na.rm = T)
  
}
i <-which.max(prof)
max(prof)
segment_buy <- df[signal  == 1, ]
segment_sell <- df[signal == 0, ]

ggplot(data= df, aes(x=full_date_time, y=close)) +
  geom_line(alpha = 0.5)+
  geom_line(aes(x=full_date_time, y=EMA_fast), color ="red", size = 0.2)+
  geom_line(aes(x=full_date_time, y=EMA_slow), color ="green", size = 0.2)+
  geom_point(data = segment_buy, aes(x=full_date_time, y=close),
             color ="green", size = 1)+
  geom_point(data = segment_sell, aes(x=full_date_time, y=close),
             color ="red", size = 1)




df <- klines[[1]]
krn_idx <- 720
init_df <- df[1:krn_idx, ]
test_df <- df[(krn_idx + 1) : nrow(df), ]

# Exhaustive search of best curves
look.back <- data.frame(look.back = seq(5, 200, 5), flag = 1)
times.sd <- data.frame(times.sd = c(1,2, 3, 4,5), flag = 1)
testing_params <- left_join(look.back, times.sd)
testing_params$flag <- NULL
testing_params <- as.data.table(testing_params)

for (i in 1:nrow(testing_params)){
  
  df[ ,Mean := runMean(close, n = testing_params$look.back[i])]
  df[ ,Sd := runSD(close, n = testing_params$look.back[i])]
  df[ ,Upper := Mean + testing_params$times.sd[i]*Sd]
  df[ ,Lower := Mean - testing_params$times.sd[i]*Sd]
  df$returns <- c(lag(diff(df$close)), 0)
  df$actions <- NA
  df$actions[df$close < df$Lower] <- 1
  View(df)
  df$position <- lag(df$action)
  df$profits <- df$returns * df$position
  runs <- rle(df$actions)
  longs <- which(runs$values ==1)
  shorts <- which(runs$values == 0)
  cums <- cumsum(runs$lengths)
  # 
  starts <- cums[longs - 1] + 1
  ends <-  cums[shorts - 1] + 1
  # 
  df$signal <- NA
  df$signal[starts] <- 1
  df$signal[ends] <- 0
  
  
  print(sum(df$profits[df$position == 1], na.rm = T))
}



ggplot(data= df, aes(x=full_date_time, y=close)) +
  geom_line(alpha = 0.5)+
  geom_line(aes(x=full_date_time, y=Mean), color ="green", size = 0.2)+
  geom_line(aes(x=full_date_time, y=Upper), color ="red", size = 0.2)+
  geom_line(aes(x=full_date_time, y=Lower), color ="red", size = 0.2)


# df$actions[df$EMA_fast > df$EMA_slow] <- 1
# df$actions[df$EMA_fast < df$EMA_slow] <- "short"
# df$actions[is.na(df$actions)] <- "no_action"
# 
# segment_buy <- df[actions %in%c("long"), ]
# segment_sell <- df[actions %in%c("short"), ]
# 
# ggplot(data= df, aes(x=full_date_time, y=close)) +
#   geom_line(alpha = 0.5)+
#   # geom_line(aes(x=full_date_time, y=Mean), color ="green", size = 0.2)+
#   # geom_line(aes(x=full_date_time, y=Upper), color ="red", size = 0.2)+
#   # geom_line(aes(x=full_date_time, y=Lower), color ="red", size = 0.2)+
#   geom_line(aes(x=full_date_time, y=EMA_fast), color ="red", size = 0.2)+
#   geom_line(aes(x=full_date_time, y=EMA_slow), color ="red", size = 0.2)+
#   geom_point(data = segment_buy, aes(x=full_date_time, y=close),
#              color ="green", size = 1)+
#   geom_point(data = segment_sell, aes(x=full_date_time, y=close),
#              color ="red", size = 1)
# 
# 
# # df$actions[df$RSI < 20] <- "long"
# # df$actions[df$RSI > 90] <- "short"
# # df$actions[is.na(df$actions)] <- "no_action"


# 
# idx_enter <- which(df$signal =="long")
# 
# price_enter <- df$close[which(df$signal =="long")]
# returns <- mapply(function(x, y){ round((x - df$close[y : nrow(df)]) / x,3) },
#                   price_enter, idx_enter)
# 
# exits <- unlist(lapply(returns, function(x){which(x >0.05 | x < -0.03)[1] } ))
# 
# idx_exit <- idx_enter + exits
# idx_exit <- which(df$signal =="short")
# df$signal[idx_exit] <- "exit_long"
# 
# 
# df$trade_id <- NA
# for (i in 1:length(idx_exit)){
#   df$trade_id[idx_enter[i]:idx_exit[i]] <- round(runif(1, 10000, 5000000))
# }
# 
# calc <- df
# calc1 <- subset(calc, !is.na(calc$trade_id))
# ids <- unique(calc1$trade_id)
# 
# prof <- c()
# for(i in 1:length(ids)){
#   
#   prof[i] <- tail(calc1$close[calc1$signal == "short" & calc1$trade_id == ids[i]], 1) - head(calc1$close[calc1$signal == "long" & calc1$trade_id == ids[i]], 1)
#   
# }
# sum(prof)
# 
# segment_buy <- df[signal %in%c("long"), ]
# segment_exit <- df[signal %in%c("short"), ]
# 
# p1 <- ggplot(data= df, aes(x=full_date_time, y=close)) +
#   geom_line(alpha = 0.5) +
#   geom_line(aes(x = full_date_time, y = EMA_fast), color ="red")+
#   geom_line(aes(x = full_date_time, y = EMA_slow), color ="green")+
#   geom_point(data = segment_buy, aes(x=full_date_time, y=close),
#              color ="blue", size = 2)+
#   geom_point(data = segment_exit, aes(x=full_date_time, y=close),
#              color ="black", size = 2);p1 
# 
