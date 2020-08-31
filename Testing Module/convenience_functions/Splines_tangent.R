library(gridExtra)
library(ggplot2)
# Data frame is candles
glimpse(candles)

# create data table
df <- data.table(y=candles$close, x=1:nrow(candles))

# Create spline
smoothingSpline = smooth.spline(df[,y] ~ df[,x] , spar = 0.80)
df[, spline := predict(smoothingSpline)$y]
df[, derivs := predict(smoothingSpline, deriv = 1)$y]
df[, sign_derivs := c(sign(df$derivs))]
df[, change_sign := c(0, diff(sign(df$derivs)))]
df[, points_y:= ifelse(change_sign == 2, spline, NA)]
df[, points_x:= ifelse(change_sign == 2, x, NA)]

df_segment <- na.omit(df[, c("points_y", "points_x")])

p1 <- ggplot(data= df, aes(x=x, y=y)) +
            geom_point(alpha = 0.1) +
            geom_line(aes(x = x, y = spline), color ="red") +
            geom_point(data = df_segment, aes(x=points_x, y=points_y),
                       color ="green", size = 3) 
p2 <- ggplot(data= df, aes(x=x, y = derivs)) +
  geom_point(color ="red", size = 0.1)
grid.arrange(p1, p2, nrow =2)



