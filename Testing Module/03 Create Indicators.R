# # Testing strategies ------------------------------------------------------------
paraller_exec <- FALSE

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
                               EMA_volume = 15,
                               takeprofit = 1,
                               stoploss_trail = 0.05,
                               stoploss_ult = 2,
                               times_vol = 1)
# 
# myresult <- Volume_trading(EMA_volume = 20,
#                            takeprofit= 0.05,
#                            stoploss_trail = 0.01,
#                            stoploss_ult = 0.01,
#                            times_vol = 3,
#                            candle_action_long = "bullish")
st <- Sys.time()  
myresult <- Dynamic_SR_Lines(roll = 50,
                            n_sort = 10,
                            takeprofit = 1,
                            stoploss_trail = 1,
                            stoploss_ult = 1,
                            RSI_Period = 5,
                            RSI_below = 30)

end <- Sys.time()  
st - end
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

mytest <- myresult
idents <- unique(mytest$id)[!is.na(unique(mytest$id))]
par(mfrow = c(1, 1))

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
i <-87
indeces <- which(mytest$action =="buy")
for(i in 1:nrow(mytest)){
  plot(mytest$close[1:i], type ="l")
  
    points(indeces[indeces[i %in% indeces] <= i], mytest$close[indeces[indeces[i %in% indeces] <= i]], pch =19, col ="blue")
  
    flush.console()
  Sys.sleep(0.2)
print(i)  
}


lambda <- runif(10,min=0,max=3)
mean(lambda)

## First plot
N <- rpois(1,mean(lambda))
plot(1,mean(N), xlim = c(1,10))

## Subsequent points
for (i in 2:10){
  N <- rpois(i,mean(lambda))
  points(i,mean(N))
}


n=1000
df=data.frame(time=1:n,y=runif(n))
window=100
for(i in 1:(n-window)) {
  flush.console()
  plot(df$time,df$y,type='l',xlim=c(i,i+window))
  Sys.sleep(.09)
}

library(shiny)

runApp(list(
  ui = pageWithSidebar(    
    
    headerPanel("Hello Shiny!"),
    
    sidebarPanel(
      sliderInput("obs", 
                  "Number of observations:", 
                  min = 1,
                  max = 1000, 
                  value = 500)
    ),
    
    mainPanel(
      plotOutput("distPlot")
    )
  ),
  server =function(input, output, session) {
    autoInvalidate <- reactiveTimer(5000, session)
    output$distPlot <- renderPlot({
      autoInvalidate()
      # generate an rnorm distribution and plot it
      dist <- rnorm(input$obs)
      hist(dist)
    })
    
  }
))


xy <- data.frame(NAME=c("NAME1","NAME1","NAME2","NAME2"), X_START_YEAR=c(1984,1986,1899,1903), Y_START_VALUE=c(75,25,-90,-8),X_END_YEAR=c(1986,1994,1909,1924),Y_END_VALUE=c(20,50,-15,-70))
layout(matrix(c(1,1)))   ## don't use layout in your case
for (dat in split(xy,xy$NAME)){
  xx = unlist(dat[,grep('X_',colnames(dat))])
  yy = unlist(dat[,grep('Y_',colnames(dat))])
  plot(xx,yy,main=unique(dat[,1]),pch=20)
  dat <- dat[,-1]
  segments(dat[,1],dat[,2],dat[,3],dat[,4])
}
