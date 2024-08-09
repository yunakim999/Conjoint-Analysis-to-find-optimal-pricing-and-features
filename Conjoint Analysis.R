
############## determine partworth estimates

library(dplyr)
library(readxl)

### ranking vectors of each of team members
jihyun <- c(11,12,4,22,24,16,9,10,3,21,23,15,7,8,2,19,20,14,5,6,1,17,18,13)
ian <- c(16,21,4,22,24,15,7,9,3,14,23,10,13,17,2,19,20,12,5,8,1,11,18,6)
amber <- c(11,23,2,16,24,8,10,19,1,15,22,7,9,18,4,14,21,6,12,17,3,13,20,5)
yuna <- c(12,4,8,23,24,16,11,3,7,21,22,15,10,2,6,19,20,14,9,1,5,17,18,13)
colin <- c(12,20,2,18,24,10,11,19,1,17,23,8,6,16,4,22,20,8,5,15,3,21,19,7)

########## actual function ##############################

conjoint_analysis <- function(preferences) {
  
  ### estimate partworth coefficients
  
  # extract design specifics
  designmatrix <- read_excel("Design Matrix.xlsx")
  designmatrix <- data.frame(designmatrix[1:5])
  
  # append preferences
  matrix <- cbind(preferences, designmatrix)
  
  # run lm on preferences (y) and design specifics (x)
  model <- lm(preferences ~ ., data = matrix)
  
  # extract partworth in vector
  partworth_with_price <- model$coefficients
  
  # extract partworth without price and price separately
  partworth <- partworth_with_price[1:5]
  price <- partworth_with_price[6]

  ### design profiles for my design, competing brand 1 sony, and competing brand 2 sharp
  
  # intercept (base price), 75 inch screen, 85 inch screen, high resolution, sony/not sony
  mydesign <- c(1,0,1,1,0) # Change this part
  sony <- c(1,1,0,1,1)
  sharp <- c(1,0,1,1,0)

  ### calculate utility based on cross product of design profile with partworth coefficients summed with price delta * partworth
  
  # utility without price
  util_mydesign_wo_price <- sum(partworth * mydesign)
  util_sony_wo_price <- sum(partworth * sony)
  util_sharp_wo_price <- sum(partworth * sharp)
  
  # utility with price for competing brands
  util_sony <- util_sony_wo_price + price*(2500-2000)/(2500-2000)
  util_sharp <- util_sharp_wo_price + price*(2000-2000)/(2500-2000)
  attractivenesstotal <- exp(util_sony) + exp(util_sharp)
  
  ### determine market share for each price
  
  # product pricing to be considered 
  prices <-seq(1500,2800, by = 100)
  
  # cost of my design
  costs <- c(1000,500,1000,250,250)
  my_cost <- mydesign%*%costs
  
  ###  formula to calculate utility, attractiveness, market share, sales, netcost and profit
  
  # empty dataframe to store and display results
  results <- data.frame(Price = numeric(0), Attractiveness = numeric(0), MarketShare = numeric(0), Sales = numeric(0), Margin = numeric(0), Profit = numeric(0))
  
  # run through each price and calculate all metrics
  for (i in prices) {
    
    # calculate utility of mydesign based on different prices
    util_mydesign <- util_mydesign_wo_price + price*(i-2000)/(2500-2000)
    attractiveness <- exp(util_mydesign)
    marketshare <- attractiveness/(attractivenesstotal + attractiveness)
    sales <- floor(marketshare*100)
    margin <- i-my_cost
    profit <- margin*sales
    
    results <- rbind(results, c(i, attractiveness, marketshare, sales, margin, profit))

  }
  
  colnames(results) <- c("Price", "Attractiveness", "MarketShare", "Sales", "Margin", "Profit")
  
  # print dataframe with results
  print(results)
  
  # plot price v marketshare and price v profit 
  plot(results$Price, results$MarketShare, type = "b", col = "blue",
       xlab = "Price", ylab = "Market Share", main = "Market Share vs Price")
  
  plot(results$Price, results$Profit, type = "b", col = "red",
       xlab = "Price", ylab = "Profit", main = "Profit vs Price")
  
  # find the max price 
  max_profit_index <- which.max(results$Profit)
  optimal_price <- results$Price[max_profit_index]
  optimal_profit <- results$Profit[max_profit_index]
  
  # statement about optimal price
  cat("The optimal price is $", optimal_price, 
      "which results in a profit of $", optimal_profit, "\n")
  
}


#### print out the results
conjoint_analysis(jihyun)
conjoint_analysis(yuna)
conjoint_analysis(colin)
conjoint_analysis(ian)
conjoint_analysis(amber)

library(dplyr)
library(readxl)

ian <- c(16,21,4,22,24,15,7,9,3,14,23,10,13,17,2,19,20,12,5,8,1,11,18,6)

yuna <- c(12,4,8,23,24,16,11,3,7,21,22,15,10,2,6,19,20,14,9,1,5,17,18,13)

amber <- c(11,23,2,16,24,8,10,19,1,15,22,7,9,18,4,14,21,6,12,17,3,13,20,5)

jihyun <- c(11,12,4,22,24,16,9,10,3,21,23,15,7,8,2,19,20,14,5,6,1,17,18,13)

colin <- c(12,14,8,23,24,16,11,13,7,21,22,15,6,10,2,20,19,4,5,9,1,18,17,3)

partworth <- function(preferences) {
  designmatrix <- read_excel("Design Matrix.xlsx")
  designmatrix <- data.frame(designmatrix[1:5])
  matrix <- cbind(preferences, designmatrix)
  model <- lm(preferences ~ ., data = matrix)
  partworth_with_price <- model$coefficients
  return(partworth_with_price)
} 

partworth(ian)
partworth(yuna)
partworth(amber)
partworth(jihyun)
partworth(colin)

