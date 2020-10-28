Dataset <- https://datahack.analyticsvidhya.com/contest/practice-problem-big-mart-sales-iii/
train <- read.csv(file.choose() , na.strings = '')
test <- read.csv(file.choose(), na.strings = '')
summary(train)
summary(test)
# Removing NAs
library(VIM)
train <- kNN(train , k = 10)
test <- kNN(test , k = 10)

train <- subset(train , select = Item_Identifier:Item_Outlet_Sales)
test <- subset(test , select = Item_Identifier:Outlet_Type)
summary(train)
summary(test)

library(dplyr)
data <- bind_rows(train ,  test)
#Outliers
boxplot(data$Item_Visibility) #Contain outliers
summary(data$Item_Visibility)
bench <- 0.09404 + 1.5*IQR(data$Item_Visibility)
data$Item_Visibility[data$Item_Visibility > bench] <- bench
boxplot(data$Item_Visibility)

#----------------
data$Item_Type <- as.character(data$Item_Type)
data$Item_Type[data$Item_Type == "Hard Drinks" | data$Item_Type ==  "Soft Drinks"] <- "Drinks"
data$Item_Type[data$Item_Type == "Baking Goods" | data$Item_Type ==  "Breads" | data$Item_Type == "Canned" | data$Item_Type == "Dairy"  | data$Item_Type == "Household"] <- "Household" 
data$Item_Type[data$Item_Type == "Breakfast" | data$Item_Type ==  "Meat" | data$Item_Type == "Seafood" | data$Item_Type == "Starchy Foods"  | data$Item_Type == "Others"] <- "Others" 
data$Item_Type <- as.factor(data$Item_Type)


# ----------------
train <- data[complete.cases(data),]
test <- data[!complete.cases(data),]
test <- test[,1:11]
# ----------------
library(caTools)
split <- sample.split(train$Item_Outlet_Sales , SplitRatio = 0.8)
training <- subset(train , split == T)
testing  <- subset(train , split == F)

model_lm <- lm(Item_Outlet_Sales ~ . - Item_Identifier , data = training)
summary(model_lm)

# ---------------
library(randomForest)
model_rf1 <- randomForest(Item_Outlet_Sales ~ . - Item_Identifier , data = training , tree = 500)
plot(model_rf)
pred_rf1 <- predict(model_rf1 , testing[,-12])
error <- testing$Item_Outlet_Sales - pred_rf1
sum(error)
library(Metrics)
mse(pred_rf1 , testing$Item_Outlet_Sales)
# ---------
model_rf <- randomForest(Item_Outlet_Sales ~ . - Item_Identifier , data = train , tree = 500)
plot(model_rf)
pred_rf <- predict(model_rf , test)
pred_rf <- data.frame(pred_rf)
write.csv(pred_rf , 'result.csv') #Actual Submission
