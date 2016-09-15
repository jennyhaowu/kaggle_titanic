# Remove all variables
rm(list=ls())

## Step 1: import data
train_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/train.csv"
train <- read.csv(train_url)

test_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/test.csv"
test <- read.csv(test_url)

## Step 2: analyze data
table(train$Survived)
prop.table(table(train$Survived))

table(train$Sex, train$Survived)
prop.table(table(train$Sex, train$Survived))
prop.table(table(train$Sex, train$Survived),1) # 1 for rows
prop.table(table(train$Sex, train$Survived),2) # 2 for cols

## Step 3: explore adding new feature: child
train$Child <- 0
train$Child[train$Age < 18] <- 1
prop.table(table(train$Child, train$Survived),1)

## Step 4: simple prediction - female survived and male died
test_one <- test
test_one$Survived <- 0
test_one$Survived[test$Sex == "female"] <- 1

## Step 5: decision trees
library(rpart)
my_tree_two <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class")

# Visualize the decision tree
plot(my_tree_two)
text(my_tree_two)

# Plot fancy tree
library(rattle)
fancyRpartPlot(my_tree_two)

# Generate test result data
my_prediction <- predict(my_tree_two, data = test, type = "class")
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)

## Step 6: overfitting
super_model <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, 
                     data = train, method = "class", control = rpart.control(minsplit = 2, cp = 0))

plot(super_model)
text(super_model)

#============================================================
## Decision Tree Model: require datasets with no missing values
# Check missing values
all_data = rbind(train, test)
apply(all_data, 2, function(x) any(is.na(x)|is.infinite(x)))

