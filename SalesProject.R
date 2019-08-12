
library(tree)
library(randomForest)
library(gbm) 

#Carseat Sales data set. 

#Created test and train sets that are 50% of the data each. 
#Also created a variable that is assigned to the true Sales values, true.vals, from the test set.

seed.val<-12345
set.seed(seed.val)
carseat.df <- read.csv("/Users/ryancoulter/Desktop/Data for R/carseat_sales_reg.csv")
carseat.df <- carseat.df[,-12]
train.rows<-sample(1:nrow(carseat.df), 200)
train.data <- carseat.df[train.rows,]
test.data<-carseat.df[-train.rows,]
true.labels<-carseat.df$Sales[-train.rows]


# Single tree models

carseat.tree <- tree(Sales ~. , data = train.data )
plot(carseat.tree)
text(carseat.tree)

#Shelf Location is best predictor, left: price, shelf location, income, age, comp price, eduaction, advertising right: price, education, comp price


#Looked for a smaller tree size by plotting the deviation vs tree size. 

set.seed(seed.val)
cv.carseat<-cv.tree(carseat.tree)
plot(cv.carseat$size, cv.carseat$dev, type="b")


#Generated a pruned tree by picking the size in the range of [5,10] with the lowest deviance from the plot above. 

carseat.prunetree <- prune.tree(carseat.tree, best = 6)
plot(carseat.prunetree)
text(carseat.prunetree)


#Most important predictors: Shelf location and price


#Made predictions and calculated mean squared error MSE to evaluate the unpruned and pruned tree models. 

pred.unpruned <- predict(carseat.tree, test.data)
pred.pruned <- predict(carseat.prunetree, test.data)

mse.unpruned <- mean((pred.unpruned-true.labels)^2)
mse.pruned <- mean((pred.pruned-true.labels)^2)

mse.unpruned
mse.pruned

#The unpruned had an mse of 6.1 and the pruned had an mse of 4.9, these are expected because a more pruned tree would be less overfitted to the data and would provide more accurate predictions to new values. 





#Bagging, Boosting and Random Forest.

#Called the "randomForest" function to create a bagged ensemble on the training data. 
#Generated predictions on the test data and displayed the mean squared error MSE.

set.seed(seed.val)
bagged <- randomForest(Sales~., data=train.data, mtry = 6, importance = TRUE)
pred.bagged <- predict(bagged, test.data)
mse.bagged <- mean((pred.bagged-true.labels)^2)
mse.bagged


#Used the "randomForest" function to create a random forest ensemble on the training data. 
#Generated predictions on the test data and display the mean squared error MSE. 

set.seed(seed.val)
bagged <- randomForest(Sales~., data=train.data, mtry = 6, importance = TRUE)
pred.bagged <- predict(bagged, test.data)
mse.bagged <- mean((pred.bagged-true.labels)^2)
mse.bagged


#Used the "importance" function to plot the predictors vs their MSE for the random forest model. 


plot(importance(bagged))


#Five most important predictors: ShelveLoc, price, compprice, age, advertising

#Used the gbm function to create a boosted model on the training data. 
#Generated predictions on the test data and displayed the mean squared error MSE.

set.seed(seed.val)
gbm <- gbm(Sales~., data=train.data, n.trees = 5000, interaction.depth = 4)
pred.gbm <- predict(gbm, test.data, n.trees = 5000)
mse.gbm <- mean((pred.gbm-true.labels)^2)
mse.gbm

# Summary of models and MSE:
# single tree: 6.05
# pruned tree: 4.92
# bagged model:2.48
# rand forest: 2.48
# boosted model: 2.09
# 
# These results make sense because as you move through the methods, the number of nodes decreases which elimates overfitting while also not underfitting the data which allows for the strongest predicitve power and lowest error. 