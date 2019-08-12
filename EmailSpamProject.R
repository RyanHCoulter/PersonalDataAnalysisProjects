
  
#Training a Naive Bayes classifier to recognize whether an email is Spam or not. The data has been adapted from the Ling-Spam dataset (refer to: http://csmining.org/index.php/ling-spam-datasets.html for the original files). 


#Read in the file "email.csv" and assigned it to the variable "email.df". 
#Removed the document ID column, index 1, from the data frame email.df. 
#Made the class label column, Spam, a factor with labels "not spam" and "spam". 


email.df <- read.csv(file = "email.csv")
email.df <- email.df[,-1]
email.df$Spam <- factor(email.df$Spam, labels = c("not spam", "spam"))


#Made the training and testing sets by random sampling

set.seed(12345)
data.size<-nrow(email.df)
train.size<-0.80
train.row.nums<-sample(1:data.size, data.size*train.size, replace=FALSE)
train.data<-subset(email.df[train.row.nums,])
test.row.nums <- setdiff(1:data.size, train.row.nums)
test.data<-subset(email.df[test.row.nums,])


#Made the true.labels vector by assigning it to the first column from the test data as it is the label, Spam

true.labels <- test.data[,1]
test.data <- test.data[,-1]


#Loaded the e1071 library. 
#Fitted a naive bayes model to the training data set. Used Spam as the predicted variable and used all columns as predictors.
#Create a variable, pred.labels, and assigned to it a call to the predict function, passing in the model and the test data.

library (e1071)
email.model <- naiveBayes(Spam~., data = train.data)
pred.labels <- predict(email.model, test.data)


#Created and displayed a confusion matrix of predicted vs true labels.

conf.matrix <- table(pred.labels,true.labels)
conf.matrix

#There were 77 true negatives, 20 false positives, 95 true positives, and 0 false negatives. Overall, the program does a good job at classifying spam vs not spam, but have 20 false positives is not good because that is 20 important emails that and individual may not see. 


#Calculated and displayed the misclassification rate of this classifier.


num.incorrect.labels<-sum(conf.matrix[row(conf.matrix)!=col(conf.matrix)])
misc.rate <- num.incorrect.labels/length(pred.labels)
misc.rate

#The misclassification rate is 10.41667%  This model gets a large percentage corrct, but I would say it is a bad predictor because all of those misclassifications put important emails in the spam folder. 

# K-fold cross validation

#Conducted a K-fold cross validation analysis using a naive bayes classifier and the email data set. Used the misclassification rate as the model performance metric. Collected the misclassification rates from each fold, graphed and reported statistics on these ten results.

data.set <- email.df # allows modifications that will not affect email.df.
data.size <- nrow(data.set)
data.cols <- ncol(data.set)
num.folds <- 10


set.seed(12345)
data.set["fold"]<-floor(runif(data.size)*num.folds)+1
data.set$fold<-factor(data.set$fold)
misclassification.results <- c()


for(i in c(1:num.folds)){
  
  train<-data.set[(data.set$fold!=i), 1:(data.cols)]
  test<-data.set[(data.set$fold==i),1:(data.cols)]
  
  model <- naiveBayes(Spam ~ ., data = train) 
  
  pred.labels <- predict(model, test[,-1]) 
  true.labels <- test[,1]
  
  num.incorrect.labels<-sum(pred.labels!=true.labels)
  
  misc.rate <- num.incorrect.labels/length(pred.labels)
  
  misclassification.results<-c(misclassification.results, misc.rate)
}

summary(misclassification.results)
sd(misclassification.results)


plot(misclassification.results, xlab="fold", ylab="misclassification rate")
lines(misclassification.results)


#The classifier has a volatile prediciting ability, it seems to bounce around between 6 and 12 percent misclassification rate. Some of the time it is pretty good while other times it can make a significant amount of mistakes. 


#Combined a histogram and a density curve of the misclassification rates. 

hist(misclassification.results, prob = TRUE, col = "grey")
lines(density(misclassification.results), lty = 3)

#The 10-fold cross validication resulted in a mean very similar to the original test above, but allowed the viewer to see that it is not a constant value. There is a large amount of variation between sets and it not a constant 10% error. 

