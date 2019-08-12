
#A KNN analysis on a cancer diagnostic data set
  

#Data: Wisconsin Diagnostic Breast Cancer (WDBC) data set. From the UC Irvine Machine Learning Repository, http://archive.ics.uci.edu/ml/index.html

#Read in the data from the file "wdbc_data.csv" and assigned it to the variable "df.wdbc".


df.wdbc <- read.csv(file = "wdbc_data.csv", header = TRUE)



#Data exploration and transformation.

#Set columns to be a factor with levels "benign" and "malignant". Called summary on the diagnosis column to see how many of each are in the data set.

df.wdbc$diagnosis<-factor(df.wdbc$diagnosis, levels = c("B","M"), labels = c("benign", "malignant"))
summary(df.wdbc$diagnosis)

# There are 357 benign and 212 malignant

#Applied a transformation to the data set to scale all values to the same range 0-1. 

normalize.it <- function(vec) {
y <- (vec - min(vec))/(max(vec) - min(vec))
y
}


#Created the normalized data set to use for the KNN classifier. Applied the "normalize.it" function to the dataset- but only to the columns 3 to 32 

#Used a call to lapply. 


wbcd.list <- lapply(df.wdbc[,3:32], normalize.it)
df.wbcd.normed <- data.frame(wbcd.list)


# Created train and test sets.

data.size <- nrow(df.wbcd.normed)
training.size <- .66
num.test.labels <- data.size*(1-training.size)


#Created the training set. 

set.seed(1234)
train.row.nums<-sample(1:data.size, data.size*training.size, replace=FALSE)
train.data<-subset(df.wbcd.normed[train.row.nums,])


#Created test data

test.row.nums<-setdiff(1:data.size,train.row.nums)
test.data<-subset(df.wbcd.normed[test.row.nums,])


#Set the training and test set labels. 

class.labels <- df.wdbc$diagnosis[train.row.nums]

true.labels <- df.wdbc$diagnosis[test.row.nums]


# Chose k, did modeling with knn, and classified 

k <- floor(sqrt(nrow(train.data)))


#Loaded the library"class"

library(class)

knn.pred <- knn(train.data,test.data,class.labels,k = k)



#Calculated the misclassification rate. 

num.incorrect.labels<- sum(knn.pred!=true.labels)
misc.rate <- (num.incorrect.labels/num.test.labels)*100
misc.rate


#Created a confusion matrix using the table function and assigned it to a variable called "conf.matrix". 

conf.matrix <- table(knn.pred,true.labels)


# A true positive: the model and true label = Malignant.
# A false positive: the model = Malignant and true label = Benign 
# A false negative: the model = Benign and true label = Malignant
# A true negative: the model and true label = Benign

#The percentage of false negatives is (false negatives / total)100 or (5/194)100 which is 2.58%. This is of special concern because if the predictor says it is benign when it is actually malignant, the patient will not get the immediate care he/she needs and could very likely lead to an preventable death. 


# Explored other values of k.

#Used a "for" loop to run knn on values of k from 1 to 20 
# called knn to get the predicted labels.
# calculated the misclassification rate.
# added the misclassification rate to the results vector.

result<-c()
for (k in 1:20) {
pred.labels<-knn(train.data, test.data, class.labels, k)
num.incorrect.labels<- sum(pred.labels!=true.labels)
misc.rate <- num.incorrect.labels/num.test.labels
result[k]<-misc.rate
}

#The lowest misclassification rate is 0.01550708 and the corresponding value of k is 5.











