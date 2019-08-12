
# Examine and prepare the data.
  
#The data file, CourseAttendance.csv, contains data about students in an introductory programming course. This analysis is to explore these data to see if there are any "natural" groupings. If there are groupings, what kind of attendance profiles, lecture and discussion, do we see in the groups, and how do the groups compare in terms of final exam scores. 

# The data set contains the following variables:
#   
#   CLASS_LVL : numeric, year in school
# GENDER    : numeric, 1=female, 2=male
# MAJOR     : numeric, twelve various undergrad majors
# PREV      : numeric, level of previous programming experience
# LECATTEND : numeric, lecture attendence, 1=never, 2= sometimes, 3=always
# DISCATTEND: numeric, discussion attendence, 1=never, 2= sometimes, 3=always
# FINAL     : numeric, final exam score
# FINALQ    : numeric, final exam score quartile, 1=lowest, 4=highest


  
#Read in the data from the file assigning it to a variable called "data.df". 
#Changed the FINALQ column from numeric to factor, with levels "1st", "2nd", "3rd", "4th".
#Created a subset of the data frame for clustering called subset.df which does not include the FINAL and FINALQ columns (that is, columns 1 to 6).
#Created the variable "exam.labels" and assigned to it the FINALQ column in the data frame data.df. Created the variable "exam.scores" and assigned to it the FINAL column.

data.df <- read.csv(file = "CourseAttendance.csv")
data.df$FINALQ<- factor(data.df$FINALQ)
subset.df <- subset(data.df[,1:6])
exam.labels <- data.df$FINALQ
exam.scores <- data.df$FINAL


#Clustering with K-means

#Created a plot of within sum of squares.

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}


#Generated a wss plot on the subset.df data set.  

wssplot(subset.df)

#Used the NbClust function to determine the optimal number of clusters.

library(NbClust)
nc <- NbClust(subset.df,min.nc = 2,max.nc = 10,method = "kmeans")
  
#The graph returns 3 as the optimal number of clusters

#Created a variable "cluster.assignments" and assigned to it the vector of cluster assignments made by the NbClust function 

cluster.assignments <- c(nc$Best.partition)


#Evaluated clusters with respect to final exam, attendance

#Examined the proportion of lecture attendance for each cluster. Created a contingency table called "cont.table". 
#Then displayed the proportions of lecture attendance across the clusters, and the proportions within each cluster. 
#Ran a chi-square test on the table for dependencies.  

cont.table <- table(data.df$LECATTEND,cluster.assignments)
prop.table(cont.table)
chisq.test(cont.table)
cont.table

# It appears as though students who always attend the lecture make up the most of cluster 1. Students who sometimes and never attend lecture make up more of cluster 3 than students who always go to lecture. There seems to be a slight relationship between cluster assignment and the frequency a student shows up in the lecture, which is also refelected by the pvalue of .0001727.


#Examined the proportion of discussion attendance for each cluster. 
#Created a contingency table called "cont.table". 
#Ran a chi-square test on the table for dependencies. 

cont.table <- table(data.df$DISCATTEND,cluster.assignments)
cont.table
prop.table(cont.table)
chisq.test(cont.table)

#For each frequency of dicussion attendence, the order of most populated clusters goes 1, 3, then 2. The most represented is students that always attend lectures in cluster 1 at .288. Overall, the relation between discussion attendence and cluster assigment seems less powerful than that of lecture atendnece which is also backed up by the p value of .006958.

#Examined the proportion of final exam quartiles for each cluster. 
#Briefly summarized the proportions of the lowest and highest exam quartiles across the clusters. 


cont.table <- table(exam.labels,cluster.assignments)
cont.table
prop.table(cont.table)
chisq.test(cont.table)

#Across the clusters, 1 has the most from each exam score, 2 has the fewest, and 3 is in the middle. Each exam label seems equally represented in nearly every cluster and the pvalue also increased to .01035 which suggests that the relationship is not very powerful at predicting. 

#Graphed the average final exam scores for each cluster. 

#Used the "tapply" function to apply the "mean" function to the exam scores, grouped by cluster.


exam.mean <- tapply(exam.scores, cluster.assignments, mean)

#Used the "barplot" function to plot the means for each cluster (the "exam.mean" vector). 

barplot(exam.mean, main = "Mean Final Exam Scores per Cluster", xlab = "cluster", ylab = "mean score")

#In general, the clustering seemed to fit with a reltively high accuracy. Most data would end up in cluster 1, then cluster 3, with a little bit in cluster 2. Lecture and cluster attendance had the most notable relationship with lecture attendance provviding a slightly better relationship. Final exam score and cluster did not provide the best relationship and left each cluster with an almost equal amount from each score label (1-4).


#Model-based Clustering

#Used the Mclust function to take a model-based approach to these data. 

#Ran the Mclust function on the subset.df dataset and assigned it to a variable called "model.cluster". 
#Plotted the models and their BIC scores. 

library(mclust)
model.cluster<- Mclust(subset.df)
summary(model.cluster)
plot(model.cluster, data=subset.df, what="BIC")

#This code will create the file "MYPLOT" in your working directory (you can give another path if you wish, and name the file what you wish). The call to dev.off returns the output to the normal setting.

jpeg("MYPLOT.jpg")
plot(your parameters)
dev.off()



#Assigned to a variable "cluster.assignments" the cluster assignments from the Mclust object. 

cluster.assignments <- model.cluster$classification
vector1 <- tapply(exam.scores,cluster.assignments, mean)
vector1


# Created a contingency table 
#Used the prop.table function, displayed the row and column proportions.
#Ran a chi-square test on the table.


cont.table <- table(data.df$LECATTEND, cluster.assignments)
prop.table(cont.table)
chisq.test(cont.table)

#The BIC plot shows that the best number of clusters would most liekly be 3 and the variable equal variable (VEV) model is the best but are all relatively close. Mean exam scores shows that the second cluster tends to have scores higher than that of cluster 1. Being in cluster 2 has a high representation of  members in the 3 category for mean. 


#Called Mclust using subset.df, but specified 3 clusters by passing in the parameter G=3. 
#Printed the summary of the Mclust object and plot the BIC score 

model.cluster<- Mclust(subset.df, G = 3)
summary(model.cluster)
plot(model.cluster, data=subset.df, what="BIC")


#Assigned to a variable "cluster.assignments" the cluster assignments from the Mclust object. 
#Applied tapply

cluster.assignments <- model.cluster$classification
vector2 <- tapply(exam.scores,cluster.assignments, mean)
vector2


# Created a contingency table 
#Used the prop.table function, displayed the row and column proportions.
#Ran a chi-square test on the table.

cont.table <- table(data.df$LECATTEND, cluster.assignments)
prop.table(cont.table)
chisq.test(cont.table)

# A final exam score of three is most dominantly represented in cluster 3, while the other two are most represented in cluster two but stay close to eachother across all three clusters. This does agree with the kmeans from above, very closely in fact, 


