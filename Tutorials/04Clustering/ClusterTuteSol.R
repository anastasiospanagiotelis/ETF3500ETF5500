library(magrittr)
##Clear Workspace
rm(list=ls())

##Hierarchical Clustering

#Load the mtcars dataset

data(mtcars) 

#Since the variables are measured in different 
#units they should be standardised

CarsScaled<-scale(mtcars) 

#Evaluate the distance Matrix using Euclidean Distance
#Hierarchical Cluster analysis is based upon distances
#so we need to create the distance matrix

CarsDist<-dist(CarsScaled,method="euclidean")

#Run hierarchical clustering with Ward's method (squared distances)
#Create cluster object (Q1)

CarsClustWard<-hclust(CarsDist,method="ward.D2")

#We aren't interested in the cluster object above per se
#We want to get information from it though

#All of the above could also be done using pipes

mtcars%>%
  scale%>%
  dist%>%
  hclust(method='ward.D2')->
  CarsClustWard


#Produce Dendrogram (Q2)

plot(CarsClustWard) 

#plot boxes for clusters
#This can help you identify a "sensible" number of clusters from the dendrogram
#You need to specify how many clusters to highlight
#You may want to try different numbers, so you can do this step a few times
#It is best to re-produce the dendrogram each time so the boxes disappear if 
#you do decide to look at a few cluster solutions

rect.hclust(CarsClustWard,k=3)

#interactive boxes (Optional)
#These enable you to click on the clusters yourself
#Be sure to press the Esc key when you are done!

#identify(CarsClustWard)

#Did you press Esc????????

#Interpret Dendrogram (Q3)

#Once you have decided on some potential cluster solutions
#we need to assign clusters to observations

#Store Ward cluster membership for 2 cluster solution (Q4)

Ward_Memb<-cutree(CarsClustWard,2)  

#Repeat process for average linkage
#Cluster analysis is subjective, so it is a good idea to see 
#if your initial solution is stable
#So we repeat our analysis using another clustering algorithm
#and compare results

#Create Average Linkage cluster object 
CarsClustAL<-hclust(CarsDist,method="average")

#Look at the dendrogram

plot(CarsClustAL) 

#You can use the rectangles &/or identify if you wish

#Store Av Linkage 2-cluster membership (Q5(a))

AL_Memb<-cutree(CarsClustAL,2) 

#Repeat process for the centroid method
#Create Centroid cluster object 
CarsClustCent<-hclust(CarsDist,method="centroid")  

#Look at the dendrogram

plot(CarsClustCent) 

#Store Centroid 2-cluster membership (Q5(b))

Cent_Memb<-cutree(CarsClustCent,2) 

#We will get the complete linkage solution using pipes (Q5(c))

mtcars%>%scale%>%dist%>%hclust(method='complete')%>%cutree(k=2)->CL_Memb

##Compute adjusted Rand inidices

library(mclust)
adjustedRandIndex(Ward_Memb,AL_Memb)
adjustedRandIndex(Ward_Memb,Cent_Memb)
adjustedRandIndex(Ward_Memb,CL_Memb)


# Although it is not asked, we can also look at the Rand Index between AL Cent and CL

adjustedRandIndex(AL_Memb,Cent_Memb)
adjustedRandIndex(AL_Memb,CL_Memb)
adjustedRandIndex(Cent_Memb,CL_Memb)

# The implication is that centroid does not work for this data while Ward D2 and Complete Linkage give 
#the same solution.  Average Linkage gives a similar solution

##k means clustering

##Load the data
load('Wholesale.RData')



#kmeans clustering is better suited to large data sets
#We generally try to avoid using Hierarchical methods in these instances
#so we don't have another method to check our results.
#We will therefore randomly split our data into two parts (test and 
#train) to check our results

#Since we are randomly selecting a sample we need to include
#following command to make sure our results don't change each 
#time we run the code
set.seed(1)

#Install the clue package (if you haven't already done so)

library(clue)


#Pick the observations used for the test data.  Imagine that
#we are putting all the numbers from 1 to 440 in a hat and 
#picking out 220 numbers at random.  This is what the
#function "sample" does

test_observations<-sample(1:440,220)

#For the test sample, only select rows of Wholesale that are 
#included in the vector test_observations

Wholesale_test<-Wholesale[test_observations,] 

#For training sample, select the rows that are not included in 
#the vector test_observations.  This can by done by putting a minus (-)
#in front of test_observations

Wholesale_train<-Wholesale[-test_observations,]

#With kmeans, we decide on the number of clusters
#There is no dendrogram to interpret
#If you want to look at different cluster solutions, change the cluster
#number below

#number of clusters 

no_clust<-2 

#Fit kmeans using the test sample only

km_test<-kmeans(Wholesale_test,no_clust)

#Profiling
  #Cluster membership
  cluster_memb<-km_test$cluster

  #Find cluster centres
  cluster_cntr<-km_test$centers
  
  #Cluster size
  cluster_size<-km_test$size

#External Validation
#Fit kmeans using the training sample only

km_train<-kmeans(Wholesale_train,no_clust)

#Predict the membership of the test data using the function
#cl_predict from the package clue

predicted_cluster_memb<-cl_predict(km_train,Wholesale_test)

#Look at the crosstab

tab<-table(predicted_cluster_memb,cluster_memb)
adjustedRandIndex(predicted_cluster_memb,cluster_memb)
tab


