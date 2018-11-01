setwd("~/Documents/UNI/uni 2018 semester 2/High dim data analysis/A2") 
df=read.csv('Transport_Final.csv')
library(clue)
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggmap)
df=df[,-1]
df=df[,-13]
df=df[sample(nrow(df)),]
df$ALLVEH_CALC=as.numeric(df$ALLVEH_CALC)
df$TRUCK_CALC=as.numeric(df$TRUCK_CALC)
df$FLOW=as.factor(df$FLOW)
df=scale(df)
training=df[1:3000,]
test=df[4001:7000,]
sum(is.na(df))

set.seed(7)

#3 clusters
c3=kmeans(dist(training),3,nstart=5)
c3m=as.factor(c3$cluster)
g3=ggplot(data=training,aes(x=training$ALLVEHS_AADT, y=training$GROWTH_RATE,color=c3m)) + 
  geom_point()+
  labs(x="Annual Volume of All Vehicles",y="Logarithmic Annual Growth Rate of Volume",title="Annual Volume of All Vehicles v. Annual Growth",colour="Clusters")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
g3+facet_wrap(training$FLOW~.)

g3.1=ggplot(data=training,aes(x=training$TRUCKS_AADT, y=training$GROWTH_RATE,color=c3m)) + 
  geom_point()+
  labs(x="Annual Volume of Trucks",y="Logarithmic Annual Growth Rate of Volume",title="Annual Volume of Trucks v. Annual Growth",colour="Clusters")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
g3.1+facet_wrap(training$FLOW~.)



#================predicting
#Cluster membership (training)
cluster_memb<-c3$cluster
table(cluster_memb)

#cl_predict from the package clue
test$ALLVEH_CALC=as.numeric(test$ALLVEH_CALC)
test$TRUCK_CALC=as.numeric(test$TRUCK_CALC)
test$FLOW=as.factor(test$FLOW)
predicted_cluster_memb<-cl_predict(c3,test)
#Look at the crosstab
predicted_cluster_memb
test
tab<-table(predicted_cluster_memb,cluster_memb)
adjustedRandIndex(predicted_cluster_memb,cluster_memb)
tab
