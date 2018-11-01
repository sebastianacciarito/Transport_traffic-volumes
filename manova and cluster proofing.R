library(biotools)
library(MVN)

setwd("~/Documents/UNI/uni 2018 semester 2/High dim data analysis/A2") 
training=read.csv('Training_data_with_clusters.csv')

#MANOVA
x=training$twoclusters
y=as.matrix(training[,2:7])  
manova(y~x)%>%
  summary(test="Roy")%>%
  print

#Homogeneity
boxM(y,x)

#normality
manova(y~x)->mm
mvn(mm$residuals)$multivariateNormality

#pialli the one we use
manova(y~x)%>%
  summary(test="Pillai")%>%
  print
