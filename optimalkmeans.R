setwd("~/Documents/UNI/uni 2018 semester 2/High dim data analysis/A2") 
df=read.csv('Transport_Final.csv')
library(factoextra)
library(NbClust)
library(magrittr)
library(dplyr)
df=df[,-1]
df=df[,-13]
df=df[,c(-1:-2)]
df=df[,-1]
df=df[,-2]
df=df[,-4]
df=df[1:20,]
df=df[sample(nrow(df)),]

set.seed(7)
scale(df)
  
opti=fviz_nbclust(df, kmeans, method = "wss") 
opti+
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Elbow method")
