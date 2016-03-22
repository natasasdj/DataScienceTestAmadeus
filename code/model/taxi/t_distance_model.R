rm(list=ls())
library('geosphere')
library(ggplot2)
file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/taxi10_01pp_cb.rdata"
load(file)
str(taxi10_01pp_cb)
#taxiOct01modelDist<-data.frame(trip_distance_m=taxi10_01pp_cb$trip_distance_m)
taxiOct01modelDist<-taxi10_01pp_cb[,c(1,5:9)]

library("Imap")
d<-dim(taxi10_01pp_cb)[1]
dist<-rep(0,d)
for (i in 1:d){
        cat("itearation ",i," ")
        dist[i]<-gdist(taxi10_01pp_cb$pickup_longitude[i],taxi10_01pp_cb$pickup_latitude[i],
              taxi10_01pp_cb$dropoff_longitude[i],taxi10_01pp_cb$dropoff_latitude[i],
              units="m")
}

taxiOct01modelDist$distance<-dist

#plot(taxiOct01modelDist$distance,taxiOct01modelDist$trip_distance)
str(taxiOct01modelDist)

file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/taxiOct01modelDist.rdata"
save("taxiOct01modelDist",file=file)
load(file)


library(ggplot2)
file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t_dist_tripdist1b.png"
png(file)
ggplot(taxiOct01modelDist, aes(distance,trip_distance_m)) + 
        geom_point(size=0.01) + geom_smooth(size=1.2)
dev.off()


file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t_dist_tripdist2b.png"
png(file)
ggplot(taxiOct01modelDist, aes(distance,trip_distance_m)) + 
        geom_point(size=0.01) + geom_smooth(size=1.2) + coord_cartesian(ylim=c(0,20000))
dev.off()

taxiOct01modelDist$trip_time_in_secs <- taxi10_01pp_cb$trip_time_in_secs

file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t_dist_triptime1b.png"
png(file)
ggplot(taxiOct01modelDist, aes(distance,trip_time_in_secs)) + 
        geom_point(size=0.01) + geom_smooth(size=1.2) #+ coord_cartesian(ylim=c(0,20000))
dev.off()

file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t_dist_triptime2b.png"
png(file)
ggplot(taxiOct01modelDist, aes(distance,trip_time_in_secs)) + 
        geom_point(size=0.01) + geom_smooth(size=1.2) + coord_cartesian(ylim=c(0,5000))
dev.off()

file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/taxiOct01modelDist.rdata"
save("taxiOct01modelDist",file=file)

library(rpart)
library(caret)

taxiOct01modelDist2<-taxiOct01modelDist[,1:5]
taxiOct01modelDist2<-taxiOct01modelDist[30001:130000,1:5]
head(taxiOct01modelDist2)
# regression tree
rpartTree<-rpart(trip_distance_m ~ ., data=taxiOct01modelDist2)
rpartTree<-rpart(trip_distance_m ~ .,data=taxiOct01modelDist2, control = rpart.control(cp = 0.005,minsplit=100,maxdepth=30))
# no good for this

# knn model
knnregFit<-knnreg(taxiOct01modelDist2[,1:4], taxiOct01modelDist2$trip_distance_m, k = 3)
knnPred<-predict(knnregFit,newdata=taxiOct01modelDist2[,1:4])
head(knnPred)
plot(taxiOct01modelDist2$trip_distance_m,knnPred,ylim=c(0,12000),xlim=c(0,12000))
# not very good

# support vector machines

library(kernlab)
xtrain<- as.matrix(taxiOct01modelDist2[,1:4])
ytrain<- taxiOct01modelDist2$trip_distance_m
svmFit<-ksvm(x=xtrain, y=ytrain,
             kernel="rbfdot",kpar="automatic",C=1,epsilon=0.1)
svmPred <- predict(svmFit,newdata=taxiOct01modelDist2[,1:4])
plot(ytrain,svmPred)

file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t_tripDist_tree.png"
png(file)
plot(taxiOct01modelDist$trip_distance_m,rpartTreePred)
ggplot(aes(taxiOct01modelDist$trip_distance,rpartTreePred)) + 
        geom_point(size=0.01) + geom_smooth(size=1.2) #+ coord_cartesian(ylim=c(0,5000))
dev.off()

library(ipred)
beggedTree<-begging(y~.,data=taxiOct01modelDist2)