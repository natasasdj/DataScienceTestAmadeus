# final svm fit of trip distance
file<-"C:/Users/natasa/Documents/projectAmadeus/code/model/taxi/fits/tOct01svmDistCombined3.rdata"
load(file)

# data for model of trip duration
file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/taxi10_01model.rdata"
load(file)

#
str(taxi10_01model)
file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/taxiOct01modelDist.rdata"
load(file)
file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/taxiOct01modelDistScut.rdata"
load(file)

str(taxiOct01modelDist)
str(taxiOct01modelDistScut)
taxiOct01<-taxi10_01model
taxiOct01$distance<-taxiOct01modelDist$distance

file<-"C:/Users/natasa/Documents/projectAmadeus/code/model/taxi/fits/tOct01DistCombinedSVMdata.rdata"
load(file)

library(kernlab)
distancePred <- as.vector(predict(svmFitFin,newdata=xtrain))
file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t_tripDistPredSVMfin3c.png"

#lmfit<-lm(taxiOct01modelDistScut$trip_distance_m~distancePred)
png(file)
plot(distancePred,taxiOct01modelDistScut$trip_distance_m,xlim=c(0,12000),ylim=c(0,12000),col="black",type="p",pch='.')
abline(a=0,b=1.2,col="red")
#abline(lmfit,col="blue")
#abline(a=lmfit$coeff[1]*1.2,b=lmfit$coeff[2]*1.2,col="yellow")
dev.off()

tail(order(taxiOct01modelDist$pickup_datetime))
taxi10_01model$distance<-taxiOct01modelDist$distance
taxiOct01model<-taxi10_01model[order(taxiOct01modelDist$pickup_datetime),]

str(taxiOct01model)
head(taxiOct01model)

cutInd <-which(taxiOct01modelDistScut$trip_distance_m > 1.2*distancePred & taxiOct01modelDistScut$trip_distance_m > 1.2*distancePred)
length(cutInd)
head(cutInd)
# [1] 34600
taxiOct01modelCut<-taxiOct01model[-cutInd,]


file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t_dist_tripdist_cut.png"
png(file)
par(mar=c(2,2,1,1),mfrow=c(2,1))
plot(taxiOct01model$distance,taxiOct01model$trip_distance_m,xlim=c(0,12000),ylim=c(0,12000),col="black",type="p",pch='.')
plot(taxiOct01modelCut$distance,taxiOct01modelCut$trip_distance_m,xlim=c(0,12000),ylim=c(0,12000),col="black",type="p",pch='.')
dev.off()


