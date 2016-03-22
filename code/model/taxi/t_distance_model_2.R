file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/taxiOct01modelDist.rdata"
load(file)
file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/taxi10_01model.rdata"
load(file)
str(taxiOct01modelDist)

taxiOct01perhour<-list()
no_hour<-NULL
for (h in 0:23){
        ind<-h+1
        taxiOct01perhour[[ind]]<-taxi10_01model[floor(taxi10_01model$pickup_hour)==h,]
        d<-dim(taxiOct01perhour[[ind]])[1]
        no_hour<-c(no_hour,d)
        # names(no_hour)[h+1]<-h
}




file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t_no_hour.png"
png(file)
names(no_hour)<-0:23
barplot(no_hour)
dev.off()
#sum(no_hour[c(22:23)])

file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/taxiOct01perhour.rdata"
save("taxiOct01perhour",file=file)

taxiOct01modelDistS<-taxiOct01modelDist[order(taxiOct01modelDist$pickup_datetime),]
file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/taxiOct01modelDistS.rdata"
save("taxiOct01modelDistS",file=file)
load(file)
str(taxiOct01modelDistS)
head(taxiOct01modelDist)
head(taxiOct01modelDistS)
dim(taxiOct01modelDistS)[1]





library(kernlab)
group<-list()
svmFitList<-list()
k<-15 # no of chunks
d<-dim(taxiOct01modelDistS)[[1]] # no of observations
no <- floor(d/k) # no of observations per chunk
for (i in 1:k){
        cat(i,"\n")
        if (i==k) {ind<-((i-1)*no+1):d
                
        } else {ind<-((i-1)*no+1):(i*no)}
                
                
        xtrain<- as.matrix(taxiOct01modelDistS[ind,2:5])
        trip_distance<- taxiOct01modelDistS[ind,6]
        svmFit<-ksvm(x=xtrain, y=trip_distance,
                     kernel="rbfdot",kpar="automatic",C=1,epsilon=0.1)
        svmPred <- as.vector(predict(svmFit,newdata=taxiOct01modelDistS[ind,2:5]))
        svmFitList[[i]] <- svmFit
        group[[i]]<-cbind(svmPred,trip_distance)
}

file<-"C:/Users/natasa/Documents/projectAmadeus/code/model/taxi/fits/tOct01svmDist.rdata"
save("svmFitList","group",file=file)

xtrain<-NULL
ind<-1:d
for (i in 1:k){
        cat("prediction: chunk",i,"\n")
        svmFit <- svmFitList[[i]]
        svmPred <- as.vector(predict(svmFit,newdata=taxiOct01modelDistS[ind,2:5]))
        xtrain<-cbind(xtrain,svmPred)
}
trip_distance <- taxiOct01modelDistS[ind,6]

file<-"C:/Users/natasa/Documents/projectAmadeus/code/model/taxi/fits/tOct01DistCombinedSVMdata.rdata"
save("xtrain","trip_distance",file=file)
load(file)
# svmFitFin<-ksvm(x=xtrain, y=trip_distance,
#              kernel="vanilladot",kpar="automatic",C=1,epsilon=0.1)
# sample data because too big
ind<-seq(1,d,5)
ind<-1:60000
svmFitFin<-ksvm(x=xtrain[ind,], y=trip_distance[ind],
                kernel="vanilladot",kpar="automatic",C=1,epsilon=0.1)
outcomeFin <- trip_distance[ind] 
svmPredFin <- as.vector(predict(svmFitFin,newdata=xtrain[ind,]))

# n<-60000, 100000, all
file<-"C:/Users/natasa/Documents/projectAmadeus/code/model/taxi/fits/tOct01svmDistCombined2.rdata"
save("svmFitFin","svmPredFin","outcomeFin",file=file)
load(file)

i<-2L
file<-paste0("C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t_tripDistPredSVM",i,".png")
png(file)
plot(group[[i]],xlim=c(0,12000),ylim=c(0,12000),col="black",type="p",pch='.')
trip_distance<-group[[i]][,2]
svmPred <- group[[i]][,1]
lmfit<-lm(trip_distance~svmPred)
abline(lmfit,col="blue")
svmFit<-ksvm(x=svmPred, y=trip_distance,
             kernel="vanilladot",kpar="automatic",C=1,epsilon=0.1)
a=predict(svmFit,0)
b=predict(svmFit,1)-predict(svmFit,0)
abline(a=a,b=b,col="red")
abline(a=1.2*a,b=1.2*b,col="green")
dev.off()

lmfit<-lm(outcomeFin~svmPredFin)
svmFit<-ksvm(x=svmPredFin, y=outcomeFin,
             kernel="vanilladot",kpar="automatic",C=1,epsilon=0.1)
file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t_tripDistPredSVMfin2b.png"
png(file)
plot(svmPredFin,outcomeFin,xlim=c(0,12000),ylim=c(0,12000),col="black",type="p",pch='.')
abline(a=0,b=1.2,col="red")
abline(lmfit,col="blue")

#svmRes<-predict(svmFit,svmPred)

a=predict(svmFit,0)
b=predict(svmFit,1)-predict(svmFit,0)
abline(a=a,b=b,col="red")
abline(a=1.2*a,b=1.2*b,col="green")
dev.off()


file<-"C:/Users/natasa/Documents/projectAmadeus/code/model/taxi/fits/tOct01svmDistCombined3.rdata"
load(file)

lmfit<-lm(outcomeFin~svmPredFin)
svmFit<-ksvm(x=svmPredFin, y=outcomeFin,
             kernel="vanilladot",kpar="automatic",C=1,epsilon=0.1)
file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t_tripDistPredSVMfin3b.png"
png(file)
plot(svmPredFin,outcomeFin,xlim=c(0,12000),ylim=c(0,12000),col="black",type="p",pch='.')
abline(a=0,b=1.2,col="red")
abline(lmfit,col="blue")

#svmRes<-predict(svmFit,svmPred)

a=predict(svmFit,0)
b=predict(svmFit,1)-predict(svmFit,0)
abline(a=a,b=b,col="red")
abline(a=1.2*a,b=1.2*b,col="green")

dev.off()
