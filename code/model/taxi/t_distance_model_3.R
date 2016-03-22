file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/taxiOct01modelDistS.rdata"
load(file)

lessInd<-which(taxiOct01modelDistS$distance>taxiOct01modelDistS$trip_distance_m)
length(lessInd)
[1] 17657

taxiOct01modelDistScut<-taxiOct01modelDistS[-lessInd,]

file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/taxiOct01modelDistScut.rdata"
save("taxiOct01modelDistScut",file=file)

library(kernlab)
group<-list()
svmFitList<-list()
k<-10 # no of chunks
d<-dim(taxiOct01modelDistScut)[[1]] # no of observations
no <- floor(d/k) # no of observations per chunk
for (i in 1:k){
        cat(i,"\n")
        if (i==k) {ind<-((i-1)*no+1):d
                   
        } else {ind<-((i-1)*no+1):(i*no)}
        
        
        xtrain<- as.matrix(taxiOct01modelDistScut[ind,2:5])
        trip_distance<- taxiOct01modelDistScut[ind,6]
        svmFit<-ksvm(x=xtrain, y=trip_distance,
                     kernel="rbfdot",kpar="automatic",C=1,epsilon=0.1)
        svmPred <- as.vector(predict(svmFit,newdata=taxiOct01modelDistScut[ind,2:5]))
        svmFitList[[i]] <- svmFit
        group[[i]]<-cbind(svmPred,trip_distance)
}

file<-"C:/Users/natasa/Documents/projectAmadeus/code/model/taxi/fits/tOct01svmDistCut.rdata"
save("svmFitList","group",file=file)

xtrain<-NULL
ind<-1:d
for (i in 1:k){
        cat("prediction: chunk",i,"\n")
        svmFit <- svmFitList[[i]]
        svmPred <- as.vector(predict(svmFit,newdata=taxiOct01modelDistScut[ind,2:5]))
        xtrain<-cbind(xtrain,svmPred)
}
trip_distance <- taxiOct01modelDistScut[ind,6]

file<-"C:/Users/natasa/Documents/projectAmadeus/code/model/taxi/fits/tOct01DistCombSVMdataCut.rdata"
save("xtrain","trip_distance",file=file)
load(file)
# svmFitFin<-ksvm(x=xtrain, y=trip_distance,
#              kernel="vanilladot",kpar="automatic",C=1,epsilon=0.1)
# sample data because too big
ind<-seq(1,d,5)
svmFitFin<-ksvm(x=xtrain[ind,], y=trip_distance[ind],
                kernel="vanilladot",kpar="automatic",C=1,epsilon=0.1)
outcomeFin <- trip_distance[ind] 
svmPredFin <- as.vector(predict(svmFitFin,newdata=xtrain[ind,]))

file<-"C:/Users/natasa/Documents/projectAmadeus/code/model/taxi/fits/tOct01svmDistComb2cut.rdata"
save("svmFitFin","svmPredFin","outcomeFin",file=file)
load(file)


