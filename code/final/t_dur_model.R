
file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/t01Oct13pp.rdata"
load(file=file)

str(t01Oct13pp)
t01Oct13pp$pickup_datetime<-strptime(t01Oct13pp$pickup_datetime,"%Y-%m-%d %H:%M:%S")
t01Oct13pp$dropoff_datetime<-strptime(t01Oct13pp$dropoff_datetime,"%Y-%m-%d %H:%M:%S")

file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/t01Oct13pp.rdata"
save("t01Oct13pp",file=file)
#292692
# consider only those rows with pickup and dropoff within the area of bike stations
cond <- t01Oct13pp$pickup_latitude>=40.68 &
        t01Oct13pp$pickup_latitude<=40.78 &
        t01Oct13pp$pickup_longitude>=-74.02 &
        t01Oct13pp$pickup_longitude<=-73.95 &
        t01Oct13pp$dropoff_latitude>=40.68 &
        t01Oct13pp$dropoff_latitude<=40.78 &
        t01Oct13pp$dropoff_longitude>=-74.02 &
        t01Oct13pp$dropoff_longitude<=-73.95 

sum(!cond)
#[1] 78818
t01Oct13ModelDur<-t01Oct13pp[,c(3,5:9,12)]
t01Oct13ModelDur$pickup_hour<-t01Oct13pp$pickup_datetime$hour + 
        t01Oct13pp$pickup_datetime$min/60
t01Oct13ModelDur<-t01Oct13ModelDur[cond,]
str(t01Oct13ModelDur)
# 213874


#sort according to pickup_hour
t01Oct13ModelDur<-t01Oct13ModelDur[order(t01Oct13ModelDur$pickup_hour),]
# sampling each 10th sample on data sorted by time
d<-dim(t01Oct13ModelDur)[1]
d
#213874
ind<-seq(1,d,10)
length(ind)
#21388
t01Oct13ModelDurTrain<-t01Oct13ModelDur[-ind,]
t01Oct13ModelDurTest<-t01Oct13ModelDur[ind,]
d<-dim(t01Oct13ModelDurTrain)[1]
d
#192486
file<-"C:/Users/natasa/Documents/projectAmadeus/data/citibike/processed/t01Oct13ModelDur.rdata"
save("t01Oct13ModelDur","t01Oct13ModelDurTrain","t01Oct13ModelDurTest",file=file)



source("C:/Users/natasa/Documents/projectAmadeus/code/model/svmCombined.R")
#data<-t01Oct13ModelDurTrain
indOutcome<-1;indPredictors<-c(2:5,8); k<-3; no<-60000
str(t01Oct13ModelDurTrain)

t_svmFitList <- svmChunked3(t01Oct13ModelDurTrain,indOutcome,indPredictors,k,no)
t_svmPred <- svmCombPred3(t01Oct13ModelDurTest,indPredictors,t_svmFitList)

tcbOct13bModelDurTest<-data.frame( 
        pickup_longitude = cbOct13bModelDurTest$start.station.longitude,
        pickup_latitude = cbOct13bModelDurTest$start.station.latitude,
        dropoff_longitude = cbOct13bModelDurTest$end.station.longitude,
        dropoff_latitude = cbOct13bModelDurTest$end.station.latitude,
        pickup_hour = cbOct13bModelDurTest$startHour)
indPredictorsCb<-c(1:5)
tcb_svmPred <- svmCombPred3(tcbOct13bModelDurTest,indPredictorsCb,t_svmFitList)
t_svmPredMed<-0
#t_svmPredMed <- svmCombPred2(tOct01modelDurTest,indOutcome,svmFitList)
file<-"C:/Users/natasa/Documents/projectAmadeus/data/fits/t01Oct13modelDur.rdata"
save("t_svmFitList","t_svmPred", "t_svmPredMed","tcb_svmPred",file=file)


plot(t01Oct13ModelDurTest[,indOutcome],t_svmPred[,4],col="black",type="p",pch='.')

















file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/tOct01modelDur.rdata"
load(file)
d<-dim(tOct01modelDur)[1]
d 
# [1] 244081

file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/taxi10_01pp_cb.rdata"
load(file)

str(taxi10_01pp_cb)


rind<-rownames(tOct01modelDur)
tOct01modelDur$trip_trip_time_in_secs<-taxi10_01pp_cb[rind,3]
tOct01modelDur$pickup_hour<-tOct01modelDur$pickup_datetime$hour + 
        tOct01modelDur$pickup_datetime$min/60




# sampling one for each minute
# t <- 0
# ind<-NULL
# cat("index: ")
# for (i in 1:d){
#         tminute<-tOct01modelDur$pickup_datetime[i]$min
#         if (tminute>=t) {
#                 ind<-c(ind,i)
#                 cat(i," ")
#                 t<-(tminute+1)%%60
#         }
#                 
# }
# length(ind)
#[1] 5429

# sampling each 10th sample on data sorted by time
ind<-seq(1,d,10)
length(ind)
#[1] 24409




tOct01modelDurTrain<-tOct01modelDur[-ind,c(2:5,8:9)]
tOct01modelDurTest<-tOct01modelDur[ind,c(2:5,8:9)]

file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/tOct01modelDur.rdata"
save("tOct01modelDur","tOct01modelDurTrain","tOct01modelDurTest",file=file)

str(tOct01modelDurTrain)
source(svmCombinedChunks)
indOutcome <- 5
k <- 5

sampleInd<-function(){
        
}
 
svmFitList <- svmChunked2(tOct01modelDurTrain,indOutcome,k)

svmPred <- svmCombPred(tOct01modelDurTest,indOutcome,svmFitList)
# svmPredTrain <- as.vector(predict(svmFitFin,newdata=data[tOct01modelDurTrain,-indOutcome]))
# svmPredTest <- as.vector(predict(svmFitFin,newdata=data[tOct01modelDurTrain,-indOutcome]))
# 
#plot(svmPred,tOct01modelDurTrain[,indOutcome])
file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t_dur_model_1.png"
png(file)
plot(tOct01modelDurTest[,indOutcome],svmPred[,6],col="black",type="p",pch='.')
abline(a=0,b=1)
abline(a=0,b=1.3,col="blue")
abline(a=0,b=0.7,col="blue")
dev.off()

file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t_dur_model_1b.png"
png(file)
plot(tOct01modelDurTest[,indOutcome],svmPred[,1],col="black",type="p",pch='.')
abline(a=0,b=1)
abline(a=0,b=1.3,col="blue")
abline(a=0,b=0.7,col="blue")
dev.off()

# try with median

svmPredMed <- svmCombPred2(tOct01modelDurTest,indOutcome,svmFitList)
file<-"C:/Users/natasa/Documents/projectAmadeus/data/fits/tOct01modelDur.rdata"
save("svmFitList","svmPred", "svmPredMed",file=file)


file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t_dur_model_1med.png"
png(file)
plot(tOct01modelDurTest[,indOutcome],svmPredMed[,6],col="black",type="p",pch='.')
abline(a=0,b=1)
abline(a=0,b=1.3,col="blue")
abline(a=0,b=0.7,col="blue")
dev.off()



# consider only trips bellow 1800s (30min)