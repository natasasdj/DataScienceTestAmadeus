
file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/t01Oct13cbpp2.rdata"
load(file=file)

str(t01Oct13cbpp)
#226025 pp
#235266 pp2

t01Oct13cbpp$pickup_datetime<-strptime(t01Oct13cbpp$pickup_datetime,"%Y-%m-%d %H:%M:%S")
t01Oct13cbpp$dropoff_datetime<-strptime(t01Oct13cbpp$dropoff_datetime,"%Y-%m-%d %H:%M:%S")

file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/t01Oct13cbpp2.rdata"
save("t01Oct13cbpp",file=file)


t01Oct13cbModelDur<-t01Oct13cbpp[,c(3,5:9,12)]
t01Oct13cbModelDur$pickup_hour<-t01Oct13cbpp$pickup_datetime$hour + 
        t01Oct13cbpp$pickup_datetime$min/60
str(t01Oct13cbModelDur)



#sort according to pickup_hour
t01Oct13cbModelDur<-t01Oct13cbModelDur[order(t01Oct13cbModelDur$pickup_hour),]
# sampling each 10th sample on data sorted by time
d<-dim(t01Oct13cbModelDur)[1]
ind<-seq(1,d,10)
length(ind)
#23527
t01Oct13cbModelDurTrain<-t01Oct13cbModelDur[-ind,]
t01Oct13cbModelDurTest<-t01Oct13cbModelDur[ind,]
d<-dim(t01Oct13cbModelDurTrain)[1]
d
#211739
file<-"C:/Users/natasa/Documents/projectAmadeus/data/citibike/processed/t01Oct13cb2ModelDur.rdata"
save("t01Oct13cbModelDur","t01Oct13cbModelDurTrain","t01Oct13cbModelDurTest",file=file)



source("C:/Users/natasa/Documents/projectAmadeus/code/model/svmCombined.R")
data<-t01Oct13cbModelDurTrain
indOutcome<-1;indPredictors<-c(2:5,8); k<-4; no<-50000
str(t01Oct13cbModelDurTrain)

t_svmFitList <- svmChunked3(t01Oct13cbModelDurTrain,indOutcome,indPredictors,k,no)
t_svmPred <- svmCombPred3(t01Oct13cbModelDurTest,indPredictors,t_svmFitList)

#file<-"C:/Users/natasa/Documents/projectAmadeus/data/fits/t01Oct13cb2modelDur.rdata"
#save("t_svmFitList","t_svmPred", "tcb_svmPred",file=file)

file<-"C:/Users/natasa/Documents/projectAmadeus/data/citibike/processed/cbOct13rbppModelDur.rdata"
load(file=file)


tcbOct13cbModelDurTest<-data.frame( 
        pickup_longitude = cbOct13rbppModelDurTest$start.station.longitude,
        pickup_latitude = cbOct13rbppModelDurTest$start.station.latitude,
        dropoff_longitude = cbOct13rbppModelDurTest$end.station.longitude,
        dropoff_latitude = cbOct13rbppModelDurTest$end.station.latitude,
        pickup_hour = cbOct13rbppModelDurTest$startHour)
indPredictorsCb<-c(1:5)
tcb_svmPred <- svmCombPred3(tcbOct13cbModelDurTest,indPredictorsCb,t_svmFitList)
#t_svmPredMed <- svmCombPred2(tOct01modelDurTest,indOutcome,svmFitList)
file<-"C:/Users/natasa/Documents/projectAmadeus/data/fits/t01Oct13cb2modelDur.rdata"
save("t_svmFitList","t_svmPred", "tcb_svmPred",file=file)
load(file)

plot(t01Oct13cbModelDurTest[,indOutcome],t_svmPred[,5],col="black",type="p",pch='.')
abline(a=0,b=1,col="blue")

str(t_svmPred)














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