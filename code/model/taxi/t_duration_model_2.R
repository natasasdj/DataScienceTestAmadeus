# taxiOct01modelDur
file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/tOct01modelDur.rdata"
load(file)
d<-dim(tOct01modelDur)[1]
d 
# [1] 244081

file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/taxi10_01pp_cb.rdata"
load(file)

str(taxi10_01pp_cb)
str(tOct01modelDur)

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