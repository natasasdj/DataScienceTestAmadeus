file<-"C:/Users/natasa/Documents/projectAmadeus/data/citibike/processed/cbOct13pp.rdata"
load(file)
str(cbOct13pp)
dim(cbOct13pp)
# [1] 832040     17
cbOct13pp$starttime<-strptime(cbOct13pp$starttime,"%Y-%m-%d %H:%M:%S")
cbOct13pp$stoptime<-strptime(cbOct13pp$stoptime,"%Y-%m-%d %H:%M:%S")

file<-"C:/Users/natasa/Documents/projectAmadeus/data/citibike/processed/cbOct13pp.rdata"
save("cbOct13pp",file=file)


indb <- cbOct13pp$starttime$wday>=1 & cbOct13pp$starttime$wday<=5
cbOct13b<-cbOct13pp[indb,]
dim(cbOct13b)
#[1] 654701     17
file<-"C:/Users/natasa/Documents/projectAmadeus/data/citibike/processed/cbOct13b.rdata"
save("cbOct13b",file=file)
load(file=file)
str(cbOct13b)
# check if tripduration=stoptime-starttime
ind<-which(cbOct13b$tripduration != difftime(cbOct13b$stoptime,cbOct13b$starttime,units="secs"))
ind
#integer(0)

# make data for model:
cbOct13bModelDur<-cbOct13b[,c(1,6,7,10,11,16)]
# make startHour column
cbOct13bModelDur$startHour<-cbOct13b$starttime$hour + cbOct13b$starttime$min/60

str(cbOct13bModelDur)
head(cbOct13bModelDur)
#sort according to startHour
cbOct13bModelDur<-cbOct13bModelDur[order(cbOct13bModelDur$startHour),]

# sampling each 15th sample on data sorted by time
d<-dim(cbOct13bModelDur)[1]
d
#654701
ind<-seq(1,d,10)
length(ind)
#65471
cbOct13bModelDurTrain<-cbOct13bModelDur[-ind,]
cbOct13bModelDurTest<-cbOct13bModelDur[ind,]
d<-dim(cbOct13bModelDurTrain)[1]
d

file<-"C:/Users/natasa/Documents/projectAmadeus/data/citibike/processed/cbOct13bModelDur.rdata"
save("cbOct13bModelDur","cbOct13bModelDurTrain","cbOct13bModelDurTest",file=file)



source("C:/Users/natasa/Documents/projectAmadeus/code/model/svmCombined.R")
data<-cbOct13bModelDurTrain
indOutcome<-1;indPredictors<-c(2:5,7); k<-5; no<-60000


str(cbOct13bModelDurTrain)

cb_svmFitList <- svmChunked3(cbOct13bModelDurTrain,indOutcome,indPredictors,k,no)
cb_svmPred <- svmCombPred3(cbOct13bModelDurTest,indPredictors,cb_svmFitList)
cb_svmPredMed<-0
cb_svmPredMed <- svmCombPred2(tOct01modelDurTest,indOutcome,svmFitList)
file<-"C:/Users/natasa/Documents/projectAmadeus/data/fits/cbOct13modelDur.rdata"
save("cb_svmFitList","cb_svmPred", "cb_svmPredMed",file=file)


plot(cbOct13bModelDurTest[,indOutcome],cb_svmPred[,6],col="black",type="p",pch='.')












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




file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t_dur_model_1med.png"
png(file)
plot(tOct01modelDurTest[,indOutcome],svmPredMed[,6],col="black",type="p",pch='.')
abline(a=0,b=1)
abline(a=0,b=1.3,col="blue")
abline(a=0,b=0.7,col="blue")
dev.off()

