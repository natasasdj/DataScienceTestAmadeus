file<-"C:/Users/natasa/Documents/projectAmadeus/data/citibike/processed/cbOct13rbpp.rdata"
load(file)
str(cbOct13rbpp)
#687080 obs
# check if tripduration=stoptime-starttime
ind<-which(cbOct13rbpp$tripduration != difftime(cbOct13rbpp$stoptime,cbOct13rb$starttime,units="secs"))
ind
#integer(0)



# make data for model:
cbOct13rbppModelDur<-cbOct13rbpp[,c(1,6,7,10,11,16)]
# make startHour column
cbOct13rbppModelDur$startHour<-cbOct13rbpp$starttime$hour + cbOct13rbpp$starttime$min/60

str(cbOct13rbppModelDur)
head(cbOct13rbppModelDur)
#sort according to startHour
cbOct13rbppModelDur<-cbOct13rbppModelDur[order(cbOct13rbppModelDur$startHour),]

# sampling each 15th sample on data sorted by time
d<-dim(cbOct13rbppModelDur)[1]
d
#687080
ind<-seq(1,d,10)
length(ind)
#[1] 68708
cbOct13rbppModelDurTrain<-cbOct13rbppModelDur[-ind,]
cbOct13rbppModelDurTest<-cbOct13rbppModelDur[ind,]
d<-dim(cbOct13rbppModelDurTrain)[1]
d
#[1] 618372

file<-"C:/Users/natasa/Documents/projectAmadeus/data/citibike/processed/cbOct13rbppModelDur.rdata"
save("cbOct13rbppModelDur","cbOct13rbppModelDurTrain","cbOct13rbppModelDurTest",file=file)



source("C:/Users/natasa/Documents/projectAmadeus/code/model/svmCombined.R")
data<-cbOct13rbppModelDurTrain
indOutcome<-1;indPredictors<-c(2:5,7); k<-5; no<-60000


str(cbOct13rbppModelDurTrain)

cb_svmFitList <- svmChunked3(cbOct13rbppModelDurTrain,indOutcome,indPredictors,k,no)
cb_svmPred <- svmCombPred3(cbOct13rbppModelDurTest,indPredictors,cb_svmFitList)
cb_svmPredMean <- svmCombPred1(cbOct13rbppModelDurTest,indPredictors,cb_svmFitList)
file<-"C:/Users/natasa/Documents/projectAmadeus/data/fits/cbOct13rbppmodelDur.rdata"
save("cb_svmFitList","cb_svmPred", "cb_svmPredMean",file=file)


file<-"C:/Users/natasa/Documents/projectAmadeus/figures/final/cb_bpp_dur_model.png"
png(file)
plot(cbOct13rbppModelDurTest[,indOutcome],cb_svmPred[,6],col="black",type="p",pch='.')
abline(a=0,b=1,col="blue")
abline(a=0,b=1.3,col="red")
abline(a=0,b=0.7,col="red")
dev.off()


file<-"C:/Users/natasa/Documents/projectAmadeus/figures/final/cb_bpp_dur_meanmodel.png"
png(file)
plot(cbOct13rbppModelDurTest[,indOutcome],cb_svmPredMean[,6],col="black",type="p",pch='.')
abline(a=0,b=1,col="blue")
abline(a=0,b=1.3,col="red")
abline(a=0,b=0.7,col="red")
dev.off()

# ```{r,echo=FALSE,cache=TRUE,fig.height=6}
# file<-"C:/Users/natasa/Documents/projectAmadeus/data/citibike/processed/cbOct13rbppModelDur.rdata"
# load(file=file)
# file<-"C:/Users/natasa/Documents/projectAmadeus/data/fits/cbOct13rbppmodelDur.rdata"
# load(file=file)
# indOutcome<-1;indPredictors<-c(2:5,7)
# plot(cbOct13rbppModelDurTest[,indOutcome],cb_svmPred[,6],col="black",type="p",pch='.',main="Predicted vs. observed values",xlab='Observed trip duration (secs)', ylab='Predicted trip duration (secs)')
# abline(a=0,b=1,col="blue")
# ```





