file<-"C:/Users/natasa/Documents/projectAmadeus/data/fits/cbOct13modelDur.rdata"
load(file=file)
file<-"C:/Users/natasa/Documents/projectAmadeus/data/fits/t01Oct13modelDur.rdata"
load(file=file)
file<-"C:/Users/natasa/Documents/projectAmadeus/data/citibike/processed/cbOct13bModelDur.rdata"
load(file=file)



cbDur<-cb_svmPred[,6]
tDur<-tcb_svmPred[,4]
r<-(cbDur-tDur)/tDur
diffDur<-(cbDur-tDur)
cbfaster<-diffDur<0

distance<-cbOct13bModelDurTest$distance
startHour<-cbOct13bModelDurTest$startHour

plot(cbDur,tDur,col="black",type="p",pch='.')
abline(a=0,b=1,col="red")
plot(tDur,r,col="black",type="p",pch='.')
abline(h=0,col="red")
plot(cbDur,diffDur,col="black",type="p",pch='.')
abline(h=0,col="red")
plot(distance,r,col="black",type="p",pch='.')
abline(h=0,col="red")
plot(startHour,r,col="black",type="p",pch='.')
abline(h=0,col="red")
cb_svmPred

cbDur09<-cb_svmPred[cbOct13bModelDurTest$distance,6]
tDur09<-tcb_svmPred[,4]

library(lattice)
## Convert 'Month' to a factor variable
#airquality <- transform(airquality, Month = factor(Month))
xyplot(distance ~ startHour | cbfaster, layout = c(1, 2),pch=15,cex=0.1,alpha=0.3)



