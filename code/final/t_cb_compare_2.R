file<-"C:/Users/natasa/Documents/projectAmadeus/data/citibike/processed/cbOct13rbppModelDur.rdata"
load(file=file)
file<-"C:/Users/natasa/Documents/projectAmadeus/data/fits/cbOct13rbppmodelDur.rdata"
load(file=file)
file<-"C:/Users/natasa/Documents/projectAmadeus/data/citibike/processed/t01Oct13cb2ModelDur.rdata"
load(file=file)
file<-"C:/Users/natasa/Documents/projectAmadeus/data/fits/t01Oct13cb2modelDur.rdata"
load(file)


cbDur<-cb_svmPred[,6]
tDur<-tcb_svmPred[,5]
r<-(tDur-cbDur)/tDur
cbfaster<-cbDur-tDur<0
dist<-cbOct13rbppModelDurTest$distance
hour<-cbOct13rbppModelDurTest$startHour
h<-floor(hour)
distf<-cut(dist, c(seq(0,3000,500),max(dist)))
df<-data.frame(cbDur=cbDur,tDur=tDur,r=r,cbfaster=cbfaster,
               dist=dist,hour=hour, h=h,distf=distf)


plot(cbDur,tDur,col="black",type="p",pch='.',xlim=c(0,2000),ylim=c(0,2000))
abline(a=0,b=1,col="red")
plot(cbDur,tDur,col="black",type="p",pch='.',xlim=c(0,1000),ylim=c(0,1200))
abline(a=0,b=1,col="red")
plot(tDur,r,col="black",type="p",pch='.')
abline(h=0,col="red")
plot(cbDur,r,col="black",type="p",pch='.')
abline(h=0,col="red")

plot(dist,r,col="black",type="p",pch='.')
abline(h=0,col="red")
plot(hour,r,col="black",type="p",pch='.')
abline(h=0,col="red")
plot(h,r,col="black",type="p",pch='.')
abline(h=0,col="red")

distf
levels(distf)
levels(distf)<-c("1","2","3","4","5","6","7")

library(ggplot2)
qplot(hour, r, data = df, facets = distf ~ . , geom = 
              c("point", "smooth"))

ggplot(df, aes(hour,r)) + geom_point(size=0.001) + facet_wrap(~distf,ncol=4) +
        geom_smooth(size=0.8,col="red") + geom_hline(yintercept=0)
        labs(title="Taxi: Predicted vs. observed values") + 
        labs(x='Observed trip duration (secs)') + 
        labs(y='Predicted trip duration (secs)') +  
        coord_cartesian(xlim = c(0, 3000))


plot(hour,dist,col=r+3.5,type="p")
abline(h=0,col="red")

cbDur09<-cb_svmPred[cbOct13bModelDurTest$distance,6]
tDur09<-tcb_svmPred[,4]

library(lattice)
## Convert 'Month' to a factor variable
#airquality <- transform(airquality, Month = factor(Month))
xyplot(distance ~ startHour | cbfaster, layout = c(1, 2),pch=15,cex=0.1,alpha=0.3)
library(lattice)
xyplot(r ~ hour | distf, layout = c(3, 3),pch=15,cex=0.5)
library(kernlab)
xtrain<-as.matrix(data.frame(dist=dist, hour=hour))
outcome<-cbfaster
svmFitFin<-ksvm(x=xtrain, y=outcome,
             kernel="rbfdot",kpar="automatic",C=1,epsilon=0.1)
# too many obs

# regression tree
library(rpart)
rpartTreeFit<-rpart(cbfaster ~ dist + hour)
rpartTreeFit2<-rpart(cbfaster ~ dist + hour, control = rpart.control(cp = 0.005,minsplit=100,maxdepth=30))
rpartTreeFit3<-rpart(cbfaster ~ dist + hour, method="class",
        control = rpart.control(cp = 0.001,minsplit=20,maxdepth=30))
xtest<-data.frame(dist=dist, hour=hour)
rpartTreePred<-predict(rpartTreeFit,newdata=xtest)
rpartTreePred2<-predict(rpartTreeFit2,newdata=xtest)
rpartTreePred3<-predict(rpartTreeFit3,newdata=xtest)
# rpartTree<-rpart(trip_distance_m ~ .,data=taxiOct01modelDist2, control = rpart.control(cp = 0.005,minsplit=100,maxdepth=30))
# no good for this
table(cbfaster,rpartTreePred>0.55)/length(cbfaster)
table(cbfaster,rpartTreePred2<0.55)/length(cbfaster)
table(cbfaster,rpartTreePred3[,2]>=0.4)/length(cbfaster)
ind <- rpartTreePred2>0.55
hourdist<-NULL
for (v1 in seq(0,23.5,0.1)){
        for (v2 in seq(300,3000,100)) hourdist<-rbind(hourdist,c(v1,v2))
        
}
disthour<-data.frame(dist=hourdist[,2],hour=hourdist[,1])
colnames(hourdist)<-c("hour","dist")
gridPred<-predict(rpartTreeFit2,newdata=disthour)
ind<-gridPred>0.55
indf<-as.factor(ind)
levels(indf)<-c("faster","slower")
faster<-hourdist[ind,]
slower<-hourdist[!ind,]
head(hourdist)
plot(faster,col='red')
plot(slower,col='blue')
distHourPred<-data.frame(distance=hourdist[,2],hour=hourdist[,1],citibike=indf)
head(distHourPred)
ggplot(distHourPred, aes(hour,distance)) + 
        geom_point(aes(color=citibike)) + theme_bw() +
        labs(title="Which is faster: citibike or taxi?",size=15)

library(randomForest)
rfFit<-randomForest(as.factor(cbfaster) ~ dist + hour,ntree=500)
rfPred<-predict(rfFit,newdata=xtest)
table(cbfaster,rfPred)/length(cbfaster)
ggplot(df, aes(hour,dist)) + 
        geom_point(aes(color=cbfaster), size = 1, alpha = 1/4) + theme_bw() +
        labs(title="Which is faster: citibike or taxi?")


rfGridPred<-predict(rfFit,newdata=disthour)
indf<-as.factor(rfGridPred)
levels(indf)<-c("citibike","taxi")
distHourPred<-data.frame(distance=hourdist[,2],hour=hourdist[,1],cbfaster=indf)

ggplot(distHourPred, aes(hour,distance))  +
        geom_point(aes(color=cbfaster)) + 
        scale_colour_discrete(name  ="Which is faster: citibike or taxi?") +
        theme_bw() + 
        theme(legend.title = element_text(size=16, face="bold")) +
        theme(legend.text = element_text(size = 16)) +
        guides(shape=guide_legend(override.aes=list(size=10,alpha=1)))















