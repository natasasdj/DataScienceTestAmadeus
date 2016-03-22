# svm with radial kernel on a random sample
file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/taxiOct01modelDistS.rdata"
load(file)
lmfit<-lm(taxiOct01modelDistS$trip_distance_m~taxiOct01modelDistS$distance)

cond <- taxiOct01modelDistS$trip_distance_m < 2*taxiOct01modelDistS$distance &
        taxiOct01modelDistS$trip_distance_m > taxiOct01modelDistS$distance
sum(cond)
#273957
taxiOct01modelDistS3<-taxiOct01modelDistS[cond,]
d<-dim(taxiOct01modelDistS)[1]
d<-dim(taxiOct01modelDistS3)[1]
ind<-seq(1,d,5)

library(kernlab)
svmFit<-ksvm(x=as.matrix(taxiOct01modelDistS[ind,2:5]), 
             y=taxiOct01modelDistS[ind,6],
             kernel="rbfdot",kpar="automatic",C=1,epsilon=0.1)
             
svmFit3 <- ksvm(x=as.matrix(taxiOct01modelDistS3[ind,2:5]), 
                y=taxiOct01modelDistS3[ind,6],
                kernel="rbfdot",kpar="automatic",C=1,epsilon=0.1)
                          

tripdistancePred <- as.vector(predict(svmFit,newdata=taxiOct01modelDistS[,2:5]))
tripdistancePred3 <- as.vector(predict(svmFit,newdata=taxiOct01modelDistS3[,2:5]))
tripdistancePred3b <- as.vector(predict(svmFit3,newdata=taxiOct01modelDistS3[ind,2:5]))

cond<-taxiOct01modelDistS3$trip_distance_m > 0.8 * tripdistancePred3
sum(cond)
#[1] 259559

file<-"C:/Users/natasa/Documents/projectAmadeus/code/model/taxi/fits/tOct01SVMrs3.rdata"
save("svmFit3","tripdistancePred3","cond",file=file)

file<-"C:/Users/natasa/Documents/projectAmadeus/code/model/taxi/fits/tOct01SVMrs3.rdata"
save("svmFit","tripdistancePred",file=file)

td<-taxiOct01modelDistS[,6]
lmfit<-lm(tripdistancePred~td)
td2<-td^2
lmfit2<-lm(tripdistancePred~td+td2)
summary(lmfit2)
tdl<-seq(200,12000,100)
lmfit2pred<-predict.lm(lmfit2,newdata=list(td=tdl,td2=tdl^2))
file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t_tripdistPredSVMrs.png"
png(file)
plot(taxiOct01modelDistS[,6],tripdistancePred,xlim=c(0,12000),ylim=c(0,12000),col="black",type="p",pch='.')
# abline(a=0,b=1.2,col="red")
# abline(a=0,b=0.8,col="red")
# abline(a=0,b=1.4,col="green")
# abline(a=0,b=0.4,col="green")
abline(lmfit,col="blue")
lines(tdl, lmfit2pred, col = "green", lwd = 2)

dev.off()

ind<-which(tripdistancePred)

cond1 <- tripdistancePred > 3000 &
         taxiOct01modelDistS$trip_distance_m < 1.2 * tripdistancePred &
        taxiOct01modelDistS$trip_distance_m > 0.8 * tripdistancePred
cond2 <- tripdistancePred <= 3000 &
        taxiOct01modelDistS$trip_distance_m < 1.4 * tripdistancePred &
        taxiOct01modelDistS$trip_distance_m > 0.8 * tripdistancePred
sum(cond2)
sum(taxiOct01modelDistS$trip_distance_in_m < 1.2 * tripdistancePred)
sum(cond1| cond2)
taxiOct01modelDistS2<-taxiOct01modelDistS[cond1 | cond2,]

file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t_dist_tripdist_rs.png"
png(file)
par(mar=c(2,2,1,1),mfrow=c(2,1))
plot(taxiOct01modelDistS[,7],taxiOct01modelDist[,6],
     ,xlim=c(0,12000),ylim=c(0,12000),col="black",type="p",pch='.')
plot(taxiOct01modelDistS2[,7],taxiOct01modelDistS2[,6],
     ,xlim=c(0,12000),ylim=c(0,12000),col="black",type="p",pch='.')
dev.off()

file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t_dist_tripdist_rs1.png"
png(file)

plot(taxiOct01modelDistS[,7],taxiOct01modelDist[,6],
     ,xlim=c(0,12000),ylim=c(0,12000),col="black",type="p",pch='.')
dev.off()

file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t_dist_tripdist_rs2.png"
png(file)
plot(taxiOct01modelDistS2[,7],taxiOct01modelDistS2[,6],
     ,xlim=c(0,12000),ylim=c(0,12000),col="black",type="p",pch='.')
dev.off()

file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t_dist_tripdist_rs3.png"
png(file)
plot(taxiOct01modelDistS3[,7],taxiOct01modelDistS3[,6],
     ,xlim=c(0,12000),ylim=c(0,12000),col="black",type="p",pch='.')
dev.off()

file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t_dist_tripdist_rs3small.png"
png(file)
plot(taxiOct01modelDistS3[,7],taxiOct01modelDistS3[,6],
     ,xlim=c(0,3000),ylim=c(0,3000),col="black",type="p",pch='.')
dev.off()

file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t_tripdistPredSVMrs3small.png"
png(file)
plot(taxiOct01modelDistS3[,6],tripdistancePred3,xlim=c(0,3000),ylim=c(0,3000),col="black",type="p",pch='.')
abline(a=0,b=1.2,col="red")
abline(a=0,b=0.8,col="red")
# abline(a=0,b=1.4,col="green")
# abline(a=0,b=0.6,col="green")
abline(a=0,b=1,col="green")
dev.off()

file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t_tripdistPredSVMrs3b.png"
png(file)
plot(taxiOct01modelDistS3[ind,6],tripdistancePred3b,xlim=c(0,12000),ylim=c(0,12000),col="black",type="p",pch='.')
#abline(a=0,b=1.2,col="red")
abline(a=0,b=0.8,col="red")
# abline(a=0,b=1.4,col="green")
# abline(a=0,b=0.6,col="green")
abline(a=0,b=1,col="green")
dev.off()




taxiOct01modelDistS4<-taxiOct01modelDistS3[cond,]
d<-dim(taxiOct01modelDistS4)[1]
ind<-seq(2,d,5)
length(ind)
svmFit<-ksvm(x=as.matrix(taxiOct01modelDistS4[ind,2:5]), 
             y=taxiOct01modelDistS4[ind,6],
             kernel="rbfdot",kpar="automatic",C=1,epsilon=0.1)
tripdistancePred <- as.vector(predict(svmFit,newdata=taxiOct01modelDistS4[,2:5]))

file<-"C:/Users/natasa/Documents/projectAmadeus/code/model/taxi/fits/tOct01SVMrs4.rdata"
save("svmFit","tripdistancePred",file=file)

file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t_tripdistPredSVMrs4.png"
png(file)
plot(taxiOct01modelDistS4[,6],tripdistancePred,xlim=c(0,12000),ylim=c(0,12000),col="black",type="p",pch='.')
abline(a=0,b=0.8,col="red")
abline(a=0,b=1,col="green")
dev.off()


cond<-taxiOct01modelDistS4$trip_distance_m > 0.8 * tripdistancePred
sum(cond)
#[1] 252687
file<-"C:/Users/natasa/Documents/projectAmadeus/code/model/taxi/fits/tOct01SVMrs4.rdata"
save("svmFit","tripdistancePred","cond",file=file)

taxiOct01modelDistS5<-taxiOct01modelDistS4[cond,]
d<-dim(taxiOct01modelDistS5)[1]
ind<-seq(3,d,5)
length(ind)
svmFit<-ksvm(x=as.matrix(taxiOct01modelDistS5[ind,2:5]), 
             y=taxiOct01modelDistS5[ind,6],
             kernel="rbfdot",kpar="automatic",C=1,epsilon=0.1)
tripdistancePred <- as.vector(predict(svmFit,newdata=taxiOct01modelDistS5[,2:5]))


file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t_tripdistPredSVMrS5.png"
png(file)
plot(taxiOct01modelDistS5[,6],tripdistancePred,xlim=c(0,12000),ylim=c(0,12000),col="black",type="p",pch='.')
abline(a=0,b=0.8,col="red")
abline(a=0,b=1,col="green")
dev.off()

cond <- taxiOct01modelDistS5$trip_distance_m > 0.8 * tripdistancePred & taxiOct01modelDistS5$trip_distance_m<10000
sum(cond)
#[1] 249174
#[1] 244868
file<-"C:/Users/natasa/Documents/projectAmadeus/code/model/taxi/fits/tOct01SVMrs5.rdata"
save("svmFit","tripdistancePred","cond",file=file)

taxiOct01modelDistS6<-taxiOct01modelDistS5[cond,]
d<-dim(taxiOct01modelDistS6)[1]
ind<-seq(4,d,5)
length(ind)
svmFit<-ksvm(x=as.matrix(taxiOct01modelDistS6[ind,2:5]), 
             y=taxiOct01modelDistS6[ind,6],
             kernel="rbfdot",kpar="automatic",C=1,epsilon=0.1)
tripdistancePred <- as.vector(predict(svmFit,newdata=taxiOct01modelDistS6[,2:5]))

file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/tOct01distModel.rdata"
save("taxiOct01modelDistS3","taxiOct01modelDistS4","taxiOct01modelDistS5","taxiOct01modelDistS6",file=file)

file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t_tripdistPredSVMrs6.png"
png(file)
plot(taxiOct01modelDistS6[,6],tripdistancePred,xlim=c(0,12000),ylim=c(0,12000),col="black",type="p",pch='.')
abline(a=0,b=0.8,col="red")
abline(a=0,b=1,col="green")
dev.off()

cond <- taxiOct01modelDistS6$trip_distance_m > 0.8 * tripdistancePred 
sum(cond)
#244081

file<-"C:/Users/natasa/Documents/projectAmadeus/code/model/taxi/fits/tOct01SVMrs6.rdata"
save("svmFit","tripdistancePred","cond",file=file)

# number of removed observations: 14398, 6872, 3513,787
# 4306 because of 10000 trip distance limitation

tOct01modelDur<-taxiOct01modelDistS6[cond,]
file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/tOct01modelDur.rdata"
save("tOct01modelDur",file=file)
load(file)
str(tOct01modelDur)
file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t_dist_tripdist_modelDur.png"
png(file)
#par(mar=c(2,2,1,1),mfrow=c(2,1))
plot(tOct01modelDur[,7],tOct01modelDur[,6],
     ,xlim=c(0,12000),ylim=c(0,12000),col="black",type="p",pch='.')
#plot(taxiOct01modelDistS2[,7],taxiOct01modelDistS2[,6],
     ,xlim=c(0,12000),ylim=c(0,12000),col="black",type="p",pch='.')
dev.off()
