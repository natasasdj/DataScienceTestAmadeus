file<-"C:/Users/natasa/Documents/projectAmadeus/data/citibike/processed/cbOct13.rdata"
load(file=file)
str(cbOct13)
dim(cbOct13)
#[1] 1037712      15

q<-quantile(cbOct13$tripduration/60)
file<-"C:/Users/natasa/Documents/projectAmadeus/figures/final/cb_tripduration_hist.png"
png(file)
hist(cbOct13$tripduration/60,xlim=c(0,60),breaks=seq(0,20992,1),
     main="Histogram of tripduration",xlab='minutes',ylab='Trip duration')
abline(v=c(q[2],q[4]),col="blue",lwd=2)
abline(v=q[3],col="red",lwd=2)
dev.off()


q<-quantile(cbOct13$distance)
file<-"C:/Users/natasa/Documents/projectAmadeus/figures/final/cb_distance_hist.png"
png(file)
hist(cbOct13$distance,breaks=seq(0,10400,100),xlim=c(0,5000),
        main="Histogram of distance",xlab='m',ylab='Distance')
abline(v=c(q[2],q[4]),col="blue",lwd=2)
abline(v=q[3],col="red",lwd=2)
dev.off()


q<-quantile(cbOct13$speedkmh)
file<-"C:/Users/natasa/Documents/projectAmadeus/figures/final/cb_speed_hist.png"
png(file)
hist(cbOct13$speedkmh,breaks=seq(0,430,1),xlim=c(0,40),
        main="Histogram of speed",xlab='km/h',ylab='Speed')
abline(v=c(q[2],q[4]),col="blue",lwd=2)
abline(v=q[3],col="red",lwd=2)
dev.off()

q1<-quantile(cbOct13$tripduration/60)
q2<-quantile(cbOct13$distance)
q3<-quantile(cbOct13$speedkmh)
file<-"C:/Users/natasa/Documents/projectAmadeus/figures/final/cb_tripduration_dist_speed_hist.png"
png(file)
par(mfrow=c(3,1),mar=c(4.5,4.5,2.5,1))
hist(cbOct13$tripduration/60,xlim=c(0,60),breaks=seq(0,20992,1),
     main="Histogram of tripduration",xlab='minutes',ylab='Trip duration')
abline(v=c(q1[2],q1[4]),col="blue",lwd=2)
abline(v=q1[3],col="red",lwd=2)
hist(cbOct13$distance,breaks=seq(0,10400,100),xlim=c(0,5000),
     main="Histogram of distance",xlab='m',ylab='Distance')
abline(v=c(q2[2],q2[4]),col="blue",lwd=2)
abline(v=q2[3],col="red",lwd=2)
hist(cbOct13$speedkmh,breaks=seq(0,430,1),xlim=c(0,40),
     main="Histogram of speed",xlab='km/h',ylab='Speed')
abline(v=c(q3[2],q3[4]),col="blue",lwd=2)
abline(v=q3[3],col="red",lwd=2)
dev.off()

v1<-quantile(cbOct13$tripduration,p=c(0.05,0.95))
v1
# 5%  95% 
# 204 1871
v1b<-quantile(cbOct13$tripduration,p=c(0.1,0.9))
v1b
# 10%  90% 
# 262 1513 
v2<-quantile(cbOct13$distance,p=c(0.05,0.95))
v2
# 5%       95% 
# 373.0881 4564.8898 
v2b<-quantile(cbOct13$distance,p=c(0.1,0.9))
v2b
# 10%       90% 
# 546.4709 3605.6063 
v3<-quantile(cbOct13$speedkmh,p=c(0.05,0.95))
v3
# 5%       95% 
# 2.435294 16.835430 
v3b<-quantile(cbOct13$speedkmh,p=c(0.1,0.9))
v3b
# 10%       90% 
# 4.375919 13.148486
cond <- (cbOct13$tripduration>v1[1] & cbOct13$tripduration<v1[2]) &
        (cbOct13$distance>v2[1] & cbOct13$distance<v2[2]) &
        (cbOct13$speedkmh>v3[1] & cbOct13$speedkmh<v3[2])
sum(cond)
#[1] 832040
cbOct13pp<-cbOct13[cond,]
file<-"C:/Users/natasa/Documents/projectAmadeus/data/citibike/processed/cbOct13pp.rdata"
save("cbOct13pp",file=file)
load(file)

q1<-quantile(cbOct13pp$tripduration/60)
q2<-quantile(cbOct13pp$distance)
q3<-quantile(cbOct13pp$speedkmh)
file<-"C:/Users/natasa/Documents/projectAmadeus/figures/final/cb__tripdur_dist_speed_pp_hist.png"
png(file)
par(mfrow=c(3,1),mar=c(4.5,4.5,2.5,1))
hist(cbOct13pp$tripduration/60,xlim=c(0,60),breaks=seq(0,20992,1),
     main="Histogram of tripduration",xlab='minutes',ylab='Trip duration')
abline(v=c(q1[2],q1[4]),col="blue",lwd=2)
abline(v=q1[3],col="red",lwd=2)
hist(cbOct13pp$distance,breaks=seq(0,10400,100),xlim=c(0,5000),
     main="Histogram of distance",xlab='m',ylab='Distance')
abline(v=c(q2[2],q2[4]),col="blue",lwd=2)
abline(v=q2[3],col="red",lwd=2)
hist(cbOct13pp$speedkmh,breaks=seq(0,430,1),xlim=c(0,40),
     main="Histogram of speed",xlab='km/h',ylab='Speed')
abline(v=c(q3[2],q3[4]),col="blue",lwd=2)
abline(v=q3[3],col="red",lwd=2)
dev.off()


file<-"C:/Users/natasa/Documents/projectAmadeus/data/citibike/processed/cbOct13.rdata"
load(file=file)
str(cbOct13)
cbOct13$starttime<-strptime(cbOct13$starttime,"%Y-%m-%d %H:%M:%S")
cbOct13$stoptime<-strptime(cbOct13$stoptime,"%Y-%m-%d %H:%M:%S")



indb <- cbOct13$starttime$wday>=1 & cbOct13$starttime$wday<=5
cbOct13rb<-cbOct13[indb,]
dim(cbOct13rb)
#[1] 808330      17
file<-"C:/Users/natasa/Documents/projectAmadeus/data/citibike/processed/cbOct13rb.rdata"
save("cbOct13rb",file=file)
load(file)

q1<-quantile(cbOct13rb$tripduration/60)
q2<-quantile(cbOct13rb$distance)
q3<-quantile(cbOct13rb$speedkmh)
file<-"C:/Users/natasa/Documents/projectAmadeus/figures/final/cb__tripdur_dist_speed_rb_hist.png"
png(file)
par(mfrow=c(3,1),mar=c(4.5,4.5,2.5,1))
hist(cbOct13rb$tripduration/60,xlim=c(0,60),breaks=seq(0,20992,1),
     main="Histogram of tripduration",xlab='minutes',ylab='Trip duration')
abline(v=c(q1[2],q1[4]),col="blue",lwd=2)
abline(v=q1[3],col="red",lwd=2)
hist(cbOct13rb$distance,breaks=seq(0,10400,100),xlim=c(0,5000),
     main="Histogram of distance",xlab='m',ylab='Distance')
abline(v=c(q2[2],q2[4]),col="blue",lwd=2)
abline(v=q2[3],col="red",lwd=2)
hist(cbOct13rb$speedkmh,breaks=seq(0,430,1),xlim=c(0,40),
     main="Histogram of speed",xlab='km/h',ylab='Speed')
abline(v=c(q3[2],q3[4]),col="blue",lwd=2)
abline(v=q3[3],col="red",lwd=2)
dev.off()

 
v<-quantile(cbOct13rb$speedkmh,p=c(0.1,0.95))
v
# 10%       95% 
# 4.375919 17.22306
cond <- cbOct13rb$speedkmh>v[1] & cbOct13rb$speedkmh<v[2]
sum(cond)
#[1] 687080

cbOct13rbpp<-cbOct13rb[cond,]
file<-"C:/Users/natasa/Documents/projectAmadeus/data/citibike/processed/cbOct13rbpp.rdata"
save("cbOct13rbpp",file=file)
load(file)

q1<-quantile(cbOct13rbpp$tripduration/60)
q2<-quantile(cbOct13rbpp$distance)
q3<-quantile(cbOct13rbpp$speedkmh)
file<-"C:/Users/natasa/Documents/projectAmadeus/figures/final/cb__tripdur_dist_speed_rbpp_hist.png"
png(file)
par(mfrow=c(3,1),mar=c(4.5,4.5,2.5,1))
hist(cbOct13rbpp$tripduration/60,xlim=c(0,60),breaks=seq(0,20992,1),
     main="Histogram of tripduration",xlab='minutes',ylab='Trip duration')
abline(v=c(q1[2],q1[4]),col="blue",lwd=2)
abline(v=q1[3],col="red",lwd=2)
hist(cbOct13rbpp$distance,breaks=seq(0,10400,100),xlim=c(0,5000),
     main="Histogram of streight line distance",xlab='m',ylab='Distance')
abline(v=c(q2[2],q2[4]),col="blue",lwd=2)
abline(v=q2[3],col="red",lwd=2)
hist(cbOct13rbpp$speedkmh,breaks=seq(0,430,1),xlim=c(0,40),
     main="Histogram of speed",xlab='km/h',ylab='Speed')
abline(v=c(q3[2],q3[4]),col="blue",lwd=2)
abline(v=q3[3],col="red",lwd=2)
dev.off()

