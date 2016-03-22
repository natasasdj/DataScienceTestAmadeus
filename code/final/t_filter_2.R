file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/taxi10_01pp.rdata"
load(file)
str(taxi10_01pp)
dim(taxi10_01pp)
#[1] 452769     11
summary(taxi10_01pp)

cond<-!is.na(taxi10_01pp$trip_time_in_secs) 
taxi10_01pp<- taxi10_01pp[cond,]

speed<-taxi10_01pp$trip_distance_m/taxi10_01pp$trip_time_in_secs
summary(speed)
ind<-which(is.na(taxi10_01pp$speed))
head(ind)
taxi10_01pp[1651,]

library("Imap")
d<-dim(taxi10_01pp)[1]
dist<-rep(0,d)
for (i in 1:d){
        if (i%%1000==0) cat("itearation ",i," ")
        dist[i]<-gdist(taxi10_01pp$pickup_longitude[i],taxi10_01pp$pickup_latitude[i],
                       taxi10_01pp$dropoff_longitude[i],taxi10_01pp$dropoff_latitude[i],
                       units="m")
        #if is.na(dist[i]) cat("NA")
}
taxi10_01pp$distance<-dist
file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/taxi10_01pp.rdata"
save("taxi10_01pp",file=file)

cond<- taxi10_01pp$trip_time_in_secs==0 | taxi10_01pp$distance==0 | is.na(taxi10_01pp$dropoff_longitude)
sum(cond)
t01Oct13pp<-taxi10_01pp[!cond,]
dim(t01Oct13)
str(t01Oct13)
#[1] 444479     12

file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/t01Oct13.rdata"
save("t01Oct13",file=file)
load(file)

# only citibike area

cond <- t01Oct13$pickup_latitude>=40.68 &
        t01Oct13$pickup_latitude<=40.78 &
        t01Oct13$pickup_longitude>=-74.02 &
        t01Oct13$pickup_longitude<=-73.95 &
        t01Oct13$dropoff_latitude>=40.68 &
        t01Oct13$dropoff_latitude<=40.78 &
        t01Oct13$dropoff_longitude>=-74.02 &
        t01Oct13$dropoff_longitude<=-73.95


sum(!cond)
#[1] 136729
t01Oct13cb<-t01Oct13[cond,]

file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/t01Oct13cb.rdata"
save("t01Oct13cb",file=file)
load(file)
str(t01Oct13cb)
# 307750
q1<-quantile(t01Oct13cb$trip_time_in_secs/60)
q2<-quantile(t01Oct13cb$trip_distance_m)
q5<-quantile(t01Oct13cb$distance)
q3<-quantile(t01Oct13cb$speedkmh)
q4<-quantile(t01Oct13cb$trip_distance_m/t01Oct13cb$distance)
file<-"C:/Users/natasa/Documents/projectAmadeus/figures/final/tcb_dur_dist_speed_hist.png"
png(file)
par(mfrow=c(4,1),mar=c(4.5,4.5,2.5,1))
xmax<-ceiling(max(t01Oct13cb$trip_time_in_secs/60))
hist(t01Oct13cb$trip_time_in_secs/60,xlim=c(0,60),breaks=seq(0,xmax,1),
     main="Histogram of trip duration",xlab='minutes')
abline(v=c(q1[2],q1[4]),col="blue",lwd=2)
abline(v=q1[3],col="red",lwd=2)
xmax<-ceiling(max(t01Oct13cb$trip_distance_m))
hist(t01Oct13cb$trip_distance_m,breaks=seq(0,xmax+100,200),xlim=c(0,10000),
     main="Histogram of trip distance",xlab='m')
abline(v=c(q2[2],q2[4]),col="blue",lwd=2)
abline(v=q2[3],col="red",lwd=2)
xmax<-ceiling(max(t01Oct13cb$distance))
hist(t01Oct13cb$distance,breaks=seq(0,xmax+100,200),xlim=c(0,10000),
     main="Histogram of straight line distance",xlab='m')
abline(v=c(q5[2],q5[4]),col="blue",lwd=2)
abline(v=q5[3],col="red",lwd=2)
xmax<-ceiling(max(t01Oct13cb$speedkmh))
hist(t01Oct13cb$speedkmh,breaks=seq(0,xmax,1),xlim=c(0,60),
     main="Histogram of speed",xlab='km/h')
abline(v=c(q3[2],q3[4]),col="blue",lwd=2)
abline(v=q3[3],col="red",lwd=2)
xmax<-ceiling(max(t01Oct13cb$trip_distance_m/t01Oct13cb$distance))
hist(t01Oct13cb$trip_distance_m/t01Oct13cb$distance,breaks=seq(0,xmax,0.05),xlim=c(0,2.5),
     main="Histogram of the ratio of trip distance and euclidian distance",xlab='')
abline(v=c(q4[2],q4[4]),col="blue",lwd=2)
abline(v=q4[3],col="red",lwd=2)
dev.off()




v3<-quantile(t01Oct13cb$speedkmh,p=c(0.05,0.95))
v3
# 5%       95% 
# 6.862932 32.292330 
v3b<-quantile(t01Oct13cb$speedkmh,p=c(0.1,0.9))
v3b
# 10%       90% 
# 8.465569 27.519714 
v4<-quantile(t01Oct13cb$trip_distance_m/t01Oct13cb$distance,p=c(0.05,0.95))
v4
# 5%       95% 
# 0.9788933 1.9177435 
v4b<-quantile(t01Oct13cb$trip_distance_m/t01Oct13cb$distance,p=c(0.1,0.9))
v4b
# 10%       90% 
# 1.032018 1.660168

cond <- (t01Oct13cb$speedkmh>v3[1] & t01Oct13cb$speedkmh<v3[2]) &
        (t01Oct13cb$trip_distance_m/t01Oct13cb$distance>v4b[1] & t01Oct13cb$trip_distance_m/t01Oct13cb$distance<v4b[2])
sum(!cond)
#[1] 81725

t01Oct13cbpp<-t01Oct13cb[cond,]
file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/t01Oct13cbpp.rdata"
save("t01Oct13cbpp",file=file)
load(file)

q1<-quantile(t01Oct13cbpp$trip_time_in_secs/60)
q2<-quantile(t01Oct13cbpp$trip_distance_m)
q3<-quantile(t01Oct13cbpp$speedkmh)
q4<-quantile(t01Oct13cbpp$trip_distance_m/t01Oct13cbpp$distance)
file<-"C:/Users/natasa/Documents/projectAmadeus/figures/final/t_dur_dist_speed_pp_hist.png"
png(file)
par(mfrow=c(4,1),mar=c(4.5,4.5,2.5,1))
xmax<-ceiling(max(t01Oct13cbpp$trip_time_in_secs/60))
hist(t01Oct13cbpp$trip_time_in_secs/60,xlim=c(0,60),breaks=seq(0,xmax,1),
     main="Histogram of trip duration",xlab='minutes')
abline(v=c(q1[2],q1[4]),col="blue",lwd=2)
abline(v=q1[3],col="red",lwd=2)
xmax<-ceiling(max(t01Oct13cbpp$trip_distance_m))
hist(t01Oct13cbpp$trip_distance_m,breaks=seq(0,xmax+100,200),xlim=c(0,10000),
     main="Histogram of distance",xlab='m')
abline(v=c(q2[2],q2[4]),col="blue",lwd=2)
abline(v=q2[3],col="red",lwd=2)
xmax<-ceiling(max(t01Oct13cbpp$speedkmh))
hist(t01Oct13cbpp$speedkmh,breaks=seq(0,xmax,1),xlim=c(0,60),
     main="Histogram of speed",xlab='km/h')
abline(v=c(q3[2],q3[4]),col="blue",lwd=2)
abline(v=q3[3],col="red",lwd=2)
xmax<-ceiling(max(t01Oct13cbpp$trip_distance_m/t01Oct13cbpp$distance))
hist(t01Oct13cbpp$trip_distance_m/t01Oct13cbpp$distance,breaks=seq(0,xmax,0.05),xlim=c(0,2.5),
     main="Histogram of the ratio of trip distance and euclidian distance",xlab='ratio')
abline(v=c(q4[2],q4[4]),col="blue",lwd=2)
abline(v=q4[3],col="red",lwd=2)
dev.off()



# ```{r,echo=FALSE,cache=TRUE,fig.height=8}
# file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/t01Oct13.rdata"
# load(file)
# q1<-quantile(t01Oct13$trip_time_in_secs/60)
# q2<-quantile(t01Oct13$trip_distance_m)
# q3<-quantile(t01Oct13$speedkmh)
# q4<-quantile(t01Oct13$trip_distance_m/t01Oct13$distance)
# par(mfrow=c(4,1),mar=c(4.5,4.5,2.5,1))
# xmax<-ceiling(max(t01Oct13$trip_time_in_secs/60))
# hist(t01Oct13$trip_time_in_secs/60,xlim=c(0,60),breaks=seq(0,xmax,1),
#      main="Histogram of trip duration",xlab='minutes')
# abline(v=c(q1[2],q1[4]),col="blue",lwd=2)
# abline(v=q1[3],col="red",lwd=2)
# xmax<-ceiling(max(t01Oct13$trip_distance_m))
# hist(t01Oct13$trip_distance_m,breaks=seq(0,xmax+100,200),xlim=c(0,10000),
#      main="Histogram of trip distance",xlab='m')
# abline(v=c(q2[2],q2[4]),col="blue",lwd=2)
# abline(v=q2[3],col="red",lwd=2)
# xmax<-ceiling(max(t01Oct13$speedkmh))
# hist(t01Oct13$speedkmh,breaks=seq(0,xmax,1),xlim=c(0,60),
#      main="Histogram of speed",xlab='km/h')
# abline(v=c(q3[2],q3[4]),col="blue",lwd=2)
# abline(v=q3[3],col="red",lwd=2)
# xmax<-ceiling(max(t01Oct13$trip_distance_m/t01Oct13$distance))
# hist(t01Oct13$trip_distance_m/t01Oct13$distance,breaks=seq(0,xmax,0.05),xlim=c(0,2.5),
#      main="Histogram of the ratio of trip distance and euclidian distance",xlab='ratio')
# abline(v=c(q4[2],q4[4]),col="blue",lwd=2)
# abline(v=q4[3],col="red",lwd=2)
# q1<-quantile(t01Oct13$trip_time_in_secs/60,probs=c(0,0.05,0.1,0.25,0.5,0.75,0.9,0.95,1))
# q2<-quantile(t01Oct13$trip_distance_m,probs=c(0,0.05,0.1,0.25,0.5,0.75,0.9,0.95,1))
# q3<-quantile(t01Oct13$speedkmh,probs=c(0,0.05,0.1,0.25,0.5,0.75,0.9,0.95,1))
# q4<-quantile(t01Oct13$trip_distance_m/t01Oct13$distance,probs=c(0,0.05,0.1,0.25,0.5,0.75,0.9,0.95,1))
# taxi_tripDuration_quantiles<-q1
# taxi_tripDistance_quantiles<-q2
# taxi_speed_quantiles<-q3
# taxi_ratioTripStraightDistance_quantiles<-q4
# ```
# 
# ```{r}
# taxi_tripDuration_quantiles
# taxi_tripDistance_quantiles
# taxi_speed_quantiles
# taxi_ratioTripStraightDistance_quantiles
# ```

cond <- (t01Oct13cb$speedkmh<v3[2]) &
        (t01Oct13cb$trip_distance_m/t01Oct13cb$distance>v4b[1] & t01Oct13cb$trip_distance_m/t01Oct13cb$distance<v4b[2])
sum(!cond)
#[1] 81725

t01Oct13cbpp<-t01Oct13cb[cond,]
file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/t01Oct13cbpp2.rdata"
save("t01Oct13cbpp",file=file)
load(file)

