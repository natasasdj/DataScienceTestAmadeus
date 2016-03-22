library(ggplot2)

file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/taxi10_01pp_cb.rdata"
load(file=file)

summary(taxi10_01pp_cb$speedkmh)
summary(taxi10_01pp_cb$trip_distance_m)
summary(taxi10_01pp_cb$trip_time_in_secs)

file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t1001_distance_duration1.png"
png(file)
plot(taxi10_01pp_cb$trip_distance_m,taxi10_01pp_cb$trip_time_in_secs,type='p',pch='.',
     xlim=c(0,5000),ylim=c(0,3000))
lmt<-lm(taxi10_01pp_cb$trip_time_in_secs~taxi10_01pp_cb$trip_distance_m)
abline(lmt)
dev.off()

file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t1001_distance_duration2.png"
png(file)

ggplot(taxi10_01pp_cb, aes(trip_distance_m,trip_time_in_secs)) + 
        geom_point(size=0.1) + geom_smooth(size=1) + 
        coord_cartesian(xlim = c(0, 5000),ylim=c(0,3000))
dev.off()

file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t1001_distance_speed1.png"
png(file)
plot(taxi10_01pp_cb$trip_distance_m,taxi10_01pp_cb$speedkmh,type='p',pch='.',
     xlim=c(0,5000),ylim=c(0,40))
lms<-lm(taxi10_01pp_cb$speedkmh~taxi10_01pp_cb$trip_distance_m)
abline(lms,col="blue")
dev.off()

file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t1001_distance_speed2.png"
png(file)
ggplot(taxi10_01pp_cb, aes(trip_distance_m,speedkmh)) + 
        geom_point(size=0.05) + geom_smooth(size=1.2) + 
        coord_cartesian(xlim = c(0, 5000),ylim=c(0,40))
dev.off()

summary(taxi10_01pp_cb$pickup_datetime)
str(taxi10_01pp_cb)

ind<-taxi10_01pp_cb$pickup_datetime$hour==9
t10day01hour09<-taxi10_01pp_cb[ind,]
dim(t10day01hour09)
#[1] 17644    11
ind<-taxi10_01pp_cb$pickup_datetime$hour==20
t10day01hour20<-taxi10_01pp_cb[ind,]
dim(t10day01hour20)
#[1] 20957    11

file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/t10day01perHour.rdata"
save("t10day01hour09","t10day01hour20",file=file)


file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t100109_distance_duration.png"
png(file)
plot(t10day01hour09$trip_distance_m,t10day01hour09$trip_time_in_secs,type='p',pch='.',
     xlim=c(0,5000),ylim=c(0,3000))
lm09t<-lm(t10day01hour09$trip_time_in_secs~t10day01hour09$trip_distance_m)
abline(lm09t)
dev.off()

file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t100109_distance_duration2.png"
png(file)
ggplot(t10day01hour09, aes(trip_distance_m,trip_time_in_secs)) + 
        geom_point(size=0.1) + geom_smooth(size=1) + 
        coord_cartesian(xlim = c(0, 5000),ylim=c(0,3000))
dev.off()

file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t100120_distance_duration.png"
png(file)
plot(t10day01hour20$trip_distance_m,t10day01hour20$trip_time_in_secs,type='p',pch='.',
     xlim=c(0,5000),ylim=c(0,3000))
lm20t<-lm(t10day01hour20$trip_time_in_secs~t10day01hour20$trip_distance_m)
abline(lm20t)
dev.off()

file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t100120_distance_duration2.png"
png(file)
ggplot(t10day01hour20, aes(trip_distance_m,trip_time_in_secs)) + 
        geom_point(size=0.1) + geom_smooth(size=1) + 
        coord_cartesian(xlim = c(0, 5000),ylim=c(0,3000))
dev.off()

file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t100109_distance_speed.png"
png(file)
plot(t10day01hour09$trip_distance_m,t10day01hour09$speedkmh,type='p',pch='.',
     xlim=c(0,5000),ylim=c(0,40))
lm09s<-lm(t10day01hour09$speedkmh~t10day01hour09$trip_distance_m)
abline(lm09s)
dev.off()

file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t100120_distance_speed.png"
png(file)
plot(t10day01hour20$trip_distance_m,t10day01hour20$speedkmh,type='p',pch='.',
     xlim=c(0,5000),ylim=c(0,40))
lm20s<-lm(t10day01hour20$speedkmh~t10day01hour20$trip_distance_m)
abline(lm20s)
dev.off()





