attach(taxi10_01pp)
plot(pickup_latitude,pickup_longitude, type='p',pch='.',
     xlim<-c(40.68,40.78),ylim<-c(-74.02,-73.95))

plot(pickup_latitude,pickup_longitude, type='p',pch='.',
     xlim<-c(40.6,40.8),ylim<-c(-74.04,-73.8))



#origpar<-par()
#par(mar=c(5.1, 4.1, 4.1, 2.1),mfrow=c(1,1))

file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t_position1.png"
png(file)
par(mfrow=c(2,1),mar=c(2,2,1,1))
plot(pickup_latitude,pickup_longitude, type='p',pch='.',
     xlim<-c(40.68,40.78),ylim<-c(-74.02,-73.95))
plot(dropoff_latitude,dropoff_longitude, type='p',pch='.',
     xlim<-c(40.68,40.78),ylim<-c(-74.02,-73.95))
dev.off()


file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t_position2.png"
png(file)
par(mfrow=c(2,1),mar=c(2,2,1,1))
plot(pickup_latitude,pickup_longitude, type='p',pch='.',
     xlim<-c(40.5,40.9),ylim<-c(-74.04,-73.8))
plot(dropoff_latitude,dropoff_longitude, type='p',pch='.',
     xlim<-c(40.5,40.9),ylim<-c(-74.04,-73.8))
dev.off()
