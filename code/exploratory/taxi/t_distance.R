file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/taxi10_01.rdata"
load(file)

hist(taxi10_01pp$trip_distance,breaks=seq(0,130000,1000),xlim=c(0,130000))

file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t_distance_hist1.png"
png(file)
hist(taxi10_01pp$trip_distance,breaks=seq(0,130000,1000),xlim=c(0,40000))
dev.off()

file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t_distance_hist2.png"
png(file)
hist(taxi10_01pp$trip_distance,breaks=seq(0,130000,500),xlim=c(0,10000))
dev.off()


file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/taxi/t_distance_hist3.png"
png(file)
hist(taxi10_01pp$trip_distance,breaks=seq(0,130000,250),xlim=c(0,5000))
dev.off()




