hist(citibikeOct13$tripduration,xlim=c(0,7200),breaks=100000)
hist(citibikeOct13$tripduration,xlim=c(0,3600),breaks=seq(0,1259490,30))
hist(citibikeOct13$tripduration,xlim=c(0,3600),breaks=seq(0,1259520,60))
hist(citibikeOct13$tripduration/60,xlim=c(0,60),breaks=seq(0,20992,0.5))
hist(citibikeOct13$tripduration/60,xlim=c(0,60),breaks=seq(0,20992,1),freq=F)
par(mar=c(5.1, 4.1, 4.1, 2.1),mfrow=c(1,1))
# 1min=60s, 30min=1800s, 1h=3600s, 24h=86400

folder<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/"
png(paste0(folder,"tripduration_hist.png"))
#jpeg(paste0(folder,"tripduration_hist.jpeg"))

hist(citibikeOct13$tripduration,xlim=c(0,7200),breaks=100000,main=NULL)
title(main="Histogram of tripduration")
hist(citibikeOct13$tripduration,xlim=c(0,3600),breaks=seq(0,1259520,60),main=NULL)
xlabel('minute')
hist(citibikeOct13$tripduration/60,xlim=c(0,60),breaks=seq(0,20992,1),main=NULL)
dev.off()

 

