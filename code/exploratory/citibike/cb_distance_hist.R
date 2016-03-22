
hist(citibikeOct13pp$distance,breaks=seq(0,10400,50),xlim=c(0,4000))
hist(citibikeOct13pp$distance)         
summary(citibikeOct13pp$distance)
max(citibikeOct13pp$distance)

file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/tripdistance_hist.png"
png(file)
hist(citibikeOct13pp$distance,breaks=seq(0,10400,50),xlim=c(0,4000))
dev.off()
# 50 m breaks