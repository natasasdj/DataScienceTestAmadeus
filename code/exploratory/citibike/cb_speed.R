file <- "C:/Users/natasa/Documents/projectAmadeus/data/citibike/processed/citibikebOct13pp.rdata"
load(file)

plot(citibikeOct13pp$tripduration,citibikeOct13pp$tripdistance)
plot(citibikeOct13pp$tripdistance,citibikeOct13pp$tripduration)
plot(citibikeOct13pp$tripdistance,citibikeOct13pp$tripspeed, type='p',pch='.')
plot(citibikeOct13pp$tripdistance,citibikeOct13pp$tripduration, type='p',pch='.')

file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/cb_distance_duration.png"
png(file)
plot(citibikeOct13pp$tripdistance,citibikeOct13pp$tripduration, type='p',pch='.')
dev.off()

file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/cb_distance_speed.png"
png(file)
plot(citibikeOct13pp$tripdistance,citibikeOct13pp$tripspeed, type='p',pch='.')
dev.off()

file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/cb_distance_speed_zoom2.png"
png(file)
plot(citibikeOct13pp$tripdistance,citibikeOct13pp$tripspeed, type='p',pch='.',xlim=c(500,3000),ylim=c(0,10))
dev.off()

# TODO: median of duration and speed
# devide into bins per 100 m distance and calculate median in these bins

which(citibikeOct13pp$tripspeed==max(citibikeOct13pp$tripspeed))
#53061
citibikeOct13pp[53061,]

names(citibikeOct13pp)
# filter weekdays, 9 and 20 hour
ind<-citibikeOct13pp$starttime$hour==9 & citibikeOct13pp$starttime$wday %in% 1:5
cbOct13hour09<-citibikeOct13pp[ind,]
ind<-citibikeOct13pp$starttime$hour==20 & citibikeOct13pp$starttime$wday %in% 1:5
cbOct13hour20<-citibikeOct13pp[ind,]
dim(cbOct13hour09)
#[1] 62696    18
dim(cbOct13hour20)
#[1] 42954    18

file <- "C:/Users/natasa/Documents/projectAmadeus/data/citibike/processed/cbOct13perHour.rdata"
save("cbOct13hour09","cbOct13hour20",file=file)

file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/citibike/cbOct13hour09_distance_duration1.png"
png(file)
plot(cbOct13hour09$distance,cbOct13hour09$tripduration,type='p',pch='.',
     xlim=c(0,5000),ylim=c(0,1500))
lmt<-lm(cbOct13hour09$tripduration~cbOct13hour09$distance)
abline(lmt,col="blue")
dev.off()

file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/citibike/cbOct13hour09_distance_duration2.png"
png(file)
library(ggplot2)
ggplot(cbOct13hour09, aes(distance,tripduration)) + 
        geom_point(size=0.01) + geom_smooth(size=1.2) + 
        coord_cartesian(xlim = c(0, 5000),ylim=c(0,1500))
dev.off()

file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/citibike/cbOct13hour20_distance_duration1.png"
png(file)
plot(cbOct13hour20$distance,cbOct13hour20$tripduration,type='p',pch='.',
     xlim=c(0,5000),ylim=c(0,2000))
lmt<-lm(cbOct13hour20$tripduration~cbOct13hour20$distance)
abline(lmt,col="blue")
dev.off()

file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/citibike/cbOct13hour20_distance_duration2.png"
png(file)
library(ggplot2)
ggplot(cbOct13hour20, aes(distance,tripduration)) + 
        geom_point(size=0.001) + geom_smooth(size=1.2) + 
        coord_cartesian(xlim = c(0, 5000),ylim=c(0,2000))
dev.off()





