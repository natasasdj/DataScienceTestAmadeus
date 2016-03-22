# popular station pairs

file <- "C:/Users/natasa/Documents/projectAmadeus/data/citibike/processed/cbOct13perHour.rdata"
load(file)

names(cbOct13hour09)

# the most popular station pair at hour 09
popularity_stat<-aggregate(cbOct13hour09[,1],by=cbOct13hour09[,c(4,5,8,9,16)],FUN=length)
head(popularity_stat)
popularity_stat_sort <- popularity_stat[order(popularity_stat$x,decreasing=TRUE),]
head(popularity_stat_sort)
# stations 417 and 415
ind<-cbOct13hour09$start.station.id==417 &
        cbOct13hour09$end.station.id==415
st417_415_h09<-cbOct13hour09[ind,]

ind<-cbOct13hour20$start.station.id==417 &
        cbOct13hour20$end.station.id==415
st417_415_h20<-cbOct13hour20[ind,]

ind <-  citibikeOct13pp$start.station.id==417 &
        citibikeOct13pp$end.station.id==415
st417_415<-citibikeOct13pp[ind,]

head(st1)
head(st2)

# histogram of trip durations between stations 417 and 415
file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/citibike/cb_st417_415_duration_hist.png"
png(file)
par(mfrow=c(3,1),mar=c(2,2,1,1))
hist(st417_415_h09$tripduration,breaks=30)
abline(v=median(st417_415_h09$tripduration),col="red")
hist(st417_415_h20$tripduration,breaks=30)
abline(v=median(st417_415_h20$tripduration),col="red")
hist(st417_415$tripduration,breaks=30)
abline(v=median(st417_415$tripduration),col="red")
dev.off()

n=20
file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/citibike/cb_st417_415_duration_hist2.png"
png(file)
par(mfrow=c(3,1),mar=c(2,2,1,1))
hist(st417_415_h09$tripduration,breaks=n)
abline(v=median(st417_415_h09$tripduration),col="red")
hist(st417_415_h20$tripduration,breaks=n)
abline(v=median(st417_415_h20$tripduration),col="red")
hist(st417_415$tripduration,breaks=n)
abline(v=median(st417_415$tripduration),col="red")
dev.off()





