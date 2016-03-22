file <- "C:/Users/natasa/Documents/projectAmadeus/data/citibike/processed/cbOct13b.rdata"
load(file)

# split data into weekdays and weekend
# for both weekdays and weekend plot speed vs. time

# look first at weekday - business days
# find the most popular stations during the weekday:Mon to Fri

popularity_stat<-aggregate(cbOct13b[,1],by=cbOct13b[,c(4,5,8,9,16)],FUN=length)
head(popularity_stat)
popularity_stat_sort <- popularity_stat[order(popularity_stat$x,decreasing=TRUE),]
head(popularity_stat_sort)
# start.station.id       start.station.name end.station.id        end.station.name        distance   x
# 318             E 43 St & Vanderbilt Ave        477     W 41 St & 8 Ave                 1077.0908 426
# 294             Washington Square E             382     University Pl & E 14 St         583.8615  398
# 116             W 17 St & 8 Ave                 521     8 Ave & W 31 St                 1116.5912 307
# 519             Pershing Square N               492     W 33 St & 7 Ave                 1132.7646 293
# 432             E 7 St & Avenue A               293     Lafayette St & E 8 St           741.9458  265
# 318             E 43 St & Vanderbilt Ave        524     W 43 St & 6 Ave                 494.3793  262

library(ggplot2)
file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/citibike/cbOct13b_stPopularity_dist.png"
png(file)
ggplot(popularity_stat_sort, aes(distance,x)) + 
        geom_point(size=0.01) + geom_smooth(size=1.2) #+ 
#coord_cartesian(xlim = c(0, 5000),ylim=c(0,2000))
dev.off()

# for station pair 318 and 477 
cond<- cbOct13b$start.station.id==318 &  cbOct13b$end.station.id==477
cb_st318_477<-cbOct13b[cond,]

file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/citibike/st318-477_duration_hour.png"
png(file)
ggplot(cb_st318_477, aes(starttime$hour,tripduration)) + 
        geom_point(size=0.01) + geom_smooth(size=1.2)
dev.off()

file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/citibike/st318-477_duration_hist.png"
png(file)
ggplot(cb_st318_477, aes(x=tripduration)) + geom_histogram() 
dev.off()
file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/citibike/st318-477_duration_hist2.png"
png(file)
hist_cut <- ggplot(cb_st318_477, aes(x=tripduration, fill=as.factor(starttime$hour)))
hist_cut + geom_bar()
dev.off()

m<-median(cb_st318_477$tripduration)
# [1] 428
cut_min<-0.8*m; cut_max <- 1.2*m
file<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/citibike/st318-477_duration_hist3.png"
png(file)
ggplot(cb_st318_477, aes(x=tripduration)) + geom_histogram() +
        geom_vline(aes(xintercept = median(tripduration)), colour="red") +
        geom_vline(xintercept=cut_min,col="blue")+ geom_vline(xintercept=cut_max,col="blue")
dev.off()

# cond_cut <- cb_st318_477$tripduration > cut_min & cb_st318_477$tripduration < cut_max
# cb_st318_477_cut <- cb_st318_477[cond_cut,]


popul_st_less100<-popularity_stat_sort[popularity_stat_sort$x<100,]
str(popul_st_less100)
# 64867 obs
cond<- cbOct13b$start.station.id==146 &  cbOct13b$end.station.id==329
cb_st146_329<-cbOct13b[cond,]
m<-median(cb_st146_329$tripduration)
# [1] 428
cut_min<-0.8*m; cut_max <- 1.2*m
ggplot(cb_st146_329, aes(x=tripduration)) + geom_histogram() +
        geom_vline(aes(xintercept = median(tripduration)), colour="red") +
        geom_vline(xintercept=cut_min,col="blue")+ geom_vline(xintercept=cut_max,col="blue") 
popul_st_less50<-popularity_stat_sort[popularity_stat_sort$x<100,]

# for each station with more than 100 observation perform cutting, and keep this data for a model of tripduration


