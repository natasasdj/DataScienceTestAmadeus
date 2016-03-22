file<-"C:/Users/natasa/Documents/projectAmadeus/data/citibike/raw/citybikeOct13.rdata"
load(file)

names(citibikeOct13)

# Question: speed of bikes?
# we need: 
# tripduration,starttime,stoptime
# start.station.latitude,start.station.longitude              
# end.station.latitude,end.station.longitude


length(citibikeOct13$tripduration)
# 1037712 observations
summary(citibikeOct13$tripduration)

# todo: histogram of trip duration
# todo: histogram of distance
# todo: histogram of speed
# at the beginning and after filtration

sum(citibikeOct13$tripduration>=1800)/length(citibikeOct13$tripduration)

# looking at histogram of trip duration we see that most of the trips last from 5 to 8 minutes, 
# the number of trips that last more than 30 minutes is 5.7%
# we will consider only trips less than 30 minutes
citibikeOct13pp<-citibikeOct13[which(citibikeOct13$tripduration<1800),]
dim(citibikeOct13pp)[1]
# 978162

 
source(t_tripdistance.R)

# histogram of 
# calculate distribution, we see that there are a number of trips with less than 50m, we consider these a mistake
# filter data so that tripdistance at least 200m
citibikeOct13pp<-citibikeOct13pp[which(citibikeOct13pp$distance>200),]
dim(citibikeOct13pp)[1]
# 959356

# filter data so that tripduration > 60s
citibikeOct13pp<-citibikeOct13pp[which(citibikeOct13pp$tripduration>60),]
dim(citibikeOct13pp)[1]
# 959351
str(citibikeOct13pp)
citibikeOct13pp$speed<-citibikeOct13pp$distance/citibikeOct13pp$tripduration
citibikeOct13pp$speedkmh<-citibikeOct13pp$distance/citibikeOct13pp$tripduration*3.6
head(citibikeOct13pp)
# starttime and stoptime into POSIXlt
citibikeOct13pp$starttime<-strptime(citibikeOct13pp$starttime,"%Y-%m-%d %H:%M:%S")
citibikeOct13pp$stoptime<-strptime(citibikeOct13pp$stoptime,"%Y-%m-%d %H:%M:%S")

file<-"C:/Users/natasa/Documents/projectAmadeus/data/citibike/processed/citibikeOct13pp.rdata"
save("citibikeOct13pp",file=file)
str(citibikeOct13pp)

# the time of the day
# distance

# weekday vs. weekend
# business days
t1<-citibikeOct13pp$starttime[1]
unclass(t1)
# datetime$mday        # day of month
# datetime$mon+1     # month of year (zero-indexed)
# daytime$year$+1900  # years since 1900
indb<-citibikeOct13pp$starttime$wday>=1 & citibikeOct13pp$starttime$wday<=5
cbOct13b<-citibikeOct13pp[indb,]
dim(cbOct13b)
#[1] 755988     16

file<-"C:/Users/natasa/Documents/projectAmadeus/data/citibike/processed/cbOct13b.rdata"
save("cbOct13b",file=file)




