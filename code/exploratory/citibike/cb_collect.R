# data citibike downloaded in data/citibike/raw folder

file<-"C:/Users/natasa/Documents/projectAmadeus/data/citibike/raw/201310-citibike-tripdata.zip"
citibikeOct13 <- read.csv(unz(file, "2013-10 - Citi Bike trip data.csv"),stringsAsFactors=FALSE)

file<-"C:/Users/natasa/Documents/projectAmadeus/data/citibike/raw/citybikeOct13.rdata"
save("citibikeOct13",file=file)
str(citibikeOct13)
#1037712
summary(citibikeOct13)
attach(citibikeOct13)
nlevels(as.factor(start.station.name))
#330
nlevels(as.factor(end.station.name))
#330
se<-paste(start.station.name,end.station.name)
nlevels(as.factor(se))
#75261

load(file)
