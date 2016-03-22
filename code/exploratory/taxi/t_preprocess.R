file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/tripdata10_01.csv"
taxi10_01<-read.csv(file=file,stringsAsFactors=F)
head(taxi10_01)
tail(taxi10_01)
dim(taxi10_01)

# error in a file, the first observation is written in the very first row
# after the names of columns
names(taxi10_01)
taxi10_01<-taxi10_01[,1:14]
names(taxi10_01)[14]<-"dropoff_latitude"

obs<-"740BD5BE61840BE4FE3905CC3EBE3E7E,E48B185060FB0FF49BE6DA43E69E624B,CMT,1,N,2013-10-01 12:44:29,2013-10-01 12:53:26,1,536,1.20,-73.974319,40.741859,-73.99115,40.742424"
l<-dim(taxi10_01)[1]
taxi10_01_v2<-strsplit(obs,",")[[1]]
names(taxi10_01_v2)<-names(taxi10_01)
taxi10_01_v2<-rbind(taxi10_01_v2,taxi10_01)
head(taxi10_01_v2)
tail(taxi10_01_v2)
taxi10_01<-taxi10_01_v2
# corrected data written 
file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/taxi10_01.rdata"
save("taxi10_01",file=file)
file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/tripdata10_01.csv"
write.csv(taxi10_01,file)

# load correct data
file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/taxi10_01.rdata"
load(file)
str(taxi10_01)
# filter only necessary columns
ind<-c(6,7,9:14)
taxi10_01pp<-taxi10_01[,ind]
head(taxi10_01pp)
str(taxi10_01pp)
names(taxi10_01pp)
# make trip distance in m
for(i in 3:8) taxi10_01pp[,i]<-as.numeric(taxi10_01pp[,i])
taxi10_01pp$trip_distance_m<-taxi10_01pp$trip_distance*1609.34
summary(taxi10_01pp)
dim(taxi10_01pp)
#[1] 452769      8

# make speed column (m/s)
taxi10_01pp$speed<-taxi10_01pp$trip_distance_m/taxi10_01pp$trip_time_in_secs
# make speed column (km/h)
taxi10_01pp$speedkmh<-taxi10_01pp$speed*3.6


file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/taxi10_01pp.rdata"
save("taxi10_01pp",file=file)

# consider only those rows with pickup and dropoff within the area of bike stations
cond <- taxi10_01pp$pickup_latitude>=40.68 &
        taxi10_01pp$pickup_latitude<=40.78 &
        taxi10_01pp$pickup_longitude>=-74.02 &
        taxi10_01pp$pickup_longitude<=-73.95 &
        taxi10_01pp$dropoff_latitude>=40.68 &
        taxi10_01pp$dropoff_latitude<=40.78 &
        taxi10_01pp$dropoff_longitude>=-74.02 &
        taxi10_01pp$dropoff_longitude<=-73.95 
        

taxi10_01pp_cb<-taxi10_01pp[cond,]
                              
dim(taxi10_01pp_cb)
#[1] 310058      9

str(taxi10_01pp_cb)
summary(taxi10_01pp_cb)

# remove rows with NA values in trip_time_in_secs
cond<-!is.na(taxi10_01pp_cb$trip_time_in_secs) 
taxi10_01pp_cb<- taxi10_01pp_cb[cond,] 
# consider only entries where trip_distance_m>400
cond<-taxi10_01pp_cb$trip_distance_m>400
taxi10_01pp_cb<- taxi10_01pp_cb[cond,]
dim(taxi10_01pp_cb)
#[1] 306082     11



summary(taxi10_01pp_cb)
# consider only entries where trip_time_in_secs>30
cond<-taxi10_01pp_cb$trip_time_in_secs>30
taxi10_01pp_cb<- taxi10_01pp_cb[cond,]
dim(taxi10_01pp_cb)
#[1] 306166     11
# very large speed: trip_time_in_secs different from 
# (dropoff_datetime - pickup_datetime)-trip_time_in_secs<10
taxi10_01pp_cb$pickup_datetime<-strptime(taxi10_01pp_cb$pickup_datetime,"%Y-%m-%d %H:%M:%S")
taxi10_01pp_cb$dropoff_datetime<-strptime(taxi10_01pp_cb$dropoff_datetime,"%Y-%m-%d %H:%M:%S")
cond <- abs(difftime(taxi10_01pp_cb$dropoff_datetime,taxi10_01pp_cb$pickup_datetime,
        units='secs')-taxi10_01pp_cb$trip_time_in_secs)<10
        
taxi10_01pp_cb<- taxi10_01pp_cb[cond,]
dim(taxi10_01pp_cb)
#[1] 306082     11

# consider only entries where speedkmh<100
cond <- taxi10_01pp_cb$speedkmh<100
taxi10_01pp_cb<- taxi10_01pp_cb[cond,]
dim(taxi10_01pp_cb)
#[1] 306033     11


file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/taxi10_01pp_cb.rdata"
save("taxi10_01pp_cb",file=file)


