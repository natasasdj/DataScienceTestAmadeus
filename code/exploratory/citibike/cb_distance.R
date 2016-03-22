install.packages('geosphere')
library('geosphere')
attach(citibikeOct13pp)
# this is the distance between start and end station in m
# distance<-distGeo(cbind(citibikeOct13pp$start.station.latitude,citibikeOct13pp$start.station.longitude),
#         cbind(citibikeOct13pp$end.station.latitude,citibikeOct13pp$start.station.longitude))
# !!!!! this does not give correct results
# !!!!! use gdist in Imap (see bellow) 

folder<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/"
png(paste0(folder,"station_positions.png"))
par(mfrow=c(2,1),mar=c(2,2,1,1))
plot(start.station.latitude,start.station.longitude)
plot(end.station.latitude,end.station.longitude)
dev.off()


library("Imap")
d<-dim(citibikeOct13)[1]
dist<-rep(0,d)
for (i in 1:d){
        cat("itearation ",i," ")
        dist[i]<-gdist(citibikeOct13$start.station.longitude[i],citibikeOct13$start.station.latitude[i],
                       citibikeOct13$end.station.longitude[i],citibikeOct13$end.station.latitude[i],
                       units="m")
}
cbOct13<-citibikeOct13
cbOct13$distance<-dist
cbOct13$speedkmh<-cbOct13$distance/cbOct13$tripduration*3.6
file<-"C:/Users/natasa/Documents/projectAmadeus/data/citibike/processed/cbOct13.rdata"
save("cbOct13",file=file)
hist(distance,breaks=seq(0,2800,10))
summary(distance)
max(distance)
par=figpar


# get driving distance through google maps API
# library(XML)
# library(RCurl)
# drivingDistance <- function(start,end){
#         origin <- paste0(as.character(start[1]),",",as.character(start[2]))
#         destination <- paste0(as.character(end[1]),",",as.character(end[2]))
#         xml.url <- paste0('http://maps.googleapis.com/maps/api/distancematrix/xml?origins=',origin,'&destinations=',destination,'&mode=driving&sensor=false')
#         xmlfile <- xmlParse(getURL(xml.url))
#         dist <- xmlValue(xmlChildren(xpathApply(xmlfile,"//distance")[[1]])$value)
#         distance <- as.numeric(sub(" km","",dist))
#         return(distance)
# }


          

# this is the shortest distance between start and end station in m
install.packages("Imap")
library("Imap") 

l<-length(citibikeOct13pp$start.station.longitude)
dist<-list()
for(k in 1:10){dist[[k]]<-numeric(0)}


int<-100000
for(j in 1:9){
for(i in ((j-1)*int+1):(j*int)){
        if (i%%10000==0) cat(i," ")
        d <- gdist(citibikeOct13pp$start.station.longitude[i],citibikeOct13pp$start.station.latitude[i],
                citibikeOct13pp$end.station.longitude[i],citibikeOct13pp$end.station.latitude[i],
                units="m")
        dist[[j]]<-c(dist[[j]],d)
        #citibikeOct13pp$distance[i] <- d
        #dist[i]<-d
        
}
}
j<-10
for(i in 900001:l){
        if (i%%10000==0) cat(i," ")
        d <- gdist(citibikeOct13pp$start.station.longitude[i],citibikeOct13pp$start.station.latitude[i],
                   citibikeOct13pp$end.station.longitude[i],citibikeOct13pp$end.station.latitude[i],
                   units="m")
        dist[[j]]<-c(dist[[j]],d)
   
        
}
distance<-NULL
for (k in 1:10) distance<-c(distance,dist[[k]])
citibikeOct13pp$distance<-distance
rm()


# dist<-NULL
# for (i in 1:l){
#         start<-c(citibikeOct13pp$start.station.latitude[i],citibikeOct13pp$start.station.longitude[i])
#         end<-c(citibikeOct13pp$end.station.latitude[i],citibikeOct13pp$end.station.longitude[i])
#         d<-driving_distance(start,end)
#         dist <- c(dist,d)  
# }
# citibikeOct13pp$driving_distance<-dist   



file<-"C:/Users/natasa/Documents/projectAmadeus/data/citibike/processed/citibikeOct13pp.rdata"
save("citibikeOct13pp",file=file)
          
rm(list=ls())
load(file)

          
          
          
          
          
          
          
          
          
          