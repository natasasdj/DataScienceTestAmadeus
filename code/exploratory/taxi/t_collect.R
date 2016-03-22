
file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/raw/trip_data_10.csv.zip"

# taxiOct13 <- read.table(unz(file, "trip_data_10.csv"),sep=",",header=T,stringsAsFactors=FALSE,nrows=10000000)
# head(taxiOct13); tail(taxiOct13)
# taxiOct13pp<-taxiOct13[order(taxiOct13$pickup_datetime),]
# head(taxiOct13pp$pickup_datetime,100)
# tail(taxiOct13pp$pickup_datetime,100)
# taxiOct13_2 <- read.table(unz(file, "trip_data_10.csv"),sep=",",header=F,stringsAsFactors=FALSE,skip=1*1000000,nrows=1000000)
# head(taxiOct13_2); tail(taxiOct13_2)
# names(taxiOct13)
# names(taxiOct13_2)<-names(taxiOct13)
# taxiOct13_2pp<-taxiOct13_2[order(taxiOct13_2$pickup_datetime),]
# head(taxiOct13_2pp$pickup_datetime,100)
# tail(taxiOct13_2pp$pickup_datetime,100)
# taxiOct13 <- read.table(unz(file, "trip_data_10.csv"),sep=",",header=T,stringsAsFactors=FALSE)

testcon <- unz(file, "trip_data_10.csv")
readsizeof <- 20000
nooflines <- 0
( while((linesread <- length(readLines(testcon,readsizeof))) > 0 ) 
        nooflines <- nooflines+linesread )
close(testcon)
nooflines
#[1] 80820000

# not good idea to put into rmongodb
# library('rmongodb')
# file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/raw/trip_data_10.csv.zip"
# nrows<-1000000
# nread<-0L
# mongo <- mongo.create(host="127.0.0.1", db="taxi")
# if (mongo.get.err(mongo)!=0){ stop("Error in connection to MongoDB") }
# 
# while (nread<nooflines){
# data <- read.table(unz(file, "trip_data_10.csv"),sep=",",header=T,stringsAsFactors=FALSE,
#                  skip=nread,nrows=nrows)
# n<-dim(data)[1]
# for (i in 1:n){
#         doc <-list("_id"=nread+i,
#                 pickup_datetime=data[i,6], dropoff_datetime=data[i,7],
#                 trip_time_in_secs=data[i,9], trip_distance=data[i,10],
#                 pickup_longitude=data[i,11],pickup_latitude=data[i,12],
#                 dropoff_longitude=data[i,13], dropoff_latitude=data[i,14])      
#         mongo.insert(mongo, paste"taxi.Oct13", doc) 
#         
# }
# nread <- nread + nrows
# }
# mongo.destroy(mongo)
#db.Oct13.find({"pickup_datetime":{$gte:'2013-10-01 00:00:00'},"pickup_datetime":{$lte:'2013-10-01 00:00:05'}}).count()



# # read 10,000,000 lines in one chunk 
# # filter for 2013-10-01
# file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/raw/trip_data_10.csv.zip"
# nrows<-10000000L
# nread<-70000000L
# nread<-0L
# nooflines <- 80820000
# cind<-c(6,7,9,10,11,12,13,14)
# taxiOct13day1<-NULL
# con <- unz(file, "trip_data_10.csv")
# taxiOct13 <- read.table(con,sep=",",header=T,stringsAsFactors=FALSE,
#                         skip=nread,nrows=nrows)
# 
# cnames<-names(taxiOct13)
# #taxiOct13
# while (nread<nooflines){
#         
#         rind<-which(taxiOct13[,"pickup_datetime"]>='2013-10-01 00:00:00'& taxiOct13[,"pickup_datetime"]<'2013-10-02 00:00:00')
#         taxiOct13day1<-rbind(taxiOct13day1,taxiOct13[rind,cind])
#         #taxiOct13day1
#         rm("rind")
#         nread <- nread + nrows
#         cat(nread,"\n")
#         rm("taxiOct13")
#         taxiOct13 <- read.table(unz(file, "trip_data_10.csv"),sep=",",header=F,stringsAsFactors=FALSE,
#                   skip=nread,nrows=nrows)
#         names(taxiOct13)<-cnames
# #taxiOct13
# }


# previous approaches did not work

# read and process (filter data) a big file in chunks 
file1<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/raw/trip_data_10.csv"
con1 <- file(file1, open="r")
file2<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/tripdata10_01.csv"
#con2<-file(file2)
colnames<-readLines(con1,1)[[1]]
cat(colnames,file=file2,append=TRUE)
readsizeof <- 50000
#readsizeof <- 100

nooflines <- 0
while ((linesread<-length(datastring <- readLines(con1,n=readsizeof))) > 0 ) {        
        nooflines<-nooflines+linesread
        cat(nooflines," ")
        data<-strsplit(datastring,",")
        ind<-sapply(data,function(x)(x[6]>='2013-10-01 00:00:00' & x[6]<'2013-10-02 00:00:00'))                
        cat(datastring[ind],file=file2,append=TRUE,sep="\n") 
        
}
nooflines
#[1] 15004556
        
        

        nooflines <- nooflines+linesread )
close(con1)
#close(con2)
nooflines

showConnections(all = FALSE)
closeAllConnections()



