file <- "C:/Users/natasa/Documents/projectAmadeus/data/citibike/processed/citibikebOct13pp.rdata"
load(file)

head(citibikeOct13pp)
# we will look only for distances less than 5000m
# ind<-citibikeOct13pp$distance<5000
# citibikeOct13pp2<-citibikeOct13pp[ind,]

# bins for each 100m distance from 200m to 5000m
dist<-100
bin<-list()
i<-1
for (i in 1:47){
        ind<-citibikeOct13pp$distance>=(i+2)*100 &
                citibikeOct13pp$distance<(i+3)*100  
        bin[[i]] <- citibikeOct13pp[ind,]
}

par(mar=c(5.1, 4.1, 4.1, 2.1),mfrow=c(1,1))
for (i in seq(5,45,5)){        
        dir<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/citibike/"
        file<-paste0(dir,"cb_bin",as.integer(i*100),"_duration.png")
        png(file)
        hist(bin[[i]]$tripduration,breaks=25)
        abline(v=median(bin[[i]]$tripduration,),col="red")        
        dev.off()
}
#??? why local maximum after distance 3000

# bins at 9h for each 100m distance from 200m to 5000m
bin09<-list()
for (i in 1:47){
        ind<-cbOct13hour09$distance>=(i+2)*100 &
                cbOct13hour09$distance<(i+3)*100  
        bin09[[i]] <- cbOct13hour09[ind,]
}

for (i in seq(5,45,5)){        
        dir<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/citibike/"
        file<-paste0(dir,"cb_bin",as.integer(i*100),"_h09_duration.png")
        png(file)
        hist(bin09[[i]]$tripduration,breaks=25)
        abline(v=median(bin09[[i]]$tripduration,),col="red")        
        dev.off()
}

# bins at 20h for each 100m distance from 200m to 5000m
bin20<-list()
for (i in 1:47){
        ind<-cbOct13hour20$distance>=(i+2)*100 &
                cbOct13hour20$distance<(i+3)*100  
        bin20[[i]] <- cbOct13hour20[ind,]
}

for (i in seq(5,45,5)){        
        dir<-"C:/Users/natasa/Documents/projectAmadeus/figures/exploratory/citibike/"
        file<-paste0(dir,"cb_bin",as.integer(i*100),"_h20_duration.png")
        png(file)
        hist(bin20[[i]]$tripduration,breaks=25)
        abline(v=median(bin20[[i]]$tripduration,),col="red")        
        dev.off()
}

# TODO: weekday vs. weekend 