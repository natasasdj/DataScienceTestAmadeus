library(kernlab)

# svmChunked<-function(data,indOutcome,k){       
#         group<-list()
#         svmFitList<-list()
#         # k<-15 # no of chunks
#         d<-dim(data)[[1]] # no of observations
#         no <- floor(d/k) # no of observations per chunk
#         for (i in 1:k){
#                 cat("chunk",i,"\n")
#                 if (i==k) {ind<-((i-1)*no+1):d
#                            
#                 } else {ind<-((i-1)*no+1):(i*no)}             
#                 xtrain<- as.matrix(data[ind,-indOutcome])
#                 outcome<- data[ind,indOutcome]
#                 svmFit<-ksvm(x=xtrain, y=outcome,
#                              kernel="rbfdot",kpar="automatic",C=1,epsilon=0.1)
#                 svmPred <- as.vector(predict(svmFit,newdata=data[ind,2:5]))
#                 svmFitList[[i]] <- svmFit
#                 group[[i]]<-cbind(svmPred,outcome)
#         }
#         return(list(svmFitList=svmFitList,group=group))
#         
#         
# }

library(kernlab)
svmChunked2<-function(data,indOutcome,k){       
        group<-list()
        svmFitList<-list()
        # k<-15 # no of chunks
        d<-dim(data)[[1]] # no of observations
        for (i in 1:k){
                cat("chunk",i,"\n")
                ind <- seq(i,d,k) # sampling into chunks             
                xtrain<- as.matrix(data[ind,-indOutcome])
                outcome<- data[ind,indOutcome]
                svmFit<-ksvm(x=xtrain, y=outcome,
                             kernel="rbfdot",kpar="automatic",C=1,epsilon=0.1)
                #svmPred <- as.vector(predict(svmFit,newdata=data[ind,2:5]))
                svmFitList[[i]] <- svmFit
                #group[[i]]<-cbind(svmPred,outcome)
        }
        return(svmFitList)       
}

# svmCombined <-function(data,indOutcome,svmFitList,sampleInd){
#         xtrain<-NULL
#         d<-dim(data)[[1]]
#         ind<-1:d
#         for (i in 1:k){
#                 cat("chunk",i,"\n")
#                 svmFit <- svmFitList[[i]]
#                 svmPred <- as.vector(predict(svmFit,newdata=data[ind,-indOutcome]))
#                 xtrain<-cbind(xtrain,svmPred)
#         }
#         
#         cat("final svm \n")
#         
#         # sample ind if too many data
#         ind<-sampleInd(d)
#         outcome <- data[ind,indOutcome]
#         svmFitFin<-ksvm(x=xtrain, y=outcome,
#                         kernel="vanilladot",kpar="automatic",C=1,epsilon=0.1)
#         #svmPredFin <- as.vector(predict(svmFitFin,newdata=data[ind,-indOutcome]))
#         return(svmFitFin)
# 
# }

svmCombPred <-function(dataTest,indOutcome,svmFitList){

         d<-dim(dataTest)[[1]]
         k<-length(svmFitList)
         svmPred<-matrix(data=NA,nrow=d,ncol=k+1)
         for (i in 1:k){
                 cat("chunk",i,"\n")
                 svmFit <- svmFitList[[i]]
                 svmPred[,i] <- predict(svmFit,newdata=dataTest[,-indOutcome])

         }
        svmPred[,k+1]<-rowMeans(svmPred[,1:k])
        return(svmPred)
}

svmCombPred2 <-function(dataTest,indOutcome,svmFitList){
        
        d<-dim(dataTest)[[1]]
        k<-length(svmFitList)
        svmPred<-matrix(data=NA,nrow=d,ncol=k+1)
        for (i in 1:k){
                cat("chunk",i,"\n")
                svmFit <- svmFitList[[i]]
                svmPred[,i] <- predict(svmFit,newdata=dataTest[,-indOutcome])
                
        }
        for(j in 1:d){
                svmPred[j,k+1]<-median(svmPred[j,1:k])
        }
        return(svmPred)
}



# number of removed observations: 14398, 6872, 3513
# 4306 because of 10000 trip distance limitation