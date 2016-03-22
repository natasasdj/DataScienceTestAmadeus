# make a model for duration
# pickup time -

file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/taxi10_01pp_cb.rdata"
load(file)
str(taxi10_01pp_cb)
summary(taxi10_01pp_cb)
# this is 2013-10-01, tue => business day

# input:
# pickup_datetime 
#       transform into decimal hour
# pickup_longitude, pickup_latitude, dropoff_longitude, dropoff_latitude
# trip_distance_m 
# trip_time_in_secs 
taxi10_01pp_cb$pickup_hour<-taxi10_01pp_cb$pickup_datetime$hour + 
        taxi10_01pp_cb$pickup_datetime$min/60
head(taxi10_01pp_cb)
taxi10_01model<-taxi10_01pp_cb[,c(3,5:9,12)]
str(taxi10_01model)
#
file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/taxi10_01model.rdata"
save("taxi10_01model",file=file)
file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/taxi10_01model.rdata"
load(file)

library(kernlab)
library(caret)
n<-295033
n<-15000 
#ok
taxi10_01modelTrain<-taxi10_01model[1:n,]
xTrain<-as.matrix(taxi10_01modelTrain[,2:7])
yTrain<-taxi10_01modelTrain[,1]
str(xTrain)
str(yTrain)

# support vector mashines 
# O(n_samples^2 * n_features) for RBF kernel using SMO solver and n_sample * n_features for linear SVMs
svmFit<-ksvm(x=xTrain,y=yTrain,kernel="rbfdot",kpar="automatic",C=1,epsilon=0.1)

taxi10_01modelTest<-taxi10_01model[n+1:length(taxi10_01model),]
xTest<-as.matrix(taxi10_01modelTest[,2:7])
yTest<-taxi10_01modelTest[,1]
svmPredict<-predict(svmFit, xTest)
defaultSummary(data.frame(obs=yTest,pred=svmPredict))

set.seed(100)
ctrl <- trainControl(method = "repeatedcv",repeats=1)
svmRTune <- train(x = xTrain, y = yTrain,
                  method = "svmRadial",
                  preProc = c("center", "scale"),
                  trControl = ctrl)
svmRTune

# Support Vector Machines with Radial Basis Function Kernel 
# 
# 15000 samples
# 6 predictor
# 
# Pre-processing: centered, scaled 
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# 
# Summary of sample sizes: 13499, 13501, 13500, 13501, 13500, 13499, ... 
# 
# Resampling results across tuning parameters:
#         
#         C     RMSE      Rsquared   RMSE SD   Rsquared SD
# 0.25  237.4795  0.6875587  26.12046  0.04108365 
# 0.50  232.9848  0.6979475  25.70281  0.04008417 
# 1.00  229.1965  0.7065676  25.18873  0.03876018 
# 
# Tuning parameter 'sigma' was held constant at a value
# of 0.1865575
# RMSE was used to select the optimal model using  the
# smallest value.
# The final values used for the model were sigma = 0.1865575
# and C = 1. 






