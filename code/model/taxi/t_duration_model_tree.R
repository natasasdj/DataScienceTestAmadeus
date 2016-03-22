library(rpart)
library(caret)


file<-"C:/Users/natasa/Documents/projectAmadeus/data/taxi/processed/taxi10_01model.rdata"
load(file)


n<-10000
n<-dim(taxi10_01model)[1]
#ok
taxi10_01modelTrain<-taxi10_01model[1:n,]
xTrain<-as.matrix(taxi10_01modelTrain[,2:7])
yTrain<-taxi10_01modelTrain[,1]
str(taxi10_01modelTrain)

rpartTree<-rpart(trip_time_in_secs ~.,data=taxi10_01modelTrain)

set.seed(100)
ctrl <- trainControl(method = "cv")
rpartTune <- train(x = xTrain, y = yTrain,
                  method = "rpart2",
                  tuneLength = 20,
                  trControl = ctrl)



# CART 
# 
# 306033 samples
# 6 predictor
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# 
# Summary of sample sizes: 275431, 275429, 275432, 275429, 275430, 275429, ... 
# 
# Resampling results across tuning parameters:
#         
#         maxdepth  RMSE      Rsquared   RMSE SD   Rsquared SD
# 1        343.6253  0.3284320  5.579008  0.006074493
# 2        329.3555  0.3830543  5.471601  0.006142399
# 3        315.0067  0.4356478  5.613839  0.006851605
# 5        303.8664  0.4748741  5.565298  0.006861743
# 7        295.5765  0.5031461  5.441965  0.006839290
# 9        286.5527  0.5330384  5.814182  0.008873233
# 10        285.9762  0.5349250  5.834539  0.008439898
# 11        285.7352  0.5357096  6.280421  0.010115242
# 12        285.7352  0.5357096  6.280421  0.010115242
# 13        285.7352  0.5357096  6.280421  0.010115242
# 14        285.7352  0.5357096  6.280421  0.010115242
# 15        285.7352  0.5357096  6.280421  0.010115242
# 17        285.7352  0.5357096  6.280421  0.010115242
# 18        285.7352  0.5357096  6.280421  0.010115242
# 20        285.7352  0.5357096  6.280421  0.010115242
# 21        285.7352  0.5357096  6.280421  0.010115242
# 22        285.7352  0.5357096  6.280421  0.010115242
# 23        285.7352  0.5357096  6.280421  0.010115242
# 24        285.7352  0.5357096  6.280421  0.010115242
# 25        285.7352  0.5357096  6.280421  0.010115242
# 
# RMSE was used to select the optimal model using  the
# smallest value.
# The final value used for the model was maxdepth = 11. 

library(randomForest)
rForest<-randomForest(x = xTrain, y = yTrain, mtry=1, ntree=100)
# not possible